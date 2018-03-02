;;; org-clubhouse.el --- Simple, unopinionated integration between org-mode and Clubhouse

;;; Copyright (C) 2018 Off Market Data, Inc. DBA Urbint
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to
;;; deal in the Software without restriction, including without limitation the
;;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;;; IN THE SOFTWARE.

;;; Commentary:
;;; org-clubhouse provides simple, unopinionated integration between Emacs's
;;; org-mode and the Clubhouse issue tracker
;;;
;;; To configure org-clubhouse, create an authorization token in Cluhbouse's
;;; settings, then place the following configuration somewhere private:
;;;
;;;   (setq org-clubhouse-auth-token "<auth_token>"
;;;         org-clubhouse-team-name  "<team-name>")
;;;

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 's)
(require 'org)
(require 'org-element)
(require 'subr-x)
(require 'json)

;;;
;;; Configuration
;;;

(defvar org-clubhouse-auth-token nil
  "Authorization token for the Clubhouse API")

(defvar org-clubhouse-team-name nil
  "Team name to use in links to Clubhouse
ie https://app.clubhouse.io/<TEAM_NAME>/stories")

(defvar org-clubhouse-project-ids nil
  "Specific list of project IDs to synchronize with clubhouse.
If unset all projects will be synchronized")

(defvar org-clubhouse-workflow-name "Default")

(defvar org-clubhouse-state-alist
  '(("LATER"  . "Unscheduled")
    ("[ ]"    . "Ready for Development")
    ("TODO"   . "Ready for Development")
    ("OPEN"   . "Ready for Development")
    ("ACTIVE" . "In Development")
    ("PR"     . "Review")
    ("DONE"   . "Merged")
    ("[X]"    . "Merged")
    ("CLOSED" . "Merged")))

;;;
;;; Utilities
;;;

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

(defun ->list (vec) (append vec nil))

(defun reject-archived (item-list)
  (-filter (lambda (item) (equal :json-false (alist-get 'archived item))) item-list))

(defun alist->plist (key-map alist)
  (->> key-map
       (-map (lambda (key-pair)
               (let ((alist-key (car key-pair))
                     (plist-key (cdr key-pair)))
                 (list plist-key (alist-get alist-key alist)))))
       (-flatten-n 1)))

(defun alist-get-equal (key alist)
  "Like `alist-get', but uses `equal' instead of `eq' for comparing keys"
  (->> alist
       (-find (lambda (pair) (equal key (car pair))))
       (cdr)))

(comment

 (alist->plist
  '((foo . :foo)
    (bar . :something))

  '((foo . "foo") (bar . "bar") (ignored . "ignoreme!")))
 ;; => (:foo "foo" :something "bar")

 )

;;;
;;; Org-element interaction
;;;

;; (defun org-element-find-headline ()
;;   (let ((current-elt (org-element-at-point)))
;;     (if (equal 'headline (car current-elt))
;;         current-elt
;;       (let* ((elt-attrs (cadr current-elt))
;;              (parent (plist-get elt-attrs :post-affiliated)))
;;         (goto-char parent)
;;         (org-element-find-headline)))))

(defun org-element-find-headline ()
  (let ((current-elt (org-element-at-point)))
    (when (equal 'headline (car current-elt))
      (cadr current-elt))))

(defun org-element-extract-clubhouse-id (elt)
  (when-let ((clubhouse-id-link (plist-get elt :CLUBHOUSE-ID)))
    (cond
     ((string-match
       (rx "[[" (one-or-more anything) "]"
           "[" (group (one-or-more digit)) "]]")
       clubhouse-id-link)
      (string-to-int (match-string 1 clubhouse-id-link)))
     ((string-match-p
       (rx buffer-start
           (one-or-more digit)
           buffer-end)
       clubhouse-id-link)
      (string-to-int clubhouse-id-link)))))

(comment
 (let ((strn "[[https://app.clubhouse.io/example/story/2330][2330]]"))
   (string-match
    (rx "[[" (one-or-more anything) "]"
        "[" (group (one-or-more digit)) "]]")
    strn)
   (string-to-int (match-string 1 strn)))

 )

(defun org-element-clubhouse-id ()
  (org-element-extract-clubhouse-id
   (org-element-find-headline)))

;;;
;;; API integration
;;;

(defvar org-clubhouse-base-url* "https://api.clubhouse.io/api/v2")

(defun org-clubhouse-auth-url (url)
  (concat url
          "?"
          (url-build-query-string
           `(("token" ,org-clubhouse-auth-token)))))

(defun org-clubhouse-baseify-url (url)
  (if (s-starts-with? org-clubhouse-base-url* url) url
    (concat org-clubhouse-base-url*
            (if (s-starts-with? "/" url) url
              (concat "/" url)))))

(defun org-clubhouse-request (method url &optional data)
  (message "%s %s %s" method url (prin1-to-string data))
  (let* ((url-request-method method)
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data data)
         (buf))

    (setq url (-> url
                  org-clubhouse-baseify-url
                  org-clubhouse-auth-url))

    (setq buf (url-retrieve-synchronously url))

    (with-current-buffer buf
      (goto-char url-http-end-of-headers)
      (prog1 (json-read) (kill-buffer)))))

(cl-defun to-id-name-pairs
    (seq &optional (id-attr 'id) (name-attr 'name))
  (->> seq
       ->list
       (-map (lambda (resource)
          (cons (alist-get id-attr   resource)
                (alist-get name-attr resource))))))

(cl-defun org-clubhouse-fetch-as-id-name-pairs
    (resource &optional
              (id-attr 'id)
              (name-attr 'name))
  "Returns the given resource from clubhouse as (id . name) pairs"
  (let ((resp-json (org-clubhouse-request "GET" resource)))
    (-> resp-json
        ->list
        reject-archived
        (to-id-name-pairs id-attr name-attr))))

(defun org-clubhouse-link-to-story (story-id)
  (format "https://app.clubhouse.io/%s/story/%d"
          org-clubhouse-team-name
          story-id))

(defun org-clubhouse-link-to-epic (epic-id)
  (format "https://app.clubhouse.io/%s/epic/%d"
          org-clubhouse-team-name
          epic-id))

(defun org-clubhouse-link-to-project (project-id)
  (format "https://app.clubhouse.io/%s/project/%d"
          org-clubhouse-team-name
          project-id))

;;;
;;; Caching
;;;

(comment
 (defcache org-clubhouse-projects
   (org-sync-clubhouse-fetch-as-id-name-pairs "projectx"))

 (clear-org-clubhouse-projects-cache)
 (clear-org-clubhouse-cache)
 )

(defvar org-clubhouse-cache-clear-functions ())

(defmacro defcache (name &optional docstring &rest body)
  (let* ((doc (when docstring (list docstring)))
         (cache-var-name (intern (concat (symbol-name name)
                                         "-cache")))
         (clear-cache-function-name
          (intern (concat "clear-" (symbol-name cache-var-name)))))
    `(progn
       (defvar ,cache-var-name :no-cache)
       (defun ,name ()
         ,@doc
         (when (equal :no-cache ,cache-var-name)
           (setq ,cache-var-name (progn ,@body)))
         ,cache-var-name)
       (defun ,clear-cache-function-name ()
         (interactive)
         (setq ,cache-var-name :no-cache))

       (push (quote ,clear-cache-function-name)
             org-clubhouse-cache-clear-functions))))

(defun org-clubhouse-clear-cache ()
  (interactive)
  (-map #'funcall org-clubhouse-cache-clear-functions))

;;;
;;; API resource functions
;;;

(defcache org-clubhouse-projects
  "Returns projects as (project-id . name)"
  (org-clubhouse-fetch-as-id-name-pairs "projects"))

(defcache org-clubhouse-epics
  "Returns projects as (project-id . name)"
  (org-clubhouse-fetch-as-id-name-pairs "epics"))

(defcache org-clubhouse-workflow-states
  "Returns worflow states as (name . id) pairs"
  (let* ((resp-json (org-clubhouse-request "GET" "workflows"))
         (workflows (->list resp-json))
         ;; just assume it exists, for now
         (workflow  (-find (lambda (workflow)
                             (equal org-clubhouse-workflow-name
                                    (alist-get 'name workflow)))
                           workflows))
         (states    (->list (alist-get 'states workflow))))
    (to-id-name-pairs states
                      'name
                      'id)))

(defun org-clubhouse-stories-in-project (project-id)
  "Returns the stories in the given project as org bugs"
  (let ((resp-json (org-clubhouse-request "GET" (format "/projects/%d/stories" project-id))))
    (->> resp-json ->list reject-archived
         (-reject (lambda (story) (equal :json-true (alist-get 'completed story))))
         (-map (lambda (story)
                 (cons
                  (cons 'status
                        (cond
                         ((equal :json-true (alist-get 'started story))
                          'started)
                         ((equal :json-true (alist-get 'completed story))
                          'completed)
                         ('t
                          'open)))
                  story)))
         (-map (-partial #'alist->plist
                         '((name . :title)
                           (id . :id)
                           (status . :status)))))))

;;;
;;; Story creation
;;;

(cl-defun org-clubhouse-create-story-internal
    (title &key project-id epic-id)
  (assert (and (stringp title)
               (integerp project-id)
               (or (null epic-id) (integerp epic-id))))
  (org-clubhouse-request
   "POST"
   "stories"
   (json-encode
    `((name . ,title)
      (project_id . ,project-id)
      (epic_id . ,epic-id)))))

(defun org-clubhouse-prompt-for-project (cb)
  (ivy-read
   "Select a project: "
   (-map #'cdr (org-clubhouse-projects))
   :require-match t
   :history 'org-clubhouse-project-history
   :action (lambda (selected)
             (let ((project-id
                    (->> (org-clubhouse-projects)
                         (-find (lambda (proj)
                                    (string-equal (cdr proj) selected)))
                         car)))
               (message "%d" project-id)
               (funcall cb project-id)))))

(defun org-clubhouse-prompt-for-epic (cb)
  (ivy-read
   "Select an epic: "
   (-map #'cdr (org-clubhouse-epics))
   :history 'org-clubhouse-epic-history
   :action (lambda (selected)
             (let ((epic-id
                    (->> (org-clubhouse-epics)
                         (-find (lambda (proj)
                                    (string-equal (cdr proj) selected)))
                         car)))
               (message "%d" epic-id)
               (funcall cb epic-id)))))

(defun org-clubhouse-populate-created-story (story)
  (let ((elt        (org-element-find-headline))
        (story-id   (alist-get 'id story))
        (epic-id    (alist-get 'epic_id story))
        (project-id (alist-get 'project_id story)))

    (org-set-property "clubhouse-id"
                      (org-make-link-string
                       (org-clubhouse-link-to-story story-id)
                       (number-to-string story-id)))

    (org-set-property "clubhouse-epic"
                      (org-make-link-string
                       (org-clubhouse-link-to-epic epic-id)
                       (alist-get epic-id (org-clubhouse-epics))))

    (org-set-property "clubhouse-project"
                      (org-make-link-string
                       (org-clubhouse-link-to-project project-id)
                       (alist-get project-id (org-clubhouse-projects))))

    (org-todo "TODO")))

(defun org-clubhouse-create-story ()
  (interactive)
  ;; (message (org-element-find-headline))
  (when-let ((elt (org-element-find-headline))
             (title (plist-get elt :title)))
    (if (plist-get elt :CLUBHOUSE-ID)
        (message "This headline is already a clubhouse story!")
      (org-clubhouse-prompt-for-project
       (lambda (project-id)
         (when project-id
           (org-clubhouse-prompt-for-epic
            (lambda (epic-id)
              (let* ((story (org-clubhouse-create-story-internal
                             title
                             :project-id project-id
                             :epic-id epic-id)))
                (org-clubhouse-populate-created-story story))))))))))

;;;
;;; Story updates
;;;

(cl-defun org-clubhouse-update-story-internal
    (story-id &rest attrs)
  (assert (and (integerp story-id)
               (listp attrs)))
  (org-clubhouse-request
   "PUT"
   (format "stories/%d" story-id)
   (json-encode attrs)))

(defun org-clubhouse-update-status ()
  (when-let (clubhouse-id (org-element-clubhouse-id))
    (let* ((elt (org-element-find-headline))
           (todo-keyword (-> elt (plist-get :todo-keyword) (substring-no-properties))))
      (message todo-keyword)
      (when-let ((clubhouse-workflow-state
                  (alist-get-equal todo-keyword org-clubhouse-state-alist))
                 (workflow-state-id
                  (alist-get-equal clubhouse-workflow-state (org-clubhouse-workflow-states))))
        (org-clubhouse-update-story-internal
         clubhouse-id
         :workflow_state_id workflow-state-id)
        (message "Successfully updated clubhouse status to \"%s\""
                 clubhouse-workflow-state)))))

(define-minor-mode org-clubhouse-mode
  :init-value nil
  :group 'org
  :lighter "Org-Clubhouse"
  :keymap '()
  (add-hook 'org-after-todo-state-change-hook
            'org-clubhouse-update-status
            nil
            t))


(provide 'org-clubhouse)
;;; org-clubhouse.el ends here
