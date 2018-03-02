;;; org-clubhouse.el --- Simple, unopinionated integration between org-mode and Clubhouse

;;; Copyright (C) 2018 Off Market Data, Inc. DBA Urbint

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


(defun org-clubhouse-collect-headlines (beg end)
  "Collects the headline at point or the headlines in a region. Returns a list."
  (setq test-headlines
  (if (and beg end)
      (get-headlines-in-region beg end)
      (list (org-element-find-headline)))))


(defun org-clubhouse-get-headlines-in-region (beg end)
  "Collects the headlines from BEG to END"
  (save-excursion
    ;; This beg/end clean up pulled from `reverse-region`.
    ;; it expands the region to include the full lines from the selected region.

    ;; put beg at the start of a line and end and the end of one --
    ;; the largest possible region which fits this criteria
    (goto-char beg)
    (or (bolp) (forward-line 1))
    (setq beg (point))
    (goto-char end)
    ;; the test for bolp is for those times when end is on an empty line;
    ;; it is probably not the case that the line should be included in the
    ;; reversal; it isn't difficult to add it afterward.
    (or (and (eolp) (not (bolp))) (progn (forward-line -1) (end-of-line)))
    (setq end (point-marker))

    ;; move to the beginning
    (goto-char beg)
    ;; walk by line until past end
    (let ((headlines '())
          (before-end 't))
      (while before-end
        (add-to-list 'headlines (org-element-find-headline))
        (let ((before (point)))
          (org-forward-heading-same-level 1)
          (setq before-end (and (not (eq before (point))) (< (point) end)))))
      headlines)))

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

(defun org-clubhouse-populate-created-story (elt story)
  (let ((elt-start  (plist-get elt :begin))
        (story-id   (alist-get 'id story))
        (epic-id    (alist-get 'epic_id story))
        (project-id (alist-get 'project_id story)))

    (save-excursion
      (goto-char elt-start)

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

      (org-todo "TODO"))))

(defun org-clubhouse-create-story (&optional beg end)
  "Creates a clubhouse story using selected headlines.

Will pull the title from the headline at point,
or create cards for all the headlines in the selected region.

All stories are added to the same project and epic, as selected via two prompts.
If the stories already have a CLUBHOUSE-ID, they are filtered and ignored."
  (interactive
    (if (use-region-p)
      (list (region-beginning) (region-end))))

  (let* ((elts     (org-clubhouse-collect-headlines beg end))
         (new-elts (-remove (lambda (elt) (plist-get elt :CLUBHOUSE-ID)) elts)))
    (org-clubhouse-prompt-for-project
     (lambda (project-id)
       (when project-id
         (org-clubhouse-prompt-for-epic
           (lambda (epic-id)
             (-map (lambda (elt)
               (let* ((title (plist-get elt :title))
                      (story (org-clubhouse-create-story-internal
                            title
                            :project-id project-id
                            :epic-id epic-id)))
               (org-clubhouse-populate-created-story elt story))) new-elts))))))))

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
