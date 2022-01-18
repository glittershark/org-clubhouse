;;; org-shortcut.el --- Simple, unopinionated integration between org-mode and
;;; Shortcut

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
;;; Package-Requires: ((emacs "26.1") dash s ivy json (org "9.3"))
;;; Package-Version: 0.0.1
;;; URL: https://github.com/glitterlshark/org-shortcut

;;; Commentary:
;;; org-shortcut provides simple, unopinionated integration between Emacs's
;;; org-mode and the Shortcut issue tracker
;;;
;;; To configure org-shortcut, create an authorization token in Cluhbouse's
;;; settings, then place the following configuration somewhere private:
;;;
;;;   (setq org-shortcut-auth-token "<auth_token>"
;;;         org-shortcut-team-name  "<team-name>")
;;;

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'org)
(require 'org-element)
(require 'subr-x)
(require 'ivy)
(require 'json)

;;;
;;; Configuration
;;;

(defvar org-shortcut-auth-token nil
  "Authorization token for the Shortcut API.")

(defvar org-shortcut-username nil
  "Username for the current Shortcut user.

Unfortunately, the Shortcut API doesn't seem to provide this via the API given
an API token, so we need to configure this for
`org-shortcut-claim-story-on-status-updates' to work")

(defvar org-shortcut-team-name nil
  "Team name to use in links to Shortcut.
ie https://app.shortcut.io/<TEAM_NAME>/stories")

(defvar org-shortcut-project-ids nil
  "Specific list of project IDs to synchronize with shortcut.
If unset all projects will be synchronized")

(defvar org-shortcut-workflow-name "Default")

(defvar org-shortcut-default-story-type nil
  "Sets the default story type. If set to 'nil', it will interactively prompt
the user each and every time a new story is created. If set to 'feature',
'bug', or 'chore', that value will be used as the default and the user will
not be prompted")

(defvar org-shortcut-state-alist
  '(("LATER"  . "Unscheduled")
    ("[ ]"    . "Ready for Development")
    ("TODO"   . "Ready for Development")
    ("OPEN"   . "Ready for Development")
    ("ACTIVE" . "In Development")
    ("PR"     . "Review")
    ("DONE"   . "Merged")
    ("[X]"    . "Merged")
    ("CLOSED" . "Merged"))
  "Alist mapping org-mode todo keywords to their corresponding states in
  Shortcut. In `org-shortcut-mode', moving headlines to these todo keywords
  will update to the corresponding status in Shortcut")

(defvar org-shortcut-story-types
  '(("feature" . "Feature")
    ("bug"     . "Bug")
    ("chore"   . "Chore")))

(defvar org-shortcut-default-story-types
  '(("feature" . "Feature")
    ("bug"     . "Bug")
    ("chore"   . "Chore")
    ("prompt"  . "**Prompt each time (do not set a default story type)**")))

(defvar org-shortcut-default-state "Proposed"
  "Default state to create all new stories in.")

(defvar org-shortcut-claim-story-on-status-update 't
  "Controls the assignee behavior of stories on status update.

If set to 't, will mark the current user as the owner of any shortcut
stories on any update to the status.

If set to nil, will never automatically update the assignee of shortcut
stories.

If set to a list of todo-state's, will mark the current user as the owner of
shortcut stories whenever updating the status to one of those todo states.")

(defvar org-shortcut-create-stories-with-labels nil
  "Controls the way org-shortcut creates stories with labels based on org tags.

If set to 't, will create labels for all org tags on headlines when stories are
created.

If set to 'existing, will set labels on created stories only if the label
already exists in shortcut

If set to nil, will never create stories with labels")

;;;
;;; Utilities
;;;

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

(defun ->list (vec) (append vec nil))

(defun reject-archived (item-list)
  (-reject (lambda (item) (equal :json-true (alist-get 'archived item))) item-list))

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

(defun invert-alist (alist)
  "Invert the keys and values of ALIST."
  (-map (lambda (cell) (cons (cdr cell) (car cell))) alist))

(comment

 (alist->plist
  '((foo . :foo)
    (bar . :something))

  '((foo . "foo") (bar . "bar") (ignored . "ignoreme!")))
 ;; => (:foo "foo" :something "bar")
 )

(defun find-match-in-alist (target alist)
  (->> alist
       (-find (lambda (key-value)
                   (string-equal (cdr key-value) target)))
       car))

(defun org-shortcut-collect-headlines (beg end)
  "Collects the headline at point or the headlines in a region. Returns a list."
  (if (and beg end)
      (org-shortcut-get-headlines-in-region beg end)
    (list (org-element-find-headline))))


(defun org-shortcut-get-headlines-in-region (beg end)
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
      (reverse headlines))))

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
  (save-mark-and-excursion
    (when (not (outline-on-heading-p)) (org-back-to-heading))
    (let ((current-elt (org-element-at-point)))
      (when (equal 'headline (car current-elt))
        (cadr current-elt)))))

(defun org-element-extract-shortcut-id (elt &optional property)
  (when-let* ((shortcut-id-link (plist-get elt (or property :SHORTCUT-ID))))
    (cond
     ((string-match
       (rx "[[" (one-or-more anything) "]"
           "[" (group (one-or-more digit)) "]]")
       shortcut-id-link)
      (string-to-number (match-string 1 shortcut-id-link)))
     ((string-match
       (rx "[[https://app.shortcut.io/"
           (one-or-more anything)
           "/story/" (group (one-or-more digit)))
       shortcut-id-link)
      (string-to-number (match-string 1 shortcut-id-link)))
     ((string-match-p
       (rx buffer-start
           (one-or-more digit)
           buffer-end)
       shortcut-id-link)
      (string-to-number shortcut-id-link)))))

(comment
 (let ((strn "[[https://app.shortcut.io/example/story/2330][2330]]"))
   (string-match
    (rx "[[" (one-or-more anything) "]"
        "[" (group (one-or-more digit)) "]]")
    strn)
   (string-to-number (match-string 1 strn))))

(defun org-element-shortcut-id (&optional property)
  (org-element-extract-shortcut-id
   (org-element-find-headline)
   property))

(defun org-shortcut--element-type (elt)
  "Return one of 'epic, 'story, or nil indicating the type of ELT."
  (cond
   ((plist-get elt :SHORTCUT-EPIC-ID) 'epic)
   ((plist-get elt :SHORTCUT-ID) 'story)))

(defun org-shortcut-clocked-in-story-id ()
  "Return the shortcut story-id of the currently clocked-in org entry, if any."
  (save-mark-and-excursion
    (save-current-buffer
      (when (org-clocking-p)
        (set-buffer (marker-buffer org-clock-marker))
        (save-restriction
          (when (or (< org-clock-marker (point-min))
                    (> org-clock-marker (point-max)))
            (widen))
          (goto-char org-clock-marker)
          (org-element-shortcut-id))))))

(comment
 (org-shortcut-clocked-in-story-id)
 ;;
 )

(defun org-element-and-children-at-point ()
  (let* ((elt (org-element-find-headline))
         (contents-begin (or (plist-get elt :contents-begin)
                             (plist-get elt :begin)))
         (end   (plist-get elt :end))
         (level (plist-get elt :level))
         (children '()))
    (save-excursion
      (goto-char (+ contents-begin (length (plist-get elt :title))))
      (while (< (point) end)
        (let* ((next-elt (org-element-at-point))
               (elt-type (car next-elt))
               (elt      (cadr next-elt)))
          (when (and (eql 'headline elt-type)
                     (eql (+ 1 level) (plist-get elt :level)))
            (push elt children))
          (goto-char (plist-get elt :end)))))
    (append elt `(:children ,(reverse children)))))

(defun +org-element-contents (elt)
  (if-let ((begin (plist-get (cadr elt) :contents-begin))
           (end (plist-get (cadr elt) :contents-end)))
      (buffer-substring-no-properties begin end)
    ""))

(defun org-shortcut-find-description-drawer ()
  "Try to find a DESCRIPTION drawer in the current element."
  (let ((elt (org-element-at-point)))
    (cl-case (car elt)
      ('drawer (+org-element-contents elt))
      ('headline
       (when-let ((drawer-pos (string-match
                               ":DESCRIPTION:"
                               (+org-element-contents elt))))
         (save-excursion
           (goto-char (+ (plist-get (cadr elt) :contents-begin)
                         drawer-pos))
           (org-shortcut-find-description-drawer)))))))

(defun org-shortcut--description-for-elt (elt)
  (save-mark-and-excursion
    (goto-char (plist-get elt :begin))
    (org-shortcut-find-description-drawer)))

(defun org-shortcut--labels-for-elt (elt)
  "Return the Shortcut labels based on the tags of ELT and the user's config."
  (unless (eq nil org-shortcut-create-stories-with-labels)
    (let ((tags (org-get-tags (plist-get elt :contents-begin))))
      (-map (lambda (l) `((name . ,l)))
            (cl-case org-shortcut-create-stories-with-labels
              ('t tags)
              ('existing (-filter (lambda (tag) (-some (lambda (l)
                                                    (string-equal tag (cdr l)))
                                                  (org-shortcut-labels)))
                                  tags)))))))

;;;
;;; API integration
;;;

(defvar org-shortcut-base-url* "https://api.shortcut.io/api/v3")

(defun org-shortcut-auth-url (url &optional params)
 (concat url
         "?"
         (url-build-query-string
          (cons `("token" ,org-shortcut-auth-token) params))))

(defun org-shortcut-baseify-url (url)
 (if (s-starts-with? org-shortcut-base-url* url) url
   (concat org-shortcut-base-url*
           (if (s-starts-with? "/" url) url
             (concat "/" url)))))

(cl-defun org-shortcut-request (method url &key data (params '()))
 (message "%s %s %s" method url (prin1-to-string data))
 (let* ((url-request-method method)
        (url-request-extra-headers
         '(("Content-Type" . "application/json")))
        (url-request-data data)
        (buf))

   (setq url (-> url
                 org-shortcut-baseify-url
                 (org-shortcut-auth-url params)))

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

(cl-defun org-shortcut-fetch-as-id-name-pairs
    (resource &optional
              (id-attr 'id)
              (name-attr 'name))
  "Returns the given resource from shortcut as (id . name) pairs"
  (let ((resp-json (org-shortcut-request "GET" resource)))
    (-> resp-json
        ->list
        reject-archived
        (to-id-name-pairs id-attr name-attr))))

(defun org-shortcut-get-story
    (shortcut-id)
  (org-shortcut-request "GET" (format "/stories/%s" shortcut-id)))

(defun org-shortcut-link-to-story (story-id)
  (format "https://app.shortcut.io/%s/story/%d"
          org-shortcut-team-name
          story-id))

(defun org-shortcut-link-to-epic (epic-id)
  (format "https://app.shortcut.io/%s/epic/%d"
          org-shortcut-team-name
          epic-id))

(defun org-shortcut-link-to-milestone (milestone-id)
  (format "https://app.shortcut.io/%s/milestone/%d"
          org-shortcut-team-name
          milestone-id))

(defun org-shortcut-link-to-project (project-id)
  (format "https://app.shortcut.io/%s/project/%d"
          org-shortcut-team-name
          project-id))

;;;
;;; Caching
;;;

(comment
 (defcache org-shortcut-projects
   (org-sync-shortcut-fetch-as-id-name-pairs "projectx"))

 (clear-org-shortcut-projects-cache)
 (clear-org-shortcut-cache)
 ;;
)

(defvar org-shortcut-cache-clear-functions ())

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
             org-shortcut-cache-clear-functions))))

(defun org-shortcut-clear-cache ()
  (interactive)
  (-map #'funcall org-shortcut-cache-clear-functions))

;;;
;;; API resource functions
;;;

(defcache org-shortcut-projects
  "Returns projects as (project-id . name)"
  (org-shortcut-fetch-as-id-name-pairs "projects"))

(defcache org-shortcut-epics
  "Returns epics as (epic-id . name)"
  (org-shortcut-fetch-as-id-name-pairs "epics"))

(defcache org-shortcut-milestones
  "Returns milestone-id . name)"
  (org-shortcut-fetch-as-id-name-pairs "milestones"))

(defcache org-shortcut-workflow-states
  "Returns worflow states as (name . id) pairs"
  (let* ((resp-json (org-shortcut-request "GET" "workflows"))
         (workflows (->list resp-json))
         ;; just assume it exists, for now
         (workflow  (-find (lambda (workflow)
                             (equal org-shortcut-workflow-name
                                    (alist-get 'name workflow)))
                           workflows))
         (states    (->list (alist-get 'states workflow))))
    (to-id-name-pairs states
                      'name
                      'id)))

(defcache org-shortcut-labels
  "Returns labels as (label-id . name)"
  (org-shortcut-fetch-as-id-name-pairs "labels"))

(defcache org-shortcut-whoami
  "Returns the ID of the logged in user"
  (->> (org-shortcut-request
        "GET"
        "/members")
       ->list
       (find-if (lambda (m)
                  (->> m
                       (alist-get 'profile)
                       (alist-get 'mention_name)
                       (equal org-shortcut-username))))
       (alist-get 'id)))

(defcache org-shortcut-iterations
  "Returns iterations as (iteration-id . name)"
  (org-shortcut-fetch-as-id-name-pairs "iterations"))

(defun org-shortcut-stories-in-project (project-id)
  "Return the stories in the given PROJECT-ID as org headlines."
  (let ((resp-json (org-shortcut-request "GET" (format "/projects/%d/stories" project-id))))
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

(defun org-shortcut-workflow-state-id-to-todo-keyword (workflow-state-id)
  "Convert the named shortcut WORKFLOW-STATE-ID to an org todo keyword."
  (let* ((state-name (alist-get-equal
                      workflow-state-id
                      (invert-alist (org-shortcut-workflow-states))))
         (inv-state-name-alist
          (-map (lambda (cell) (cons (cdr cell) (car cell)))
                org-shortcut-state-alist)))
    (or (alist-get-equal state-name inv-state-name-alist)
        (if state-name (s-upcase state-name) "UNKNOWN"))))

;;;
;;; Prompting
;;;

(defun org-shortcut-prompt-for-project (cb)
  (ivy-read
   "Select a project: "
   (-map #'cdr (org-shortcut-projects))
   :require-match t
   :history 'org-shortcut-project-history
   :action (lambda (selected)
             (let ((project-id
                    (find-match-in-alist selected (org-shortcut-projects))))
               (funcall cb project-id)))))

(defun org-shortcut-prompt-for-epic (cb)
  "Prompt the user for an epic using ivy and call CB with its ID."
  (ivy-read
   "Select an epic: "
   (-map #'cdr (append '((nil . "No Epic")) (org-shortcut-epics)))
   :history 'org-shortcut-epic-history
   :action (lambda (selected)
             (let ((epic-id
                    (find-match-in-alist selected (org-shortcut-epics))))
               (funcall cb epic-id)))))

(defun org-shortcut-prompt-for-milestone (cb)
  "Prompt the user for a milestone using ivy and call CB with its ID."
  (ivy-read
   "Select a milestone: "
   (-map #'cdr (append '((nil . "No Milestone")) (org-shortcut-milestones)))
   :require-match t
   :history 'org-shortcut-milestone-history
   :action (lambda (selected)
             (let ((milestone-id
                    (find-match-in-alist selected (org-shortcut-milestones))))
               (funcall cb milestone-id)))))

(defun org-shortcut-prompt-for-story-type (cb)
  (ivy-read
   "Select a story type: "
   (-map #'cdr org-shortcut-story-types)
   :history 'org-shortcut-story-history
   :action (lambda (selected)
             (let ((story-type
                    (find-match-in-alist selected org-shortcut-story-types)))
               (funcall cb story-type)))))

(defun org-shortcut-prompt-for-default-story-type ()
  (interactive)
  (ivy-read
   "Select a default story type: "
   (-map #'cdr org-shortcut-default-story-types)
   :history 'org-shortcut-default-story-history
   :action (lambda (selected)
             (let ((story-type
                    (find-match-in-alist selected org-shortcut-default-story-types)))
                  (if (string-equal story-type "prompt")
                      (setq org-shortcut-default-story-type nil)
                      (setq org-shortcut-default-story-type story-type))))))

;;;
;;; Epic creation
;;;

(cl-defun org-shortcut-create-epic-internal
    (title &key milestone-id description labels)
  (cl-assert (and (stringp title)
                  (or (null milestone-id)
                      (integerp milestone-id))
                  (or (null description)
                      (stringp description))
                  (and (listp labels)
                       (-all? #'stringp labels))))
  (org-shortcut-request
   "POST"
   "epics"
   :data
   (json-encode
    `((name . ,title)
      (milestone_id . ,milestone-id)
      (description . ,(or description ""))
      (labels . ,labels)))))

(defun org-shortcut-populate-created-epic (elt epic)
  (let ((elt-start  (plist-get elt :begin))
        (epic-id    (alist-get 'id epic))
        (milestone-id (alist-get 'milestone_id epic)))
    (save-excursion
      (goto-char elt-start)

      (org-set-property "shortcut-epic-id"
                        (org-link-make-string
                         (org-shortcut-link-to-epic epic-id)
                         (number-to-string epic-id)))

      (when milestone-id
        (org-set-property "shortcut-milestone"
                          (org-link-make-string
                           (org-shortcut-link-to-milestone milestone-id)
                           (alist-get milestone-id (org-shortcut-milestones))))))))

(defun org-shortcut-create-epic (&optional beg end)
  "Creates a shortcut epic using selected headlines.
Will pull the title from the headline at point, or create epics for all the
headlines in the selected region.

All epics are added to the same milestone, as selected via a prompt.
If the epics already have a SHORTCUT-EPIC-ID, they are filtered and ignored."
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))

  (let* ((elts (org-shortcut-collect-headlines beg end))
         (elts (-remove (lambda (elt) (plist-get elt :SHORTCUT-EPIC-ID)) elts)))
    (org-shortcut-prompt-for-milestone
     (lambda (milestone-id)
       (dolist (elt elts)
         (let* ((title (plist-get elt :title))
                (description (org-shortcut--description-for-elt elt))
                (labels (org-shortcut--labels-for-elt elt))
                (epic  (org-shortcut-create-epic-internal
                        title
                        :milestone-id milestone-id
                        :labels labels
                        :description description)))
           (org-shortcut-populate-created-epic elt epic)))))))

;;;
;;; Story creation
;;;

(defun org-shortcut-default-state-id ()
  (alist-get-equal org-shortcut-default-state (org-shortcut-workflow-states)))

(cl-defun org-shortcut-create-story-internal
    (title &key project-id epic-id story-type description labels)
  (cl-assert (and (stringp title)
               (integerp project-id)
               (or (null epic-id) (integerp epic-id))
               (or (null description) (stringp description))))
  (let ((workflow-state-id (org-shortcut-default-state-id))
        (params `((name . ,title)
                  (project_id . ,project-id)
                  (epic_id . ,epic-id)
                  (story_type . ,story-type)
                  (description . ,(or description ""))
                  (labels . ,labels))))

    (when workflow-state-id
      (push `(workflow_state_id . ,workflow-state-id) params))

    (org-shortcut-request
     "POST"
     "stories"
     :data
     (json-encode params))))

(cl-defun org-shortcut-populate-created-story (elt story &key extra-properties)
  (let ((elt-start  (plist-get elt :begin))
        (story-id   (alist-get 'id story))
        (epic-id    (alist-get 'epic_id story))
        (project-id (alist-get 'project_id story))
        (story-type (alist-get 'story_type story)))

    (save-excursion
      (goto-char elt-start)

      (org-set-property "shortcut-id"
                        (org-link-make-string
                         (org-shortcut-link-to-story story-id)
                         (number-to-string story-id)))
      (when epic-id
        (org-set-property "shortcut-epic"
                          (org-link-make-string
                           (org-shortcut-link-to-epic epic-id)
                           (alist-get epic-id (org-shortcut-epics)))))

      (org-set-property "shortcut-project"
                        (org-link-make-string
                         (org-shortcut-link-to-project project-id)
                         (alist-get project-id (org-shortcut-projects))))

      (org-set-property "story-type"
                        (alist-get-equal story-type org-shortcut-story-types))

      (dolist (extra-prop extra-properties)
        (org-set-property (car extra-prop)
                          (alist-get (cdr extra-prop) story)))

      (org-todo "TODO"))))

(defun org-shortcut-create-story (&optional beg end &key then)
  "Creates a shortcut story using selected headlines.

Will pull the title from the headline at point,
or create cards for all the headlines in the selected region.

All stories are added to the same project and epic, as selected via two prompts.
If the stories already have a SHORTCUT-ID, they are filtered and ignored."
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))

  (let* ((elts     (org-shortcut-collect-headlines beg end))
         (new-elts (-remove (lambda (elt) (plist-get elt :SHORTCUT-ID)) elts)))
    (org-shortcut-prompt-for-project
     (lambda (project-id)
       (when project-id
         (org-shortcut-prompt-for-epic
          (lambda (epic-id)
            (let ((create-story
                   (lambda (story-type)
                     (-map
                      (lambda (elt)
                        (let* ((title (plist-get elt :title))
                               (description
                                (org-shortcut--description-for-elt elt))
                               (labels (org-shortcut--labels-for-elt elt))
                               (story (org-shortcut-create-story-internal
                                       title
                                       :project-id project-id
                                       :epic-id epic-id
                                       :story-type story-type
                                       :description description
                                       :labels labels)))
                          (org-shortcut-populate-created-story elt story)
                          (when (functionp then)
                            (funcall then story))))
                      new-elts))))
              (if org-shortcut-default-story-type
                  (funcall create-story org-shortcut-default-story-type)
                (org-shortcut-prompt-for-story-type create-story))))))))))

(defun org-shortcut-create-story-with-task-list (&optional beg end)
  "Creates a shortcut story using the selected headline, making all direct
children of that headline into tasks in the task list of the story."
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))

  (let* ((elt (org-element-and-children-at-point)))
    (org-shortcut-create-story nil nil
     :then (lambda (story)
             (pp story)
             (org-shortcut-push-task-list
              (alist-get 'id story)
              (plist-get elt :children))))))

;;;
;;; Task creation
;;;

(cl-defun org-shortcut-create-task (title &key story-id)
  (cl-assert (and (stringp title)
               (integerp story-id)))
  (org-shortcut-request
   "POST"
   (format "/stories/%d/tasks" story-id)
   :data (json-encode `((description . ,title)))))

(defun org-shortcut-push-task-list (&optional parent-shortcut-id child-elts)
  "Writes each child of the element at point as a task list item.

When called as (org-shortcut-push-task-list PARENT-SHORTCUT-ID CHILD-ELTS),
allows manually passing a shortcut ID and list of org-element plists to write"
  (interactive)
  (let* ((elt (org-element-and-children-at-point))
         (parent-shortcut-id (or parent-shortcut-id
                                  (org-element-extract-shortcut-id elt)))
         (child-elts (or child-elts (plist-get elt :children)))
         (story (org-shortcut-get-story parent-shortcut-id))
         (existing-tasks (alist-get 'tasks story))
         (task-exists
          (lambda (task-name)
            (cl-some (lambda (task)
                    (string-equal task-name (alist-get 'description task)))
                  existing-tasks)))
         (elts-with-starts
          (-map (lambda (e) (cons (set-marker (make-marker)
                                         (plist-get e :begin))
                             e))
                child-elts)))
    (dolist (child-elt-and-start elts-with-starts)
      (let* ((start (car child-elt-and-start))
             (child-elt (cdr child-elt-and-start))
             (task-name (plist-get child-elt :title)))
        (unless (funcall task-exists task-name)
          (let ((task (org-shortcut-create-task
                       task-name
                       :story-id parent-shortcut-id)))
            (org-shortcut-populate-created-task child-elt task start)))))))

(defun org-shortcut-populate-created-task (elt task &optional begin)
  (let ((elt-start (or begin (plist-get elt :begin)))
        (task-id   (alist-get 'id task))
        (story-id  (alist-get 'story_id task)))

    (save-excursion
      (goto-char elt-start)

      (org-set-property "shortcut-task-id" (format "%d" task-id))

      (org-set-property "shortcut-story-id"
                        (org-link-make-string
                         (org-shortcut-link-to-story story-id)
                         (number-to-string story-id)))

      (org-todo "TODO"))))

;;;
;;; Task Updates
;;;

(cl-defun org-shortcut-update-task-internal
    (story-id task-id &rest attrs)
  (cl-assert (and (integerp story-id)
                  (integerp task-id)
                  (listp attrs)))
  (org-shortcut-request
   "PUT"
   (format "stories/%d/tasks/%d" story-id task-id)
   :data
   (json-encode attrs)))

;;;
;;; Story updates
;;;

(cl-defun org-shortcut-update-story-internal
    (story-id &rest attrs)
  (cl-assert (and (integerp story-id)
                  (listp attrs)))
  (org-shortcut-request
   "PUT"
   (format "stories/%d" story-id)
   :data
   (json-encode attrs)))

(cl-defun org-shortcut-update-epic-internal
    (story-id &rest attrs)
  (cl-assert (and (integerp story-id)
                  (listp attrs)))
  (org-shortcut-request
   "PUT"
   (format "epics/%d" epic-id)
   :data
   (json-encode attrs)))

(cl-defun org-shortcut-update-story-at-point (&rest attrs)
  (when-let* ((shortcut-id (org-element-shortcut-id)))
    (apply
     #'org-shortcut-update-story-internal
     (cons shortcut-id attrs))
    t))

(cl-defun org-shortcut-update-epic-at-point (&rest attrs)
  (when-let* ((epic-id (org-element-shortcut-id :SHORTCUT-EPIC-ID)))
    (apply
     #'org-shortcut-update-epic-internal
     (cons epic-id attrs))
    t))

(defun org-shortcut-update-story-title ()
  "Update the title of the Shortcut story linked to the current headline.

Update the title of the story linked to the current headline with the text of
the headline."
  (interactive)

  (let* ((elt (org-element-find-headline))
         (title (plist-get elt :title))
         (shortcut-id (org-element-shortcut-id)))
    (and
     (org-shortcut-update-story-at-point
      shortcut-id
      :name title)
     (message "Successfully updated story title to \"%s\""
              title))))

(defun org-shortcut-update-status ()
  "Update the status of the Shortcut story linked to the current element.

Update the status of the Shortcut story linked to the current element with the
entry in `org-shortcut-state-alist' corresponding to the todo-keyword of the
element."
  (interactive)
  (let* ((elt (org-element-find-headline))
         (todo-keyword (-> elt
                           (plist-get :todo-keyword)
                           (substring-no-properties)))

         (shortcut-id (org-element-extract-shortcut-id elt))
         (task-id (plist-get elt :SHORTCUT-TASK-ID)))
    (cond
     (shortcut-id
      (let* ((todo-keyword (-> elt
                               (plist-get :todo-keyword)
                               (substring-no-properties))))
        (when-let* ((shortcut-workflow-state
                     (alist-get-equal todo-keyword org-shortcut-state-alist))
                    (workflow-state-id
                     (alist-get-equal shortcut-workflow-state
                                      (org-shortcut-workflow-states))))
          (let ((update-assignee?
                 (if (or (eq 't org-shortcut-claim-story-on-status-update)
                         (member todo-keyword
                                 org-shortcut-claim-story-on-status-update))
                     (if org-shortcut-username
                         't
                       (warn "Not claiming story since `org-shortcut-username'
                       is not set")
                       nil))))

            (if update-assignee?
                (org-shortcut-update-story-internal
                 shortcut-id
                 :workflow_state_id workflow-state-id
                 :owner_ids (if update-assignee?
                                (list (org-shortcut-whoami))
                              (list)))
              (org-shortcut-update-story-internal
                 shortcut-id
                 :workflow_state_id workflow-state-id))
            (message
             (if update-assignee?
                 "Successfully claimed story and updated shortcut status to \"%s\""
               "Successfully updated shortcut status to \"%s\"")
             shortcut-workflow-state)))))

     (task-id
      (let ((story-id (org-element-extract-clubhouse-id
                       elt
                       :SHORTCUT-STORY-ID))
            (done? (member todo-keyword org-done-keywords)))
        (org-shortcut-update-task-internal
         story-id
         (string-to-number task-id)
         :complete (if done? 't :json-false))
        (message "Successfully marked shortcut task status as %s"
                 (if done? "complete" "incomplete")))))))

(defun org-shortcut-update-description ()
  "Update the description of the Shortcut story linked to the current element.

Update the status of the Shortcut story linked to the current element with the
contents of a drawer inside the element called DESCRIPTION, if any."
  (interactive)
  (when-let* ((new-description (org-shortcut-find-description-drawer)))
    (and
     (org-shortcut-update-story-at-point
      :description new-description)
     (message "Successfully updated story description"))))

(defun org-shortcut-update-labels (&optional beg end)
  "Update the labels of the story or epic linked to the element at point.

When called interactively with a region, operates on all elements between BEG
and END.

Will use the value of `org-shortcut-create-stories-with-labels' to determine
which labels to set."
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))

  (dolist (elt (org-shortcut-collect-headlines beg end))
    (let* ((new-labels (org-shortcut--labels-for-elt elt))
           (label-desc (->> new-labels (-map #'cdar) (s-join ":"))))
      (case (org-shortcut--element-type elt)
        ('story
         (and
          (org-shortcut-update-story-at-point
           :labels new-labels)
          (message "Successfully updated story labels to :%s:"
                   label-desc)))
        ('epic
         (and
          (org-shortcut-update-epic-at-point :labels new-labels)
          (message "Successfully updated epic labels to :%s:"
                   label-desc)))
        (otherwise
         (message "Element at point is not a shortcut epic or story!"))))))


;;;
;;; Creating headlines from existing stories
;;;

(defun org-shortcut--task-to-headline-text (level task)
  (format "%s %s %s
:PROPERTIES:
:shortcut-task-id: %s
:shortcut-story-id: %s
:END:"
          (make-string level ?*)
          (if (equal :json-false (alist-get 'complete task))
              "TODO" "DONE")
          (alist-get 'description task)
          (alist-get 'id task)
          (let ((story-id (alist-get 'story_id task)))
            (org-link-make-string
             (org-shortcut-link-to-story story-id)
             story-id))))

(defun org-shortcut--story-to-headline-text (level story)
  (let ((story-id (alist-get 'id story)))
    (format
     "%s %s %s %s
:PROPERTIES:
:shortcut-id: %s
:END:
%s
%s
"
     (make-string level ?*)
     (org-shortcut-workflow-state-id-to-todo-keyword
      (alist-get 'workflow_state_id story))
     (alist-get 'name story)
     (if-let ((labels (->> story
                             (alist-get 'labels)
                             ->list
                             (-map (apply-partially #'alist-get 'name)))))
         (format ":%s:" (s-join ":" labels))
       "")
     (org-link-make-string
      (org-shortcut-link-to-story story-id)
      (number-to-string story-id))
     (let ((desc (alist-get 'description story)))
       (if (= 0 (length desc)) ""
         (format ":DESCRIPTION:\n%s\n:END:" desc)))
     (if-let ((tasks (seq-sort-by
                      (apply-partially #'alist-get 'position)
                      #'<
                      (or (alist-get 'tasks story)
                          (alist-get 'tasks
                                     (org-shortcut-get-story story-id))))))
         (mapconcat (apply-partially #'org-shortcut--task-to-headline-text
                                     (1+ level))
                    tasks
                    "\n")
       ""))))

(defun org-shortcut-headline-from-my-tasks (level)
  "Prompt my active stories and create a single `org-mode' headline at LEVEL."
  (interactive "*nLevel: \n")
  (if org-shortcut-username
      (let* ((story-list (org-shortcut--search-stories
                          (format "owner:%s !is:done !is:archived"
                                  org-shortcut-username)))
             (stories (to-id-name-pairs story-list)))
        (org-shortcut-headline-from-story-id level
                                              (find-match-in-alist
                                               (ivy-read "Select Story: "
                                                         (-map #'cdr stories))
                                               stories)))
    (warn "Can't fetch my tasks if `org-shortcut-username' is unset")))

(defun org-shortcut-headline-from-story-id (level story-id)
  "Create a single `org-mode' headline at LEVEL based on the given shortcut STORY-ID."
  (interactive "*nLevel: \nnStory ID: ")
  (let* ((story (org-shortcut-get-story story-id)))
    (if (equal '((message . "Resource not found.")) story)
        (message "Story ID not found: %d" story-id)
      (save-mark-and-excursion
        (insert (org-shortcut--story-to-headline-text level story))
        (org-align-tags)))))

(defun org-shortcut--search-stories (query)
  (unless (string= "" query)
    (-> (org-shortcut-request "GET" "search/stories" :params `((query ,query)))
        cdadr
        (append nil)
        reject-archived)))

(defun org-shortcut-prompt-for-iteration (cb)
  "Prompt for iteration and call CB with that iteration"
  (ivy-read
   "Select an interation: "
   (-map #'cdr (org-shortcut-iterations))
   :require-match t
   :history 'org-shortcut-iteration-history
   :action (lambda (selected)
             (let ((iteration-id
                    (find-match-in-alist selected (org-shortcut-iterations))))
               (funcall cb iteration-id)))))

(defun org-shortcut--get-iteration (iteration-id)
  (-> (org-shortcut-request "GET" (format "iterations/%d/stories" iteration-id))
      (append nil)))

(defun org-shortcut-headlines-from-iteration (level)
  "Create `org-mode' headlines from a shortcut iteration.

Create `org-mode' headlines from all the resulting stories at headline level LEVEL."
  (interactive "*nLevel: ")
  (org-shortcut-prompt-for-iteration
   (lambda (iteration-id)
     (let ((story-list (org-shortcut--get-iteration iteration-id)))
       (if (null story-list)
           (message "Iteration id returned no stories: %d" iteration-id)
         (let ((text (mapconcat (apply-partially
                                 #'org-shortcut--story-to-headline-text
                                 level)
                                (reject-archived story-list) "\n")))
               (save-mark-and-excursion
                 (insert text)
                 (org-align-all-tags))
             text))))))

(defun org-shortcut-headlines-from-query (level query)
  "Create `org-mode' headlines from a shortcut query.

Submits QUERY to shortcut, and creates `org-mode' headlines from all the
resulting stories at headline level LEVEL."
  (interactive
   "*nLevel: \nMQuery: ")
  (let* ((story-list (org-shortcut--search-stories query)))
    (if (null story-list)
        (message "Query returned no stories: %s" query)
      (let ((text (mapconcat (apply-partially
                              #'org-shortcut--story-to-headline-text
                              level)
                             (reject-archived story-list) "\n")))
        (if (called-interactively-p)
            (save-mark-and-excursion
              (insert text)
              (org-align-all-tags))
          text)))))

(defun org-shortcut-prompt-for-story (cb)
  "Prompt the user for a shortcut story, then call CB with the full story."
  (ivy-read "Story title: "
            (lambda (search-term)
              (let* ((stories (org-shortcut--search-stories
                               (if search-term (format "\"%s\"" search-term)
                                 ""))))
                (-map (lambda (story)
                        (propertize (alist-get 'name story) 'story story))
                      stories)))
            :dynamic-collection t
            :history 'org-shortcut-story-prompt
            :action (lambda (s) (funcall cb (get-text-property 0 'story s)))
            :require-match t))

(defun org-shortcut-headline-from-story (level)
  "Prompt for a story, and create an org headline at LEVEL from that story."
  (interactive "*nLevel: ")
  (org-shortcut-prompt-for-story
   (lambda (story)
     (save-mark-and-excursion
       (insert (org-shortcut--story-to-headline-text level story))
       (org-align-tags)))))


(defun org-shortcut-link ()
  "Link the current `org-mode' headline with an existing shortcut story."
  (interactive)
  (org-shortcut-prompt-for-story
   (lambda (story)
     (org-shortcut-populate-created-story
      (org-element-find-headline)
      story
      :extra-properties '(("shortcut-story-name" . name)))
     (org-todo
      (org-shortcut-workflow-state-id-to-todo-keyword
       (alist-get 'workflow_state_id story))))))

(defun org-shortcut-claim ()
  "Assign the shortcut story associated with the headline at point to yourself."
  (interactive)
  (if org-shortcut-username
      (and
       (org-shortcut-update-story-at-point
        :owner_ids (list (org-shortcut-whoami)))
       (message "Successfully claimed story"))
    (warn "Can't claim story if `org-shortcut-username' is unset")))

(defun org-shortcut-sync-status (&optional beg end)
  "Pull the status(es) for the story(ies) in region and update the todo state.

Uses `org-shortcut-state-alist'. Operates over stories from BEG to END"
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))
  (let ((elts (-filter (lambda (e) (plist-get e :SHORTCUT-ID))
                       (org-shortcut-collect-headlines beg end))))
    (save-mark-and-excursion
      (dolist (e elts)
        (goto-char (plist-get e :begin))
        (let* ((shortcut-id (org-element-extract-shortcut-id e))
               (story (org-shortcut-get-story shortcut-id))
               (workflow-state-id (alist-get 'workflow_state_id story))
               (todo-keyword (org-shortcut-workflow-state-id-to-todo-keyword
                              workflow-state-id)))
          (let ((org-after-todo-state-change-hook
                 (remove 'org-shortcut-update-status
                         org-after-todo-state-change-hook)))
            (org-todo todo-keyword)))))
    (message "Successfully synchronized status of %d stories from Shortcut"
             (length elts))))

(cl-defun org-shortcut-set-epic (&optional story-id epic-id cb &key beg end)
  "Set the epic of shortcut story STORY-ID to EPIC-ID, then call CB.

When called interactively, prompt for an epic and set the story of the shortcut
stor{y,ies} at point or region"
  (interactive
   (when (use-region-p)
     (list nil nil nil
           :beg (region-beginning)
           :end (region-end))))
  (if (and story-id epic-id)
      (progn
        (org-shortcut-update-story-internal
         story-id :epic-id epic-id)
        (when cb (funcall cb)))
    (let ((elts (-filter (lambda (elt) (plist-get elt :SHORTCUT-ID))
                         (org-shortcut-collect-headlines beg end))))
      (org-shortcut-prompt-for-epic
       (lambda (epic-id)
         (-map
          (lambda (elt)
            (let ((story-id (org-element-extract-shortcut-id elt)))
              (org-shortcut-set-epic
               story-id epic-id
               (lambda ()
                 (org-set-property
                  "shortcut-epic"
                  (org-link-make-string
                   (org-shortcut-link-to-epic epic-id)
                   (alist-get epic-id (org-shortcut-epics))))
                 (message "Successfully set the epic on story %d to %d"
                          story-id epic-id)))))
          elts))))))

;;;

(define-minor-mode org-shortcut-mode
  "If enabled, updates to the todo keywords on org headlines will update the
linked ticket in Shortcut."
  :group 'org
  :lighter "Org-Shortcut"
  :keymap '()
  (add-hook 'org-after-todo-state-change-hook
            'org-shortcut-update-status
            nil
            t))

(provide 'org-shortcut)

;;; org-shortcut.el ends here
