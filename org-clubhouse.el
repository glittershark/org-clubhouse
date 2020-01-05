;;; org-clubhouse.el --- Simple, unopinionated integration between org-mode and
;;; Clubhouse

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

(require 'cl-macs)
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
  "Authorization token for the Clubhouse API.")

(defvar org-clubhouse-username nil
  "Username for the current Clubhouse user.

Unfortunately, the Clubhouse API doesn't seem to provide this via the API given
an API token, so we need to configure this for
`org-clubhouse-claim-story-on-status-updates' to work")

(defvar org-clubhouse-team-name nil
  "Team name to use in links to Clubhouse.
ie https://app.clubhouse.io/<TEAM_NAME>/stories")

(defvar org-clubhouse-project-ids nil
  "Specific list of project IDs to synchronize with clubhouse.
If unset all projects will be synchronized")

(defvar org-clubhouse-workflow-name "Default")

(defvar org-clubhouse-default-story-type nil
  "Sets the default story type. If set to 'nil', it will interactively prompt
the user each and every time a new story is created. If set to 'feature',
'bug', or 'chore', that value will be used as the default and the user will
not be prompted")

(defvar org-clubhouse-state-alist
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
  Clubhouse. In `org-clubhouse-mode', moving headlines to these todo keywords
  will update to the corresponding status in Clubhouse")

(defvar org-clubhouse-story-types
  '(("feature" . "Feature")
    ("bug"     . "Bug")
    ("chore"   . "Chore")))

(defvar org-clubhouse-default-story-types
  '(("feature" . "Feature")
    ("bug"     . "Bug")
    ("chore"   . "Chore")
    ("prompt"  . "**Prompt each time (do not set a default story type)**")))

(defvar org-clubhouse-default-state "Proposed"
  "Default state to create all new stories in.")

(defvar org-clubhouse-claim-story-on-status-update 't
  "Controls the assignee behavior of stories on status update.

If set to 't, will mark the current user as the owner of any clubhouse
stories on any update to the status.

If set to nil, will never automatically update the assignee of clubhouse
stories.

If set to a list of todo-state's, will mark the current user as the owner of
clubhouse stories whenever updating the status to one of those todo states.")

(defvar org-clubhouse-create-stories-with-labels nil
  "Controls the way org-clubhouse creates stories with labels based on org tags.

If set to 't, will create labels for all org tags on headlines when stories are
created.

If set to 'existing, will set labels on created stories only if the label
already exists in clubhouse

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

(defun org-clubhouse-collect-headlines (beg end)
  "Collects the headline at point or the headlines in a region. Returns a list."
  (setq test-headlines
  (if (and beg end)
      (org-clubhouse-get-headlines-in-region beg end)
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

(defun org-element-extract-clubhouse-id (elt &optional property)
  (when-let* ((clubhouse-id-link (plist-get elt (or property :CLUBHOUSE-ID))))
    (cond
     ((string-match
       (rx "[[" (one-or-more anything) "]"
           "[" (group (one-or-more digit)) "]]")
       clubhouse-id-link)
      (string-to-number (match-string 1 clubhouse-id-link)))
     ((string-match-p
       (rx buffer-start
           (one-or-more digit)
           buffer-end)
       clubhouse-id-link)
      (string-to-number clubhouse-id-link)))))

(comment
 (let ((strn "[[https://app.clubhouse.io/example/story/2330][2330]]"))
   (string-match
    (rx "[[" (one-or-more anything) "]"
        "[" (group (one-or-more digit)) "]]")
    strn)
   (string-to-number (match-string 1 strn)))
 )

(defun org-element-clubhouse-id ()
  (org-element-extract-clubhouse-id
   (org-element-find-headline)))

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

(defun org-clubhouse-find-description-drawer ()
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
           (org-clubhouse-find-description-drawer)))))))

(defun org-clubhouse--labels-for-elt (elt)
  "Return the Clubhouse labels based on the tags of ELT and the user's config."
  (unless (eq nil org-clubhouse-create-stories-with-labels)
    (let ((tags (org-get-tags (plist-get elt :contents-begin))))
      (-map (lambda (l) `((name . ,l)))
            (cl-case org-clubhouse-create-stories-with-labels
              ('t tags)
              ('existing (-filter (lambda (tag) (-some (lambda (l)
                                                    (string-equal tag (cdr l)))
                                                  (org-clubhouse-labels)))
                                  tags)))))))

;;;
;;; API integration
;;;

(defvar org-clubhouse-base-url* "https://api.clubhouse.io/api/v3")

(defun org-clubhouse-auth-url (url &optional params)
 (concat url
         "?"
         (url-build-query-string
          (cons `("token" ,org-clubhouse-auth-token) params))))

(defun org-clubhouse-baseify-url (url)
 (if (s-starts-with? org-clubhouse-base-url* url) url
   (concat org-clubhouse-base-url*
           (if (s-starts-with? "/" url) url
             (concat "/" url)))))

(cl-defun org-clubhouse-request (method url &key data (params '()))
 (message "%s %s %s" method url (prin1-to-string data))
 (let* ((url-request-method method)
        (url-request-extra-headers
         '(("Content-Type" . "application/json")))
        (url-request-data data)
        (buf))

   (setq url (-> url
                 org-clubhouse-baseify-url
                 (org-clubhouse-auth-url params)))

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

(defun org-clubhouse-get-story
    (clubhouse-id)
  (org-clubhouse-request "GET" (format "/stories/%s" clubhouse-id)))

(defun org-clubhouse-link-to-story (story-id)
  (format "https://app.clubhouse.io/%s/story/%d"
          org-clubhouse-team-name
          story-id))

(defun org-clubhouse-link-to-epic (epic-id)
  (format "https://app.clubhouse.io/%s/epic/%d"
          org-clubhouse-team-name
          epic-id))

(defun org-clubhouse-link-to-milestone (milestone-id)
  (format "https://app.clubhouse.io/%s/milestone/%d"
          org-clubhouse-team-name
          milestone-id))

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

(defcache org-clubhouse-milestones
  "Returns milestone-id . name)"
  (org-clubhouse-fetch-as-id-name-pairs "milestones"))

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

(defcache org-clubhouse-labels
  "Returns labels as (label-id . name)"
  (org-clubhouse-fetch-as-id-name-pairs "labels"))

(defcache org-clubhouse-whoami
  "Returns the ID of the logged in user"
  (->> (org-clubhouse-request
        "GET"
        "/members")
       ->list
       (find-if (lambda (m)
                  (->> m
                       (alist-get 'profile)
                       (alist-get 'mention_name)
                       (equal org-clubhouse-username))))
       (alist-get 'id)))

(defun org-clubhouse-stories-in-project (project-id)
  "Return the stories in the given PROJECT-ID as org headlines."
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

(defun org-clubhouse-workflow-state-id-to-todo-keyword (workflow-state-id)
  "Convert the named clubhouse WORKFLOW-STATE-ID to an org todo keyword."
  (let* ((state-name (alist-get-equal
                      workflow-state-id
                      (invert-alist (org-clubhouse-workflow-states))))
         (inv-state-name-alist
          (-map (lambda (cell) (cons (cdr cell) (car cell)))
                org-clubhouse-state-alist)))
    (or (alist-get-equal state-name inv-state-name-alist)
        (s-upcase state-name))))

;;;
;;; Prompting
;;;

(defun org-clubhouse-prompt-for-project (cb)
  (ivy-read
   "Select a project: "
   (-map #'cdr (org-clubhouse-projects))
   :require-match t
   :history 'org-clubhouse-project-history
   :action (lambda (selected)
             (let ((project-id
                    (find-match-in-alist selected (org-clubhouse-projects))))
               (funcall cb project-id)))))

(defun org-clubhouse-prompt-for-epic (cb)
  (ivy-read
   "Select an epic: "
   (-map #'cdr (org-clubhouse-epics))
   :history 'org-clubhouse-epic-history
   :action (lambda (selected)
             (let ((epic-id
                    (find-match-in-alist selected (org-clubhouse-epics))))
               (funcall cb epic-id)))))

(defun org-clubhouse-prompt-for-milestone (cb)
  (ivy-read
   "Select a milestone: "
   (-map #'cdr (org-clubhouse-milestones))
   :require-match t
   :history 'org-clubhouse-milestone-history
   :action (lambda (selected)
             (let ((milestone-id
                    (find-match-in-alist selected (org-clubhouse-milestones))))
               (funcall cb milestone-id)))))

(defun org-clubhouse-prompt-for-story-type (cb)
  (ivy-read
   "Select a story type: "
   (-map #'cdr org-clubhouse-story-types)
   :history 'org-clubhouse-story-history
   :action (lambda (selected)
             (let ((story-type
                    (find-match-in-alist selected org-clubhouse-story-types)))
               (funcall cb story-type)))))

(defun org-clubhouse-prompt-for-default-story-type ()
  (interactive)
  (ivy-read
   "Select a default story type: "
   (-map #'cdr org-clubhouse-default-story-types)
   :history 'org-clubhouse-default-story-history
   :action (lambda (selected)
             (let ((story-type
                    (find-match-in-alist selected org-clubhouse-default-story-types)))
                  (if (string-equal story-type "prompt")
                      (setq org-clubhouse-default-story-type nil)
                      (setq org-clubhouse-default-story-type story-type))))))

;;;
;;; Epic creation
;;;

(cl-defun org-clubhouse-create-epic-internal
    (title &key milestone-id)
  (cl-assert (and (stringp title)
               (integerp milestone-id)))
  (org-clubhouse-request
   "POST"
   "epics"
   :data
   (json-encode
    `((name . ,title)
      (milestone_id . ,milestone-id)))))

(defun org-clubhouse-populate-created-epic (elt epic)
  (let ((elt-start  (plist-get elt :begin))
        (epic-id    (alist-get 'id epic))
        (milestone-id (alist-get 'milestone_id epic)))

    (save-excursion
      (goto-char elt-start)

      (org-set-property "clubhouse-epic-id"
                        (org-make-link-string
                         (org-clubhouse-link-to-epic epic-id)
                         (number-to-string epic-id)))

      (org-set-property "clubhouse-milestone"
                        (org-make-link-string
                         (org-clubhouse-link-to-milestone milestone-id)
                         (alist-get milestone-id (org-clubhouse-milestones)))))))

(defun org-clubhouse-create-epic (&optional beg end)
  "Creates a clubhouse epic using selected headlines.
Will pull the title from the headline at point, or create epics for all the
headlines in the selected region.

All epics are added to the same milestone, as selected via a prompt.
If the epics already have a CLUBHOUSE-EPIC-ID, they are filtered and ignored."
  (interactive
   (when (use-region-p)
     (list (region-beginning region-end))))

  (let* ((elts (org-clubhouse-collect-headlines beg end))
         (elts (-remove (lambda (elt) (plist-get elt :CLUBHOUSE-EPIC-ID)) elts)))
    (org-clubhouse-prompt-for-milestone
     (lambda (milestone-id)
       (when milestone-id
         (dolist (elt elts)
           (let* ((title (plist-get elt :title))
                  (epic  (org-clubhouse-create-epic-internal
                          title
                          :milestone-id milestone-id)))
             (org-clubhouse-populate-created-epic elt epic))
               elts))))))

;;;
;;; Story creation
;;;

(defun org-clubhouse-default-state-id ()
  (alist-get-equal org-clubhouse-default-state (org-clubhouse-workflow-states)))

(cl-defun org-clubhouse-create-story-internal
    (title &key project-id epic-id story-type description labels)
  (cl-assert (and (stringp title)
               (integerp project-id)
               (or (null epic-id) (integerp epic-id))
               (or (null description) (stringp description))))
  (let ((workflow-state-id (org-clubhouse-default-state-id))
        (params `((name . ,title)
                  (project_id . ,project-id)
                  (epic_id . ,epic-id)
                  (story_type . ,story-type)
                  (description . ,(or description ""))
                  (labels . ,labels))))

    (when workflow-state-id
      (push `(workflow_state_id . ,workflow-state-id) params))

    (org-clubhouse-request
     "POST"
     "stories"
     :data
     (json-encode params))))

(cl-defun org-clubhouse-populate-created-story (elt story &key extra-properties)
  (let ((elt-start  (plist-get elt :begin))
        (story-id   (alist-get 'id story))
        (epic-id    (alist-get 'epic_id story))
        (project-id (alist-get 'project_id story))
        (story-type (alist-get 'story_type story)))

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

      (org-set-property "story-type"
                        (alist-get-equal story-type org-clubhouse-story-types))

      (dolist (extra-prop extra-properties)
        (org-set-property (car extra-prop)
                          (alist-get (cdr extra-prop) story)))

      (org-todo "TODO"))))

(defun org-clubhouse-create-story (&optional beg end &key then)
  "Creates a clubhouse story using selected headlines.

Will pull the title from the headline at point,
or create cards for all the headlines in the selected region.

All stories are added to the same project and epic, as selected via two prompts.
If the stories already have a CLUBHOUSE-ID, they are filtered and ignored."
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))

  (let* ((elts     (org-clubhouse-collect-headlines beg end))
         (new-elts (-remove (lambda (elt) (plist-get elt :CLUBHOUSE-ID)) elts)))
    (org-clubhouse-prompt-for-project
     (lambda (project-id)
       (when project-id
         (org-clubhouse-prompt-for-epic
          (lambda (epic-id)
            (let ((create-story
                   (lambda (story-type)
                     (-map
                      (lambda (elt)
                        (let* ((title (plist-get elt :title))
                               (description
                                (save-mark-and-excursion
                                  (goto-char (plist-get elt :begin))
                                  (org-clubhouse-find-description-drawer)))
                               (labels (org-clubhouse--labels-for-elt elt))
                               (story (org-clubhouse-create-story-internal
                                       title
                                       :project-id project-id
                                       :epic-id epic-id
                                       :story-type story-type
                                       :description description
                                       :labels labels)))
                          (org-clubhouse-populate-created-story elt story)
                          (when (functionp then)
                            (funcall then story))))
                      new-elts))))
              (if org-clubhouse-default-story-type
                  (funcall create-story org-clubhouse-default-story-type)
                (org-clubhouse-prompt-for-story-type create-story))))))))))

(defun org-clubhouse-create-story-with-task-list (&optional beg end)
  "Creates a clubhouse story using the selected headline, making all direct
children of that headline into tasks in the task list of the story."
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))

  (let* ((elt (org-element-and-children-at-point)))
    (org-clubhouse-create-story nil nil
     :then (lambda (story)
             (pp story)
             (org-clubhouse-push-task-list
              (alist-get 'id story)
              (plist-get elt :children))))))

;;;
;;; Task creation
;;;

(cl-defun org-clubhouse-create-task (title &key story-id)
  (cl-assert (and (stringp title)
               (integerp story-id)))
  (org-clubhouse-request
   "POST"
   (format "/stories/%d/tasks" story-id)
   :data (json-encode `((description . ,title)))))

(defun org-clubhouse-push-task-list (&optional parent-clubhouse-id child-elts)
  "Writes each child of the element at point as a task list item.

When called as (org-clubhouse-push-task-list PARENT-CLUBHOUSE-ID CHILD-ELTS),
allows manually passing a clubhouse ID and list of org-element plists to write"
  (interactive)
  (let* ((elt (org-element-and-children-at-point))
         (parent-clubhouse-id (or parent-clubhouse-id
                                  (org-element-extract-clubhouse-id elt)))
         (child-elts (or child-elts (plist-get elt :children)))
         (story (org-clubhouse-get-story parent-clubhouse-id))
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
          (let ((task (org-clubhouse-create-task
                       task-name
                       :story-id parent-clubhouse-id)))
            (org-clubhouse-populate-created-task child-elt task start)))))))

(defun org-clubhouse-populate-created-task (elt task &optional begin)
  (let ((elt-start (or begin (plist-get elt :begin)))
        (task-id   (alist-get 'id task))
        (story-id  (alist-get 'story_id task)))

    (save-excursion
      (goto-char elt-start)

      (org-set-property "clubhouse-task-id" (format "%d" task-id))

      (org-set-property "clubhouse-story-id"
                        (org-make-link-string
                         (org-clubhouse-link-to-story story-id)
                         (number-to-string story-id)))

      (org-todo "TODO"))))

;;;
;;; Task Updates
;;;

(cl-defun org-clubhouse-update-task-internal
    (story-id task-id &rest attrs)
  (cl-assert (and (integerp story-id)
                  (integerp task-id)
                  (listp attrs)))
  (org-clubhouse-request
   "PUT"
   (format "stories/%d/tasks/%d" story-id task-id)
   :data
   (json-encode attrs)))

;;;
;;; Story updates
;;;

(cl-defun org-clubhouse-update-story-internal
    (story-id &rest attrs)
  (cl-assert (and (integerp story-id)
               (listp attrs)))
  (org-clubhouse-request
   "PUT"
   (format "stories/%d" story-id)
   :data
   (json-encode attrs)))

(cl-defun org-clubhouse-update-story-at-point (&rest attrs)
  (when-let* ((clubhouse-id (org-element-clubhouse-id)))
    (apply
     #'org-clubhouse-update-story-internal
     (cons clubhouse-id attrs))
    t))

(defun org-clubhouse-update-story-title ()
  "Update the title of the Clubhouse story linked to the current headline.

Update the title of the story linked to the current headline with the text of
the headline."
  (interactive)

  (let* ((elt (org-element-find-headline))
         (title (plist-get elt :title)))
    (and
     (org-clubhouse-update-story-at-point
      clubhouse-id
      :name title)
     (message "Successfully updated story title to \"%s\""
              title))))

(defun org-clubhouse-update-status ()
  "Update the status of the Clubhouse story linked to the current element.

Update the status of the Clubhouse story linked to the current element with the
entry in `org-clubhouse-state-alist' corresponding to the todo-keyword of the
element."
  (interactive)
  (let* ((elt (org-element-find-headline))
         (todo-keyword (-> elt
                           (plist-get :todo-keyword)
                           (substring-no-properties)))

         (clubhouse-id (org-element-extract-clubhouse-id elt))
         (task-id (plist-get elt :CLUBHOUSE-TASK-ID)))
    (cond
     (clubhouse-id
      (let* ((todo-keyword (-> elt
                               (plist-get :todo-keyword)
                               (substring-no-properties))))
        (when-let* ((clubhouse-workflow-state
                     (alist-get-equal todo-keyword org-clubhouse-state-alist))
                    (workflow-state-id
                     (alist-get-equal clubhouse-workflow-state
                                      (org-clubhouse-workflow-states))))
          (let ((update-assignee?
                 (if (or (eq 't org-clubhouse-claim-story-on-status-update)
                         (member todo-keyword
                                 org-clubhouse-claim-story-on-status-update))
                     (if org-clubhouse-username
                         't
                       (warn "Not claiming story since `org-clubhouse-username'
                       is not set")
                       nil))))

            (org-clubhouse-update-story-internal
             clubhouse-id
             :workflow_state_id workflow-state-id
             :owner_ids (when update-assignee?
                          (list (org-clubhouse-whoami))))
            (message
             (if update-assignee?
                 "Successfully claimed story and updated clubhouse status to \"%s\""
               "Successfully updated clubhouse status to \"%s\"")
             clubhouse-workflow-state)))))

     (task-id
      (let ((story-id (org-element-extract-clubhouse-id
                       elt
                       :CLUBHOUSE-STORY-ID))
            (done? (member todo-keyword org-done-keywords)))
        (org-clubhouse-update-task-internal
         story-id
         (string-to-number task-id)
         :complete (if done? 't :json-false))
        (message "Successfully marked clubhouse task status as %s"
                 (if done? "complete" "incomplete")))))))

(defun org-clubhouse-update-description ()
  "Update the description of the Clubhouse story linked to the current element.

Update the status of the Clubhouse story linked to the current element with the
contents of a drawer inside the element called DESCRIPTION, if any."
  (interactive)
  (when-let* ((new-description (org-clubhouse-find-description-drawer)))
    (and
     (org-clubhouse-update-story-at-point
      :description new-description)
     (message "Successfully updated story description"))))

(defun org-clubhouse-update-labels ()
  "Update the labels of the Clubhouse story linked to the current element.

Will use the value of `org-clubhouse-create-stories-with-labels' to determine
which labels to set."
  (interactive)
  (when-let* ((elt (org-element-find-headline))
              (new-labels (org-clubhouse--labels-for-elt elt)))
    (and
     (org-clubhouse-update-story-at-point
      :labels new-labels)
     (message "Successfully updated story labels to :%s:"
              (->> new-labels
                   (-map #'cdar)
                   (s-join ":"))))))


;;;
;;; Creating headlines from existing stories
;;;

(defun org-clubhouse--task-to-headline-text (level task)
  (format "%s %s %s
:PROPERTIES:
:clubhouse-task-id: %s
:clubhouse-story-id: %s
:END:"
          (make-string level ?*)
          (if (equal :json-false (alist-get 'complete task))
              "TODO" "DONE")
          (alist-get 'description task)
          (alist-get 'id task)
          (let ((story-id (alist-get 'story_id task)))
            (org-make-link-string
             (org-clubhouse-link-to-story story-id)
             story-id))))

(defun org-clubhouse--story-to-headline-text (level story)
  (let ((story-id (alist-get 'id story)))
    (format
     "%s %s %s %s
:PROPERTIES:
:clubhouse-id: %s
:END:
%s
%s
"
     (make-string level ?*)
     (org-clubhouse-workflow-state-id-to-todo-keyword
      (alist-get 'workflow_state_id story))
     (alist-get 'name story)
     (if-let ((labels (->> story
                             (alist-get 'labels)
                             ->list
                             (-map (apply-partially #'alist-get 'name)))))
         (format ":%s:" (s-join ":" labels))
       "")
     (org-make-link-string
      (org-clubhouse-link-to-story story-id)
      (number-to-string story-id))
     (let ((desc (alist-get 'description story)))
       (if (= 0 (length desc)) ""
         (format ":DESCRIPTION:\n%s\n:END:" desc)))
     (if-let ((tasks (seq-sort-by
                      (apply-partially #'alist-get 'position)
                      #'<
                      (or (alist-get 'tasks story)
                          (alist-get 'tasks
                                     (org-clubhouse-get-story story-id))))))
         (mapconcat (apply-partially #'org-clubhouse--task-to-headline-text
                                     (1+ level))
                    tasks
                    "\n")
       ""))))

(defun org-clubhouse-headline-from-story-id (level story-id)
  "Create a single `org-mode' headline at LEVEL based on the given clubhouse STORY-ID."
  (interactive "*nLevel: \nnStory ID: ")
  (let* ((story (org-clubhouse-get-story story-id)))
    (if (equal '((message . "Resource not found.")) story)
        (message "Story ID not found: %d" story-id)
      (save-mark-and-excursion
        (insert (org-clubhouse--story-to-headline-text level story))
        (org-align-tags)))))

(defun org-clubhouse--search-stories (query)
  (unless (string= "" query)
    (-> (org-clubhouse-request "GET" "search/stories" :params `((query ,query)))
        cdadr
        (append nil)
        reject-archived)))

(defun org-clubhouse-headlines-from-query (level query)
  "Create `org-mode' headlines from a clubhouse query.

Submits QUERY to clubhouse, and creates `org-mode' headlines from all the
resulting stories at headline level LEVEL."
  (interactive
   "*nLevel: \nMQuery: ")
  (let* ((story-list (org-clubhouse--search-stories query)))
    (if (null story-list)
        (message "Query returned no stories: %s" query)
      (let ((text (mapconcat (apply-partially
                              #'org-clubhouse--story-to-headline-text
                              level)
                             (reject-archived story-list) "\n")))
        (if (called-interactively-p)
            (save-mark-and-excursion
              (insert text)
              (org-align-all-tags))
          text)))))

(defun org-clubhouse-prompt-for-story (cb)
  "Prompt the user for a clubhouse story, then call CB with the full story."
  (ivy-read "Story title: "
            (lambda (search-term)
              (let* ((stories (org-clubhouse--search-stories
                               (if search-term (format "\"%s\"" search-term)
                                 ""))))
                (-map (lambda (story)
                        (propertize (alist-get 'name story) 'story story))
                      stories)))
            :dynamic-collection t
            :history 'org-clubhouse-story-prompt
            :action (lambda (s) (funcall cb (get-text-property 0 'story s)))
            :require-match t))

(defun org-clubhouse-headline-from-story (level)
  "Prompt for a story, and create an org headline at LEVEL from that story."
  (interactive "*nLevel: ")
  (org-clubhouse-prompt-for-story
   (lambda (story)
     (save-mark-and-excursion
       (insert (org-clubhouse--story-to-headline-text level story))
       (org-align-tags)))))


(defun org-clubhouse-link ()
  "Link the current `org-mode' headline with an existing clubhouse story."
  (interactive)
  (org-clubhouse-prompt-for-story
   (lambda (story)
     (org-clubhouse-populate-created-story
      (org-element-find-headline)
      story
      :extra-properties '(("clubhouse-story-name" . name)))
     (org-todo
      (org-clubhouse-workflow-state-id-to-todo-keyword
       (alist-get 'workflow_state_id story))))))

(defun org-clubhouse-claim ()
  "Assign the clubhouse story associated with the headline at point to yourself."
  (interactive)
  (if org-clubhouse-username
      (and
       (org-clubhouse-update-story-at-point
        :owner_ids (list (org-clubhouse-whoami)))
       (message "Successfully claimed story"))
    (warn "Can't claim story if `org-clubhouse-username' is unset")))

(defun org-clubhouse-sync-status (&optional beg end)
  "Pull the status(es) for the story(ies) in region and update the todo state.

Uses `org-clubhouse-state-alist'. Operates over stories from BEG to END"
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))
  (let ((elts (-filter (lambda (e) (plist-get e :CLUBHOUSE-ID))
                       (org-clubhouse-collect-headlines beg end))))
    (save-mark-and-excursion
      (dolist (e elts)
        (goto-char (plist-get e :begin))
        (let* ((clubhouse-id (org-element-extract-clubhouse-id e))
               (story (org-clubhouse-get-story clubhouse-id))
               (workflow-state-id (alist-get 'workflow_state_id story))
               (todo-keyword (org-clubhouse-workflow-state-id-to-todo-keyword
                              workflow-state-id)))
          (let ((org-after-todo-state-change-hook
                 (remove 'org-clubhouse-update-status
                         org-after-todo-state-change-hook)))
            (org-todo todo-keyword)))))
    (message "Successfully synchronized status of %d stories from Clubhouse"
             (length elts))))

;;;

(define-minor-mode org-clubhouse-mode
  "If enabled, updates to the todo keywords on org headlines will update the
linked ticket in Clubhouse."
  :group 'org
  :lighter "Org-Clubhouse"
  :keymap '()
  (add-hook 'org-after-todo-state-change-hook
            'org-clubhouse-update-status
            nil
            t))

(provide 'org-clubhouse)

;;; org-clubhouse.el ends here
