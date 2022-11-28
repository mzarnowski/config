;;;  -*- lexical-binding: t -*-

;; org-related extensions
(defun org-find-or-create-headline (headline &optional reposition)
  "Find or create a given headline"
  (when (eq major-mode 'org-mode)
    (save-match-data
      (when reposition (goto-char (point-min)))
      (unless (re-search-forward  (concat "^" headline "$") nil 'no-error)
	(end-of-line)
	(newline)
	(insert headline)))))

(defun org-headline (level name)
  (let ((prefix (s-repeat (+ level 1) "*")))
    (s-concat prefix " " name)))

(defun org-find-or-create-olp (olp)
  (when (eq major-mode 'org-mode)
    (let ((headlines (-map-indexed #'org-headline olp)))
      (goto-char (point-min))
      (-each headlines  #'org-find-or-create-headline)
      (org-end-of-subtree))))

(defun org-find-or-create-category-headline (category)
  "Find or create a given category headline"
  (org-find-or-create-headline (org-headline 0 (s-capitalize category)))
  (org-set-category category)
  (org-end-of-subtree))

(defun org-set-category (category)
  (unless (string= category (org-entry-get-with-inheritance "CATEGORY"))
    (org-set-property "CATEGORY" category)))

(defun org-current-year-month-day ()
  (format-time-string "<%Y-%m-%d>" (current-time)))

;; header/mode line
(defun hm-line/format-command (key name)
  ;; leading 'M-' prefix is represented as 'ESC'
  (let ((key (s-replace-regexp "^ESC " "M-" key)))
    (list (concat name ": '")
	  `(:propertize ,key
			face help-key-binding
			font-lock-face help-key-binding)
	  "'")))

(defun hm-line/format-keymap-binding (key action)
  (pcase action
    (`(,name . ,_) (hm-line/format-command key name))))

(defun hm-line/merge-key (prefix key)
  (let ((suffix (single-key-description key)))
    (if prefix
	(concat prefix " " suffix)
      suffix)))

(defun hm-line/for-each-in-keymap (acc keymap f &optional prefix)
  "f returns a list of elements"
  (map-keymap (lambda (key action)
		(let ((key (hm-line/merge-key prefix key)))
		  (if (keymapp action)
		      (hm-line/for-each-in-keymap acc action f key)
		    (add-to-list acc (funcall f key action))
		    )))
	      keymap)
    acc)

(defun hm-line-format-keymap (keymap)
  (let ((properties ()))
    (hm-line/for-each-in-keymap 'properties keymap #'hm-line/format-keymap-binding)
    (-interpose " " properties)))

;; keymaps
(defun workspace/define-keymap (&rest bindings)
  (let ((keymap (make-sparse-keymap)))
    (-each bindings (lambda (binding) (workspace/define-key keymap binding)))
    keymap))

(defun workspace/extend-keymap (parent &rest bindings)
  (when (keymapp parent)
    (let ((keymap (apply #'workspace/define-keymap bindings)))
      (set-keymap-parent keymap parent)
      keymap)))

(defun workspace/define-key (keymap binding)
  (let ((key (nth 0 binding))
	(name (nth 1 binding))
	(command (workspace/normalize-keymap-command (nth 2 binding))))
    (define-key keymap (kbd key) (cons name command))))

(defun workspace/normalize-keymap-command (action)
  (cond ((commandp  action) action)
	((keymapp   action) action)
	((functionp action) `(lambda () (interactive) (,action)))
	((listp     action) `(lambda () (interactive) ,action))
	(_ (user-error "Unsupported command of type %s: %s" (type-of action) action))))

;; workspace
(defvar workspace/root    "~/workspace")
(defvar workspace/current "mzarnowski")
(defvar workspace/project nil)

(defvar workspace/current-workspace nil)
(defvar workspace/current-project nil)

(defvar workspace-buffer--project-path nil)

(defun workspace/path (project-path name)
  (apply #'f-join
	 (-non-nil (list (workspace/project-path-root-path project-path)
			 (workspace/project-path-workspace-name project-path)
			 (workspace/project-path-project-name project-path)
			 name))))

(defun workspace/current-file (project-path directory)
  (let* ((file-name (format-time-string "%Y" (current-time)))
	 (file-path (format "%s/%s.org.gpg" directory file-name)))
    (workspace/path project-path file-path)))

(defun workspace/projects-dir ()
  (f-join workspace/root workspace/current "projects"))

;;; workspace-buffer-local-entry

(defun workspace-buffer--path (name)
  (workspace/path workspace-buffer--project-path name))

(defun workspace-buffer--latest-file-in (directory)
  (workspace/current-file workspace-buffer--project-path directory))

;; org-mode
(defun workspace/org-capture (template)
  (let ((org-capture-entry template))
    (org-capture)))

(defun workspace/org-agenda-in (&rest directories)
  (let ((org-agenda-files directories))
    (consult-org-agenda)))

;; agenda
(defun workspace/agenda-template (file)
  (car (doct `("Agenda" :keys "a" :file ,file :template "* TODO %?"))))

(defun workspace/agenda-capture (&optional project-name)
  (let* ((workspace workspace/current)
	 (project (workspace/find-project workspace project-name))
    (workspace--agenda-capture workspace project))))

(defun workspace--agenda-capture (workspace project)
  (let ((file (workspace/current-file workspace project "agenda")))
    (workspace/org-capture (workspace/agenda-template file))))


(defun workspace/agenda-open (&optional project)
  (workspace/org-agenda-in (workspace/path project "agenda")))

;; inbox
(defun workspace/schedule-thought-refinement ()
  (let ((date (org-current-year-month-day))
	(org-blank-before-new-entry nil))
    (goto-char (point-max)) ;; cannot be on the same line as the header
    (re-search-backward org-heading-regexp) ;; puts us on the beginning of the line
    (next-line)
    (insert (concat "SCHEDULED: " date))
    (newline 2)))

(defvar workspace/before-thought-capture #'workspace/schedule-thought-refinement)

(defun workspace/thought-template (file)
  "Captured note will automatically be scheduled for the same day"
  (car (doct `("Default"
	       :keys "d"
	       :type entry
	       :file ,file
	       :template "* TODO %?"
	       :prepare-finalize ,workspace/before-thought-capture))))

;; journal

(defun workspace/create-journal-entry-headline ()
  (org-find-or-create-olp (-map (lambda (p) (format-time-string p (current-time)))
				'("%Y" "%Y-%m" "%Y-%m-%d"))))

(defun workspace/journal-template (file)
  (car (doct `("Journal"
	       :keys "j"
	       :file ,file
	       :function workspace/create-journal-entry-headline
	       :template "* %<%H:%M> %?"))))

(defun workspace/journal-capture (workspace &optional project)
  (let* ((file (workspace/current-file workspace project "journal")))
    (workspace/org-capture (workspace/journal-template file))))

(defun workspace/journal-open (&optional project)
  (find-file (workspace/current-journal-file)))    



;; TODO transient per-project menu opened on the right
;;  with ability to switch-to-project.
;; By default, it reads the current-buffer's workspace/project value

(cl-defstruct (workspace/workspace) name projects)
(cl-defstruct (workspace/project) name)
(cl-defstruct (workspace/project-path) root-path workspace-name project-name)

(defun workspace/string->project-path (value)
  (let ((segments (s-split-up-to "/" value 1)))
    (make-workspace/project-path :root-path workspace/root
				 :workspace-name (car segments)
				 :project-name (cadr segments))))

(defun workspace/find-project (workspace name)
  (-find (lambda (it) (string= name (workspace/project-name it)))
	 (workspace/workspace-projects workspace)))

(defun workspace/select-project ()
  (completing-read "Project:" (-map #'f-filename (f-directories (workspace/projects-dir)))))

(setq workspace/current (make-workspace/workspace
			  :name "mzarnowski"
			  :projects (list
				     (make-workspace/project :name "spex"))))

;;  frame / window parameter holds the latest workspace / project used
;; there are four commands used to change those values:
;; (workspace-{frame,window}-switch-{workspace,project})
;;  there is nothing like that for buffers, because those
;; use workspace-owner buffer-local variable. Content could potentially be moved to
;; a different project, but that depents on the content type, so let's ignore it for now

;;  now, calling (workspace-thought-capture) uses the
;; workspace / project found in buffer or window or frame parameter  

(defun workspace/projects (&optional workspace)
  (let ((workspace (if workspace workspace workspace/current)))
    (mapcar #'workspace/project-name (workspace/workspace-projects workspace))))

(defun workspace/read- (prompt initial-input history)
  (completing-read prompt (workspace/projects) nil nil initial-input history))

(transient-define-argument workspace-menu/change-project ()
  :class 'transient-option
  :key "p"
  :description "project"
  :argument "project")

(defun workspace-menu/current-project ()
  (if workspace/project
      (format ("Project: %s" workspace/project))
    "Workspace"))


;; agenda contains everything I want to or have to work on scheduled at some day
;; issues/tickets contain a description of same, but not scheduled
;;   only prioritization is eisenhover box

;; TODO (e)xplore or (f)ind? e.g. agenda, thoughts
;;   e seems more versatile
;; also, (e)xplore (r)andom issue/thought/routine seems like a nice prefix

;; ca - capture agenda item
;; fa - explore agenda
;; ci - capture issue using this workflow: https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
;; fi - explore issues?
;; ct - capture raw thought
;; ft - find thought
;; cj - capture journal entry
;; fj - find journal entry
;; h  - help prefix
;; hr - show described routines (e.g. how to take notes, or create sourdough
;; cr - capture a routine (like org-roam, first, specify a name. Then, edit the routine)
;; p  - switch project

(cl-defstruct workspace-menu-row    title columns)
(cl-defstruct workspace-menu-column title commands)

(defun workspace-issues--menu-column (workspace project)
  (make-workspace-menu-column :title "issues"
			      :commands
			      `(("c" "capture" (user-error "Not implemented")))))

(defun workspace-journal--menu-column (workspace project)
  (make-workspace-menu-column :title "journal"
			      :commands
			      `(("c" "capture" (workspace/journal-capture workspace project)))))

(defun workspace-thoughts--menu-column (workspace project)
  (make-workspace-menu-column :title "thoughts"
			      :commands
			      `(("c" "capture" (workspace-thought--capture workspace project))
				("r" "refine" (workspace-thought--browse-unrefined workspace project)))))

(defun workspace-notes--menu-column (workspace project)
  (make-workspace-menu-column :title "notes"
			      :commands
			      `("f" "find" (workspace-notes--find workspace project))))

(defun workspace-menu--transient-layout (workspace)
  (let ((namespace (concat "WORKSPACE:" (workspace/workspace-name workspace))))
    (list (workspace-menu--row namespace ))))

(defun workspace-menu--workspace-transient-prefix (workspace)
  (let ((symbol (intern (format "workspace-%s-menu" (workspace/workspace-name workspace)))))
    (defalias symbol `(lambda () (interactive) (transient-setup ',symbol)))
    (put symbol 'interactive-only t)
;;    (put symbol 'transient--prefix (transient-prefix :command 'which-key-show-full-keymap))
    (put symbol 'transient--layout foo-bar)
    symbol))

(defun workspace-menu--workspace->transient-layout (workspace)
  (let ((workspace-name (workspace/workspace-name workspace)))
    (workspace--transient-menu
     workspace-name
     `("Workspace: mzarnowski"
       ,(workspace-issues--transient-menu-column workspace)
       ,(workspace-journal--transient-menu-column workspace)
       ,(workspace-notes--transient-menu-column workspace)
       ,(workspace-thoughts--transient-menu-column workspace))
     `("projects"
       (("spex" "ps" (workspace-menu ,workspace "spex")))))))

(defun workspace-menu--make-segment (name)
  (replace-regexp-in-string "[ \t\n\r]+" "-" name))

(defun workspace-menu--make-namespace (&rest segments)
  (s-join ":" (mapcar #'workspace-menu--make-segment segments)))

(defun workspace-menu--make-symbol (namespace form)
  (let ((name (format "%s:%d" namespace (sxhash command))))
    (intern name)))

(defun workspace-menu--command->transient-suffix (namespace key name action)
  (let* ((command (workspace-menu--action->command action))
	 (symbol  (workspace-menu--bind-command namespace command)))
    `(1 transient-suffix ,(list :key key :description name :command symbol))))

(defun workspace-menu--transient-column (namespace commands)
  (let ((namespace (format "%s:%s" namespace title)))
    (-map (lambda (it) (let ((key    (aref it 0))
			     (name   (aref it 1))
			     (action (aref it 2)))
			 (workspace-menu--command->transient-suffix namespace key name action)))
	    commands)))

(defun workspace-menu--transient-row (namespace columns)
  "[\"title\" (a b c)]"
  (let ((columns (mapcar (lambda (it) (workspace-menu--transient-column namespace it)) columns)))
    columns))				;

(defun workspace--transient-menu (title &rest rows)
  (let* ((namespace (workspace-menu--make-namespace "WORKSPACE" title))
	 (rows (mapcar (lambda (it) (workspace-menu--transient-row namespace it)) rows)))
    (vector rows)))

(defun workspace-menu--action->command (command)
  (cond
   ((commandp  command) command)
   ((functionp command) `(lambda () (interactive) (funcall ,command)))
   ((listp     command) `(lambda () (interactive) ,command))
   (_ (user-error "not a command, function or list: %s" command))))

(defun workspace-menu--bind-command (namespace command)
  (cond
   ((symbolp   command) command)
   ((functionp command)
    (let* ((symbol (workspace-menu--make-symbol namespace command)))
      (defalias symbol command)))
   (_ (user-error "not a symbol or command form: %s" command))))

(defun workspace-menu-item (namespace key name action)
  (let* ((command (workspace-menu--action->command action))
	 (symbol  (workspace-menu--bind-command namespace command)))
    `(1 transient-suffix ,(list :key key
			       :description name
			       :command (workspace-menu--bind-command namespace command)))))

(defun workspace-keymap->bindings (namespace keymap)
  (let ((rows ())
	(f (lambda (key action)
	     (pcase action
	       (`(,name . ,action) (workspace-menu-item namespace key name action))
	       (_ (user-error "Command without description: %s" action))))))
    (hm-line/for-each-in-keymap 'rows keymap f)
    rows))

(defun workspace-menu-column (namespace keymap)
  (let ((bindings (workspace-keymap->bindings namespace keymap)))
    bindings))

;; menu

(defface workspace-menu-heading
  '((t (:weight bold)))
  "Face used for headings in Helpful buffers.")

(defun workspace-menu--heading (text)
  "Propertize TEXT as a heading."
  (format "%s\n" (propertize text 'face 'helpful-heading)))

;; custom thought capture
(defun workspace-thought--capture (project-path-string)
  (let ((project-path (workspace/string->project-path project-path-string))
	(buffer (generate-new-buffer "*thought*")))
    (with-current-buffer buffer
      (org-mode)
      (insert "* ")
      ;; must be set AFTER entering org-mode
      (setq-local workspace-buffer--project-path project-path)
      (workspace/thought-capture-mode 1)
      
      (select-window (display-buffer buffer)))))

(defun workspace/thought-capture-finalize ()
  (interactive)
  (let* ((file (workspace-buffer--latest-file-in "thoughts/raw"))
	 (provider (current-buffer))
	 (receiver (find-file-noselect file)))
    ;; make the entry a TODO
    (goto-char (point-min))
    (org-todo "TODO")
    
    ;; schedule refinement, unless already scheduled by the author
    (unless (org-entry-get nil "SCHEDULED")
      (org-entry-put nil "SCHEDULED" (org-current-year-month-day)))
    
    ;; copy the content into the target buffer
    (with-current-buffer receiver
      ;; org-id-get-create requires the file to already exist...
      (unless (f-exists? file)
	(save-buffer))
      
      (save-excursion
	(goto-char (point-max))
	(unless (looking-at-p "^$")
	  (newline))
	
	(insert-buffer provider)
	;; assign an ID
	(org-id-get-create))
      (save-buffer))

    ;; finally, kill the capture buffer
    (kill-buffer)))

(defun workspace/thought-capture-discard ()
  (interactive)
  (when (or (string= "* " (buffer-string))
	    (y-or-n-p "Discard this thought?"))
    (kill-buffer)))

(define-minor-mode workspace/thought-capture-mode
  "Disables all keys."
  :lighter " th-cap"
  :keymap (workspace/define-keymap
	   `("C-c C-c" "finalize" workspace/thought-capture-finalize)
	   `("C-c C-k" "discard"  workspace/thought-capture-discard))
  (setq-local header-line-format (hm-line-format-keymap workspace/thought-capture-mode-map)))
 
;; thought refine
(require 'org-ql-search)

(defvar workspace/org-file-regexp "\\`[^.].*\\.org\\(\\.gpg\\)?\\'"
  "TODO")

(defun workspace/thoughts-all-files (project)
  (org-ql-search-directories-files
   :directories (list (workspace/path project "thoughts"))
   :recurse     t
   :regexp      workspace/org-file-regexp))

(defun workspace/thought-browse-unrefined (&optional project)
  (let ((files (workspace/thoughts-all-files project))
	(org-ql-view-map (workspace/extend-keymap
			  org-ql-view-map
			  `("RET" "refine" (workspace/thought-refine ,project)))))
    (org-ql-search files '(todo) :title "Unrefined thoughts")))

(defun workspace/thought-refine (project)
  "Begin refinement of the selected entry"
  ;; TODO - open in separate window, like in org-src?
  ;; otherwise, only one thought can be refined at a time
  ;; ... which might be a good thing
  ;; otherwise, we could create a dedicated buffer named
  ;; after the thought being refined
  (org-agenda-switch-to)
  (org-narrow-to-subtree)

  (setq-local workspace/project project) ;; must be set after entering org-mode
  (workspace/thought-refine-mode 1))

(defun workspace/thought-refine-close ()
  (interactive)

  (setq header-line-format nil)

  (widen)
  (bury-buffer))

(define-minor-mode workspace/thought-refine-mode
  "Disables all keys."
  :lighter " th-ref"
  :keymap (workspace/define-keymap
	   `("C-c C-n" "derive"   (workspace/note-derive))
	   `("C-c C-c" "finalize" (message "fin")))
  (setq-local header-line-format (hm-line-format-keymap workspace/thought-refine-mode-map)))

;; notes
;;; a note is derived from a thought
(defun workspace/note-derive ()
  (let ((buffer  (generate-new-buffer "*derive-note*"))
	(project workspace/project))
    (let ((capture-buffer (generate-new-buffer "*derived-note*")))
      (setq-local workspace/project project)
      (switch-to-buffer-other-window capture-buffer)
      (org-mode)
      (workspace/note-derive-mode 1))))

(define-minor-mode workspace/note-derive-mode
  "Minor mode for special key bindings in a derive-note buffer."
  :lighter     " Der"
  :keymap (workspace/define-keymap)
  (setq-local header-line-format (hm-line-format-keymap workspace/note-derive-mode-map)))

;; TODO defvar
;; (setq workspace/note-derive-mode-map
;;   (workspace/make-menu
;;    `("C-c n i" "link to:" org-roam-node-insert)
;;    `("C-c C-c" "finalize" workspace/note-derive-finalize)
;;    `("C-c C-k" "cancel" workspace/note-derive-cancel)))

(defun workspace/note-link-to ()
  "Insert linkt o a note (or thought) at point"
  (interactive)
  (let ((files (workspace/thoughts-all-files workspace/project)))
    (let* ((results (org-ql-select files '(property "ID")
		  :action 'element-with-markers
		  :narrow nil
		  :sort   nil))
	   (strings (-map #'org-ql-view--format-element results))
	   (foo (s-join "\n" strings))
	   (buffer (current-buffer)))
      (with-temp-buffer
	(strings)
	(local-set-key (kbd "RET") (lambda ()
				      (interactive)
				      (let ((id (org-get-at-bol 'ID)))
					(kill-buffer)
					(switch-to-buffer buffer)
					(insert (concat "--" id "--"))))))
    )))

(defun workspace/note-derive-finalize ()
  "Insert currently derived note into the slipbox"
  (interactive)
  (ignore))

(defun workspace/note-derive-cancel ()
  "Discard current note"
  (interactive)
  (when (or (not (= (point-min) (point-max)))
	    (y-or-n-p "Note is not empty. Cancel anyway?"))
    (ignore)))

