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
	(t (user-error "Unsupported command of type %s: %s" (type-of action) action))))

;; workspace
(defvar workspace/root    "~/workspace")
(defvar workspace/current "mzarnowski")

(defun workspace/path (project name)
  (if project
      (f-join (workspace/projects-dir) project name)
    (f-join workspace/root workspace/current name)))

(defun workspace/current-file (project directory)
  (let ((filename (format-time-string "%Y" (current-time))))
    (workspace/path project (format "%s/%s.org.gpg" directory filename))))

(defun workspace/projects-dir ()
  (f-join workspace/root workspace/current "projects"))

(defun workspace/org-capture (template)
  (let ((org-capture-entry template))
    (org-capture)))

(defun workspace/org-agenda-in (&rest directories)
  (let ((org-agenda-files directories))
    (consult-org-agenda)))

;; agenda
(defun workspace/agenda-template (file)
  (car (doct `("Agenda" :keys "a" :file ,file :template "* TODO %?"))))

(defun workspace/agenda-capture (&optional project)
  (let* ((file (workspace/current-file project "agenda")))
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

(defun workspace/journal-capture (&optional project)
  (let* ((file (workspace/current-file project "journal")))
    (workspace/org-capture (workspace/journal-template file))))

(defun workspace/journal-open (&optional project)
  (find-file (workspace/current-journal-file)))    

;;; menu
(defun workspace/menu-target (target)
  (cond ((commandp  target) target)
	((keymapp   target) target)
	((functionp target) `(lambda () (interactive) (,target)))
	((listp     target) `(lambda () (interactive) ,target))))

(defun workspace/menu-bind (keys binding)
  (let ((key    (nth 0 binding))
	(name   (nth 1 binding))
	(target (nth 2 binding)))
    (define-key keys (kbd key) (cons name (workspace/menu-target target)))))

(defun workspace/make-menu (&rest bindings)
  (let ((keys (make-sparse-keymap)))
    (-each bindings (lambda (it) (workspace/menu-bind keys it)))
    keys))

(defun workspace/agenda-menu (project)
  (workspace/define-keymap
   `("a"   "new entry"   (workspace-agenda-capture ,project))
   `("RET" "open agenda" (workspace-agenda-open    ,project))))

(defun workspace/journal-menu (project)
  (workspace/define-keymap
   `("j" "new entry" (workspace/journal-capture ,project))))

(defun workspace/thoughts-menu (project)
  (workspace/make-menu
   `("t" "capture thought" (workspace/thought-capture ,project))
   `("r" "refine thoughts" (workspace/thought-browse-unrefined  ,project))
   `("RET" "open thought"  (workspace/thoughts-open   ,project))))

(defun workspace/select-project ()
  (completing-read "Project:" (-map #'f-filename (f-directories (workspace/projects-dir)))))

(defun workspace-menu ()
  (workspace/make-menu
   `("C-`" "in project" (workspace/select-project))))

;; spex
(defun spex-make-keymap ()
  (workspace/make-menu
   `("a" "agenda"   ,(workspace/agenda-menu "spex"))
   `("j" "journal"  ,(workspace/journal-menu "spex"))
   `("t" "thoughts" ,(workspace/thoughts-menu "spex"))))

(global-set-key (kbd "C-c n") (spex-make-keymap))


;; TODO transient per-project menu opened on the right
;;  with ability to switch-to-project.
;; By default, it reads the current-buffer's workspace/project value


(defvar workspace/project nil)
(transient-define-argument workspace-menu/project ()
  "Currently selected project"
  :description "Current project"
  :class 'transient-lisp-variable
  :variable 'workspace/project)

(transient-define-prefix workspace-menu () ["foo"]
  )


;; custom thought capture
(defun workspace/thought-capture (&optional project)
  (let ((buffer (generate-new-buffer "*thought*")))
    (with-current-buffer buffer
      (org-mode)
      (insert "* ")

      (setq-local workspace/project project) ;; must be set AFTER entering org-mode
      (workspace/thought-capture-mode 1)
      
      (select-window (display-buffer buffer)))))

(defun workspace/thought-capture-finalize ()
  (interactive)
  (let* ((file (workspace/current-file workspace/project "thoughts/raw"))
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
      (save-excursion
	(goto-char (point-max))
	(unless (looking-at-p "^$"):
	  (newline))
	(insert-buffer provider)
	;; assign an ID
	(org-id-get-create)))

    ;; finally, kill the capture buffer
    (kill-buffer)

    ;; open then receiver buffer
    ;; TODO probably not needed
    (select-window (display-buffer receiver))))

(defun workspace/thought-capture-discard ()
  (interactive)
  (kill-buffer))

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
  (interactive)
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
(setq workspace/note-derive-mode-map
  (workspace/make-menu
   `("C-c n i" "link to:" org-roam-node-insert)
   `("C-c C-c" "finalize" workspace/note-derive-finalize)
   `("C-c C-k" "cancel" workspace/note-derive-cancel)))

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

