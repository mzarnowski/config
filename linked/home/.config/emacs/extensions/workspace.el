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

(defun workspace/thought-capture (&optional project)
  (let* ((file (workspace/current-file project "thoughts/raw")))
    (workspace/org-capture (workspace/thought-template file))))

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
  (workspace/make-menu
   `("a"   "new entry"   (workspace-agenda-capture ,project))
   `("RET" "open agenda" (workspace-agenda-open    ,project))))

(defun workspace/journal-menu (project)
  (workspace/make-menu
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

(global-set-key (kbd "C-`") (spex-make-keymap))

;; custom thought capture
(defun workspace/thought-capture (&optional project)
  (let ((buffer (generate-new-buffer "*thought*")))
    (with-current-buffer buffer
      (org-mode)
      (workspace/thought-capture-mode 1)
      (set (make-local-variable 'workspace/project) project)
      (insert "* ")
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
      (end-of-line)
      (newline)
      (insert (concat org-scheduled-string " " (org-current-year-month-day))))

    ;; copy the content into the target buffer
    (with-current-buffer receiver
      (save-excursion
	(goto-char (point-max))
	(unless (looking-at-p "^$"):
	  (newline))
	(insert-buffer provider)))

    ;; finally, kill the capture buffer
    (kill-buffer)

    ;; open then receiver buffer
    ;; TODO probably not needed
    (select-window (display-buffer receiver))))

(defun workspace/thought-capture-kill ()
  (interactive)
  (kill-buffer))

(defvar workspace/thought-capture-mode-map
  (workspace/make-menu
   `("C-c C-c" "finalize" workspace/thought-capture-finalize)
   `("C-c C-k" "kill"     workspace/thought-capture-kill))
  "Keymap for `workspace/thought-capture-mode', a minor mode.")


(define-minor-mode workspace/thought-capture-mode
  "Minor mode for special key bindings in a thought capture buffer."
  :global      nil
  :interactive nil
  :init-value  nil
  :lighter     " Cap"
  (if workspace/thought-refine-mode
      (let ((format (hm-line-format "Capture: " 'workspace/thought-capture-mode-map)))
	(setq-local header-line-format format))
    (progn (setq-local header-line-format nil))))

;; thought refine
(require 'org-ql-search)

(defvar workspace/org-file-regexp
  "\\`[^.].*\\.org\\(\\.gpg\\)?\\'"
  "TODO")

(defun workspace/thoughts-all-files (project)
  (org-ql-search-directories-files
   :directories (list (workspace/path project "thoughts"))
   :recurse     t
   :regexp      workspace/org-file-regexp))

(defun workspace/keymap-shadow (original &rest replacements)
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap org-ql-view-map)
    (-each replacements
      (lambda (it) (define-key keymap (kbd (car it)) (cdr it))))
    keymap))


(defun workspace/thought-refine-shadowed-keymap (original)
  (workspace/keymap-shadow original
			   `("RET" . workspace/thought-refine)))

(defun workspace/thought-browse-unrefined (&optional project)
  (let ((files (workspace/thoughts-all-files project))
	(org-ql-view-map (workspace/thought-refine-shadowed-keymap org-ql-view-map)))
    (org-ql-search files '(and (todo) (scheduled))
      :title "Refine thoughts")))


(defun workspace/thought-refine ()
  (interactive)
  "Begin refinement of the selected entry
(at point in the agenda view)"
  ;; TODO - open in separate window, like in org-src?
  ;; otherwise, only one thought can be refined at a time
  ;; ... which might be a good thing
  (org-agenda-switch-to)
  (org-narrow-to-subtree)
  (workspace/thought-refine-mode 1)
  (ignore)) ;; TODO: also, set the correct minor modes?

(defun workspace/thought-refine-close ()
  (interactive)
  (widen)
  (workspace/thought-refine-mode -1)
  (bury-buffer))

(defvar workspace/thought-refine-mode-map
  (workspace/make-menu
   `("C-c C-c" "new note" workspace/thought-refine-close)
   `("C-c C-k" "close" workspace/thought-refine-close))
  "Keymap for `workspace/thought-refine-mode', a minor mode.")

(define-minor-mode workspace/thought-refine-mode
  "Minor mode for special key bindings in a thought capture buffer."
  :global      nil
  :interactive nil
  :init-value  nil
  :lighter     " Ref"
  (if workspace/thought-refine-mode
      (let ((format (hm-line-format "Refine: " 'workspace/thought-refine-mode-map)))
	(setq-local header-line-format format))
    (progn (setq-local header-line-format nil))))

;; header/mode line
(defun hm-line/build-key-code (prefix key-code)
  (let ((key (single-key-description key-code)))
    (if prefix (concat prefix " " key) key)))

(defun hm-line/describe-key (prefix action)
    (pcase action
      ((pred keymapp) (hm-line/keymap-collect-keys action prefix))
      (`(,name . ,action) (format "%s `\\[%s]'" name (symbol-name action)))
      (_ (print prefix) (print action))))

(defun hm-line/keymap-collect-keys (keymap prefix)
  (let ((segments ()))
    (map-keymap (lambda (key-code action)
		  (let ((prefix (hm-line/build-key-code prefix key-code)))
		    (add-to-list 'segments (hm-line/describe-key prefix action))))
		keymap)
    segments))

(defun hm-line/template (header keymap-name keymap)
  (let ((segments (hm-line/keymap-collect-keys (eval keymap-symbol) nil)))
    (concat "\\<" keymap-name ">" header (s-join ", " (-flatten segments)))))
  

(defun hm-line-format (header keymap-symbol)
  (let ((keymap-name (symbol-name keymap-symbol))
	(keymap      (eval keymap-symbol)))
    (substitute-command-keys (hm-line/template header keymap-name keymap))))
