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

;; workspace

(defvar workspace/root    "~/workspace")
(defvar workspace/current "mzarnowski")

(defun workspace/path (name)
  (f-join workspace/root workspace/current name))

(defun workspace/current-file (directory)
  (let ((filename (format-time-string "%Y" (current-time))))
    (workspace/path (format "%s/%s.org.gpg" directory filename))))

;; agenda

(defun workspace/agenda-directory ()
  (workspace/path "agenda"))

(defun workspace/agenda-current-file ()
  (workspace/current-file "agenda"))

(defun workspace/agenda-template (header scheduled-on deadlined-on)
  (let ((properties (-non-nil `(,(when scheduled-on (concat "SCHEDULED: " scheduled-on))
                                ,(when deadlined-on (concat "DEADLINE: "  deadlined-on))))))
    (s-join "\n" (cons header properties))))

(transient-define-suffix workspace/capture-agenda (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (let* ((scheduled-on (when args (transient-arg-value "--since=" args)))
         (deadlined-on (when args (transient-arg-value "--deadline=" args)))
         (template (workspace/agenda-template "* TODO %?" scheduled-on deadlined-on))
         (file     (workspace/agenda-current-file)))
    (let ((org-capture-templates (doct `("Agenda" :keys "a" :file ,file :template ,template))))
      (org-capture nil "a"))))

(transient-define-suffix workspace/open-agenda (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (let ((org-agenda-files (list (workspace/agenda-directory))))
    (org-agenda)))

(transient-define-prefix workspace-agenda-menu ()
  ["Schedule"
   ("s" "since" "--since=" :reader (lambda (a b c) (org-read-date)))
   ("d" "deadline" "--deadline=" :reader (lambda (a b c) (org-read-date)))]
  ["Agenda actions"
   ("a" "show agenda" workspace/open-agenda)
   ("n" "new entry" workspace/capture-agenda)
   ("q" "quit" transient-quit-all)])

;; inbox
(defun workspace/schedule-thought-refinement ()
  (let ((date (format-time-string "<%Y-%m-%d>" (current-time)))
	(org-blank-before-new-entry nil))
    (goto-char (point-max)) ;; cannot be on the same line as the header
    (re-search-backward org-heading-regexp) ;; puts us on the beginning of the line
    (next-line)
    (insert (concat "SCHEDULED: " date))
    (newline 2)))

(defvar workspace/before-thought-capture #'workspace/schedule-thought-refinement)

(defun workspace/thought-template (file category)
  "Captured note will automatically be scheduled for the same day"
  (car (doct `("Default"
	       :keys "d"
	       :type entry
	       :file ,file
	       :function (lambda () (org-find-or-create-category-headline ,category))
	       :template "* TODO %?"
	       :prepare-finalize ,workspace/before-thought-capture))))

(transient-define-suffix workspace/capture-thought (category)
  (interactive (list (transient-args transient-current-command)))
  (let* ((file (workspace/current-file "thoughts/inbox"))
	 (org-capture-entry (workspace/thought-template file category)))
    (org-capture)))

(transient-define-prefix workspace-thought-capture-menu ()
  [[("d" "default" (lambda () (interactive) (workspace/capture-thought "default")))
    ("r" "recipe"  (lambda () (interactive) (workspace/capture-thought "recipe" )))]
   [("b" "book"    (lambda () (interactive) (workspace/capture-thought "book"   )))]
   [""
    ("q" "quit" transient-quit-all)]])

;; journal

(defun workspace/create-journal-entry-headline ()
  (org-find-or-create-olp (-map (lambda (p) (format-time-string p (current-time)))
				'("%Y" "%Y-%m" "%Y-%m-%d"))))

(defun workspace/current-journal-file ()
  (workspace/current-file "journal"))

(transient-define-suffix workspace/capture-journal (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (let* ((file     (workspace/current-journal-file))
	 (org-capture-templates (doct `("Journal"
					:keys "j"
					:file ,file
					:function workspace/create-journal-entry-headline
					:template "* %<%H:%M> %?"))))
      (org-capture nil "j")))

(transient-define-suffix  workspace/open-journal (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (find-file (workspace/current-journal-file)))    

(transient-define-prefix workspace-journal-menu ()
  ["Journal actions"
   ("j"   "create" workspace/capture-journal)
   ("RET" "open" workspace/open-journal)
   ("q"   "quit" transient-quit-one)])

;; workspace menu

(transient-define-prefix workspace-transient-menu ()
  ["Actions"
   ("a" "agenda" workspace-agenda-menu)
   ("j" "journal" workspace-journal-menu)
   ("q" "quit" transient-quit-one)])

