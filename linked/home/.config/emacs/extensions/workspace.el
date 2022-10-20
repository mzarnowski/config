;; org-related extensions
(defun org-find-or-create-headline (headline &optional reposition)
  "Find or create a given headline"
  (when (eq major-mode 'org-mode)
    (save-match-data
      (when reposition (goto-char (point-min)))
      (unless (re-search-forward  (concat "*" headline "$") nil 'no-error)
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

;; workspace

(defvar workspace/root    "~/workspace")
(defvar workspace/current "mzarnowski")

(defun workspace/file (name)
  (f-join workspace/root workspace/current name))

;; journal

(defun workspace/create-journal-entry-headline ()
  (org-find-or-create-olp (-map (lambda (p) (format-time-string p (current-time)))
				'("%Y" "%Y-%m" "%Y-%m-%d"))))

(defun workspace/current-journal-file ()
  (let ((filename (format-time-string "%Y" (current-time))))
    (workspace/file (format "journal/%s.org.gpg" filename))))


(defun workspace/capture-journal ()
  (let* ((file     (workspace/current-journal-file))
	 (org-capture-templates (doct `("Journal"
					:keys "j"
					:file ,file
					:function workspace/create-journal-entry-headline
					:template "* %<%H:%M> %?"))))
      (org-capture nil "j")))

(defun workspace/open-journal ()
  (find-file (workspace/current-journal-file)))
    

;; workspace menu

(transient-define-prefix workspace-journal-menu ()
  ["Actions"
   ("j" "create" (lambda () (interactive) (workspace/capture-journal)))
   ("RET" "open" (lambda () (interactive) (workspace/open-journal)))
   ("q" "quit" transient-quit-one)])

(transient-define-prefix workspace-transient-menu ()
  ["Actions"
   ("j" "journal" workspace-journal-menu)
   ("q" "quit" transient-quit-one)])

