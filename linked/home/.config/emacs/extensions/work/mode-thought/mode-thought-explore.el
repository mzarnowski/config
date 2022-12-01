;;; -*- lexical-binding: t; -*-

(require 'workspace)
;;(require 'workspace-thought-mode)

(require 'org-ql-search)


(cl-defun workspace-thoughts--file-regexp (workspace mode)
  ""
  ;; TODO
  "\\`[^.].*\\.org\\(\\.gpg\\)?\\'")

(cl-defun workspace-thoughts--files (workspace mode)
  (let ((workspace-path (f-join (workspace-path workspace) "thoughts"))
	(file-regexp    (workspace-thoughts--file-regexp workspace mode)))
    (org-ql-search-directories-files
     :directories (list workspace-path)
     :regexp file-regexp
     :recurse t )))

(cl-defun workspace-thoughts--find-thoughts (workspace mode &key query)
  (let ((files (workspace-thoughts--files workspace mode)))
    (org-ql-select files query :action 'element-with-markers)))

(cl-defun workspace-thoughts--refresh-explore-buffer (workspace mode)
  (setq-local tabulated-list-entries (workspace-thoughts--explore-buffer-entries workspace mode))
  (tabulated-list-print))

(cl-defun workspace-thoughts--explore-buffer-headers (workspace mode)
  [("Created" 10 t)
   ("Status" 6 t)
   ("Title" 1 t)])

(cl-defun workspace-thoughts--explore-buffer-title (plist))

(cl-defun workspace-thoughts--explore-buffer-entry (plist)
  (let ((id      (plist-get plist ':ID))
	(title   (plist-get plist ':raw-value))
	(status  (plist-get plist ':todo-keyword))
	(created (plist-get plist ':scheduled))
	(marker  (plist-get plist ':org-marker)))
    (let ((created (let* ((org-timestamp created)
			  (time (org-timestamp-to-time org-timestamp)))
		     (format-time-string "%Y-%m-%d" time))))
      (when (and id marker)
	(list (propertize id :org-marker marker)
	      (vector created status title))))))

(cl-defun workspace-thoughts--explore-buffer-entries (workspace mode)
  "Adapts thoughts (org-mode headlines) to tabulated list

The format for the list element is defined by 'tabulated-list-entries':
(ID [string ... string])"
  (cl-labels ((f (thought) (workspace-thoughts--explore-buffer-entry (cadr thought))))
    (let ((thoughts (workspace-thoughts--find-thoughts workspace mode :query '(todo))))
      (-non-nil (-map #'f thoughts)))))

(cl-defun workspace-thoughts--explore-buffer-act ()
  (interactive)

  (unless (and current-workspace current-workmode)
    (error "thoughts-explore buffer not initialized"))

  (let ((marker (get-text-property 0 :org-marker (tabulated-list-get-id))))
    (with-current-buffer (marker-buffer marker)
      (switch-to-buffer (current-buffer)))))

(cl-defun workspace-thoughts--explore-buffer-refresh ()
  (unless (and current-workspace current-workmode)
    (error "thoughts-explore buffer not initialized"))

  (workspace-thoughts--refresh-explore-buffer current-workspace current-workmode))

(defvar workspace-thoughts-explore-mode-map
  (workspace-define-keymap
   `("RET" "refine" workspace-thoughts--explore-buffer-act)))

(define-derived-mode workspace-thoughts-explore-mode tabulated-list-mode "Thoughts"
  (add-hook 'tabulated-list-revert-hook
	    #'workspace-thoughts--explore-buffer-refresh
	    nil t)

  (setq-local tabulated-list-padding  2
	      tabulated-list-sort-key '("Created" . t)))

(cl-defun workspace-thoughts-explore-mode-p ()
  (equal 'workspace-thoughts-explore-mode major-mode))

(cl-defun workspace-thoughts-explore-mode-initialize-buffer (workspace mode &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (unless (workspace-thoughts-explore-mode-p)
      (error "not in workspace-thoughts-explore-mode"))

    (setq-local current-workspace workspace
		current-workmode  mode)

    (let ((header (workspace-thoughts--explore-buffer-headers workspace mode)))
      (setq tabulated-list-format header)
      (tabulated-list-init-header))))

;; TODO should be idempotent?
(cl-defun workspace-thoughts--explore-make-buffer (workspace mode buffer-name)
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (workspace-thoughts-explore-mode)
      (workspace-thoughts-explore-mode-initialize-buffer workspace mode))
    buffer))

(cl-defun workspace-thoughts--explore-get-buffer-create (workspace mode)
  (let* ((buffer-name (format "%s: thoughts" (workspace-name workspace)))
	 (buffer (get-buffer buffer-name)))
    (or buffer
	(workspace-thoughts--explore-make-buffer workspace mode buffer-name))))

(cl-defun workspace-thoughts--explore (workspace mode)
  (let* ((buffer (workspace-thoughts--explore-get-buffer-create workspace mode)))
    (with-current-buffer buffer
      (revert-buffer))

    (switch-to-buffer buffer)))
