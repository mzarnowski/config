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

(cl-defun workspace-thoughts--explore-get-buffer-create (workspace mode)
  (let ((buffer-name (format "%s: thoughts" (workspace-name workspace))))
    (get-buffer-create buffer-name)))

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

(cl-defun workspace-thoughts--explore-buffer-act-on-f (workspace mode)
  (lambda ()
    (interactive)
    (let ((marker (get-text-property 0 :org-marker (tabulated-list-get-id))))
      (with-current-buffer (marker-buffer marker)
	(switch-to-buffer (current-buffer))))))

(cl-defun workspace-thoughts--explore-buffer-refresh-f (workspace mode)
  (lambda () (workspace-thoughts--refresh-explore-buffer workspace mode)))

(cl-defun workspace-thoughts--explore-buffer (workspace mode)
  (let* ((buffer (workspace-thoughts--explore-get-buffer-create workspace mode))
	 (header (workspace-thoughts--explore-buffer-headers    workspace mode)))
    (with-current-buffer buffer
      (tabulated-list-mode)
      (setq-local header-line-format header
		  tabulated-list-padding  2
		  tabulated-list-sort-key '("Created" . t))

      (setq tabulated-list-format header)
      (tabulated-list-init-header)

      (local-set-key (kbd "RET") (workspace-thoughts--explore-buffer-act-on-f workspace mode))

      (add-hook 'tabulated-list-revert-hook
		(workspace-thoughts--explore-buffer-refresh-f workspace mode)
		nil t)
      (revert-buffer))
    buffer))

(cl-defun workspace-thoughts--explore (workspace mode)
  (let* ((thoughts (workspace-thoughts--find-thoughts workspace mode :query '(todo)))
	 (strings (-map #'org-ql-view--format-element thoughttabulated-list-modes)))
    (org-ql-view--display
      :buffer "*foo*"
      :header "my header"
      :string (s-join "\n" strings))))

