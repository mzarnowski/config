;;; -*- lexical-binding: t; -*-

(require 'workspace)
(require 'workspace-thought-mode)

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
    (org-ql-select files query)))

(cl-defun workspace-thoughts--explore (workspace mode)
  (let ((thoughts (workspace-thoughts--find-thoughts workspace mode :query '(todo))))
    thoughts))
