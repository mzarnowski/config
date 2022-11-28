;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'f)

(defvar workspace-root "~/workspace")
(cl-defstruct workspace name)

(cl-defun workspace-path (workspace)
  (f-join workspace-root (workspace-name workspace)))

(cl-defun workspace-current-path (workspace directory &key (extension "org") (encrypt t))
  (let ((directory (f-join (workspace-path workspace) directory))
	(file-name (format-time-string "%Y" (current-time)))
	(extension (if encrypt (concat extension ".gpg") extension)))
    (f-join directory (format "%s.%s" file-name extension))))

(cl-defun workspace-compare (f1 f2)
  (and f1 f2 (equal (workspace-name f1) (workspace-name f2))))

(provide 'workspace)

