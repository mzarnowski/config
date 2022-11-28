;;; -*- lexical-binding: t; -*-

(require 'workspace)

(cl-defun workspace-find-frame (workspace)
  (let ((expected-name (workspace-name workspace)))
    (-find (lambda (it) (workspace-compare workspace (frame-parameter it 'current-workspace)))
	   (frame-list))))

(cl-defun workspace-make-frame (workspace)
  (make-frame `((name . ,(format "Workspace: %s" (workspace-name workspace)))
		(current-workspace . ,workspace))))

(cl-defun workspace-switch-to-frame (workspace)
  (let ((old-frame (selected-frame))
	(new-frame (or (workspace-find-frame workspace)
		       (workspace-make-frame workspace))))
    (unless (equal old-frame new-frame)
      (select-frame-set-input-focus new-frame)
      (make-frame-invisible old-frame))))

(provide 'workspace-frame)
