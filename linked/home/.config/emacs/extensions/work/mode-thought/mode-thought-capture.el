(require 'cl-lib)
(require 'workspace-thought-mode)

(cl-defstruct workflow-step
  name emacs-major-mode next-steps)

;; next-step action does something with the current buffer (e.g. (f workspace mode buffer))
(make-workflow-step :name "foo"
		    :emacs-major-mode "foo"
		    :next-steps `((:name "discard" :action lambda)
				  (:name "commit"  :action lambda)))

(defun workmode-thought-capture ()
  (interactive))

(defun workmode-thoughts--capture-thought (workspace mode)
  (with-current-buffer (generate-new-buffer "*new-thought*")
    (org-mode)
    (insert "* ")

    (let ((display-buffer-alist '((t . display-buffer-reuse-window))))
      (select-window (display-buffer (current-buffer))))))

(defun workmode-thoughts--finalize-thought-capture (workspace mode)
  ;; make the entry a TODO
  (goto-char (point-min))
  (org-todo "TODO")

  ;; schedule refinement, unless already scheduled by the author
  (unless (org-entry-get nil "SCHEDULED")
    (org-entry-put nil "SCHEDULED" (org-current-year-month-day))))

(defun workmode-thoughts--insert-thought (workspace mmode)
  (let ((source-buffer (current-buffer))
	(file (workspace-current-path workspace "thoughts/raw")))
    (with-current-buffer (find-file-noselect file)
      ;; org-id-get-create requires the file to already exist...
      (unless (f-exists? file)
	(save-buffer))

      (save-excursion
	(goto-char (point-max))
	;; ensure we will insert into an empty line
	(unless (looking-at-p "^$")
	  (newline))

	(insert-buffer source-buffer)
	;; assign an ID
	(org-id-get-create))
      (save-buffer))))

(defun workmode-thoughts--commit-thought (workspace mode)
  (workmode-thoughts--finalize-thought-capture workspace mode)
  (workmode-thoughts--insert-thought workspace mode)
  ;; finally, kill the capture buffer
  (kill-buffer))

(defun workmode-thoughts--discard-thought (workspace mode)
  (when (or (string= "* " (buffer-string))
	    (y-or-n-p "Discard this thought?"))
    (kill-buffer)))

(provide 'workmode-thought-capture)
