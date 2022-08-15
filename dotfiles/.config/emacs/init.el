;; Move customization variables to a separate file
(setq custom-file (locate-user-emacs-file "custom-variables.el"))
;; And then load those variables
(load custom-file 'noerror 'nomessage)

(org-babel-load-file "~/.config/emacs/config.org")

(defun init/edit-init-config ()
  "Edit init config"
  (interactive)
  (find-file "~/.config/emacs/config.org"))

(global-set-key (kbd "C-z e") #'init/edit-init-config)
