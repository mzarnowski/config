;; Move customization variables to a separate file
(setq custom-file (locate-user-emacs-file "custom-variables.el"))
;; And then load those variables
(load custom-file 'noerror 'nomessage)

(defun configure (name)
  (load (locate-user-emacs-file (concat "init/" name))))

(configure "package-manager")
(configure "hydra")   ;; chained key sequences

(configure "emacs-frame") 
(configure "emacs-window")
(configure "emacs-buffer")
(configure "fonts")
(configure "theme")

(configure "mode-line")
(configure "version-control")
(configure "backups")
(configure "term")
(configure "dired")
;; modes
(configure "mode-org")
(configure "mode-command-log") ; e.g. separate window with executed commands

;; extensions
(configure "helpful") ;; better contextual information
(configure "vertico") ;; completion
(configure "avy")
(configure "ace-window") ;; navigate between windows
(configure "rainbow-delimiters")
(configure "which-key") ;; prompt with available key sequence continuations
(configure "http-server")
(configure "treemacs")
(configure "embark")

(use-package recentf
  :init
  (recentf-mode))

;; remember last place when opening a file again
(save-place-mode t)

