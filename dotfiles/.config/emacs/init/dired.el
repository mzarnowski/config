(use-package dired
  :ensure nil ; not to be downloaded
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  :config
  (global-set-key (kbd "M-RET") 'dired-display-file)
  (use-package dired-single)
  (define-key dired-mode-map [remap dired-find-file] 'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory] 'dired-single-up-directory)
  (use-package treemacs-icons-dired 
    :if (display-graphic-p)
    :hook (dired-mode . treemacs-icons-dired-mode))
  (use-package dired-collapse ; collapse directories with only one child into e.g. foo/bar/baz.txt
    :hook (dired-mode . dired-collapse-mode)))
  ;(use=package dired-hide-dotfiles
  ;  :hook (dired-mode . dired-hide-dotfiles-mode)))
  ;(use-package dired-open
  ;  :config
  ;  (setq dired-open-extensions '(("mkv" . "mpv")))

