(use-package treemacs
  :defer t
  :config
  (init/disable-line-numbers 'treemacs-mode-hook)
  (setq treemacs-select-when-already-in-treemacs 'close)
  (general-define-key "M-1"    'treemacs-select-window))

