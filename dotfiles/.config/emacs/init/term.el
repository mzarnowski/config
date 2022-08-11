(init/disable-line-numbers 'term-mode-hook)
(init/disable-line-numbers 'vterm-mode-hook)
(init/disable-line-numbers 'shell-mode-hook)
(init/disable-line-numbers 'eshell-mode-hook)

(use-package term
  :config
  (setq explicit-shell-file-name "bash"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))
