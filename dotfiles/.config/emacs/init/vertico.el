(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  :config
  (use-package embark)
  (use-package savehist
    :init
    (savehist-mode)
    :config
    (setq history-length 256))
  (use-package marginalia
    :init
    (marginalia-mode)))
