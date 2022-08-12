(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  :config
  (use-package embark
    :bind
    (("C-."    . embark-act     )
     ("<menu>" . embark-act     )
     ("C->"    . embark-become  )
     ("M-."    . embark-dwim    )
     ("C-h B"  . embark-bindings))
    :init
    (setq prefix-help-command #'embark-prefix-help-command))
  (use-package savehist
    :init
    (savehist-mode)
    :config
    (setq history-length 256))
  (use-package marginalia
    :init
    (marginalia-mode)))
