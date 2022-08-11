;; disable default vc-mode
(setq vc-handled-backends nil)
(require 'vc)
(eval-after-load "vc" '(remove-hook 'find-file-hook 'vc-find-file-hook))
(eval-after-load "vc" '(remove-hook 'find-file-hook 'vc-refresh-state))

(use-package magit)
