(global-display-line-numbers-mode t)
(global-auto-revert-mode          t)

;; don't show line numbers in some modes
(defun init/disable-line-numbers (mode)
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
