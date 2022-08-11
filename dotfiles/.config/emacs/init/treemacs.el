(defun my/treemacs-go-to-window ()
  (let* ((focused-on-tree-buffer (equal (treemacs-get-local-buffer  ) (window-buffer)))
	 (is-tree-buffer-visible (equal (treemacs-current-visibility) 'visible)))
    (cond (focused-on-tree-buffer (treemacs))
	  (is-tree-buffer-visible (treemacs-select-window))
	  (t                      (treemacs-select-window)))))

(use-package treemacs
  :defer t
  :config
  (init/disable-line-numbers 'treemacs-mode-hook)
  :bind
  (:map global-map
	("M-1" . (lambda () (interactive) (my/treemacs-go-to-window)))))


      
