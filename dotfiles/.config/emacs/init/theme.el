(use-package doom-themes)
(load-theme 'doom-one-light)
(load-theme 'doom-palenight)

(defun toggle-theme ()
  (interactive)
  (let* ((day   'doom-one-light)
	 (night 'doom-palenight)	 
	 (is-day (eq (car custom-enabled-themes) day))
	 (other-theme (if is-day night day)))
    (enable-theme other-theme)))

(global-set-key [f5] 'toggle-theme)
