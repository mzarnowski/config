(use-package doom-themes )
(use-package leuven-theme)
; TODO looks nice for dark theme: (use-package melancholy-theme)
;(load-theme 'doom-one-light)
;(load-theme 'doom-palenight)
;(load-theme 'doom-1337)
(load-theme 'leuven)
(load-theme 'leuven-dark)

(defun toggle-theme ()
  (interactive)
  (let* ((day   'leuven)
	 (night 'leuven-dark)	 
	 (is-day (eq (car custom-enabled-themes) day))
	 (other-theme (if is-day night day)))
    (load-theme other-theme)))

(global-set-key [f5] 'toggle-theme)

;; TODO consider themes
;; dark : melancholy-theme
;; light: ample-theme
