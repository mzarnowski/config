(require 's)
(require 'dash)

(defun workspace-header--merge-key (prefix key)
  (let ((suffix (single-key-description key)))
    (if prefix
	(concat prefix " " suffix)
      suffix)))

(defun workspace-header--for-each-in-keymap (acc keymap f &optional prefix)
  (map-keymap (lambda (key action)
		(let ((key (workspace-header--merge-key prefix key)))
		  (if (keymapp action)
		      (workspace-header--for-each-in-keymap acc action f key)
		    (add-to-list acc (funcall f key action)))))
	      keymap)
    acc)

(defun workspace-header--format-key (key)
  ;; leading 'M-' prefix is represented as 'ESC'
  (let ((key (s-replace-regexp "^ESC " "M-" key)))
    (propertize key
		'face           'help-key-binding
		'font-lock-face 'help-key-binding)))

(defun workspace-header--format-command (key name)
  (let ((key (workspace-header--format-key key)))
    (format "%s: '%s'" name key)))

(defun workspace-header--format-binding (key action)
  (pcase action
    (`(,name . ,_) (workspace-header--format-command key name))))

(defun workspace-header (keymap)
  (let ((properties ()))
    (workspace-header--for-each-in-keymap 'properties keymap #'workspace-header--format-binding)
    (s-join " " properties)))

(provide 'workspace-header)
(workspace-header (workspace/define-keymap
	   `("C-c C-c" "finalize" workspace/thought-capture-finalize)
	   `("C-c C-k" "discard"  workspace/thought-capture-discard)))
