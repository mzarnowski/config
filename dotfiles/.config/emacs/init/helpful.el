(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command ] . helpful-command )
  ([remap describe-key     ] . helpful-key     )
  ;(global-set-key (kbd "C-c C-d") #'helpful-at-point)
  )
