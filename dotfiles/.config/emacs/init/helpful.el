(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :config
  (general-define-key "C-h C-h" 'helpful-at-point)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command ] . helpful-command )
  ([remap describe-symbol  ] . helpful-symbol  )					  
  ([remap describe-key     ] . helpful-key     ))
