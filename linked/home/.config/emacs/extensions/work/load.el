(let ((files (f-files "~/.config/emacs/extensions/work" (lambda (it) (f-ext? it "el")) t)))
  (-each (-filter (lambda (it) (not (equal (f-filename it) "load.el"))) files) #'load-file))
