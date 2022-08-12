(use-package org
  :config
  (setq org-hide-emphasis-markers      t)
  (setq org-startup-indented           t)
  (setq org-pretty-entities            t)
  (setq org-hide-emphasis-markers      t)
  (setq org-startup-with-inline-images t)
  (use-package org-superstar
    :config
    (setq org-superstar-special-todo-items t)
    (add-hook 'org-mode-hook (lambda() (org-superstar-mode 1)))))

(use-package org-roam
  :custom
  (org-roam-directory "~/workspace/mzarnowski/notes")
  (org-node-completions-everywhere t)
  :bind
  (("C-c n f" . org-roam-node-find    )
   ("C-c n i" . org-roam-node-insert  )
   ("C-c n l" . org-roam-buffer-toggle))
  :config
  (setq org-return-follows-link        t)
  (org-roam-setup))

