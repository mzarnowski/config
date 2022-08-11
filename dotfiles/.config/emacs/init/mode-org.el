(use-package org)

(use-package org-roam
  :custom
  (org-roam-directory "~/workspace/mzarnowski/notes")
  (org-node-completions-everywhere t)
  :bind
  (("C-c n f" . org-roam-node-find    )
   ("C-c n i" . org-roam-node-insert  )
   ("C-c n l" . org-roam-buffer-toggle))
  :config
  (setq org-return-follows-link  t)
  (org-roam-setup))
