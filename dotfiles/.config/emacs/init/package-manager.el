(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")))
;; initializes package-system
(package-initialize) 
(unless package-archive-contents
        (package-refresh-contents))

(unless (package-installed-p 'use-package)
        (package-install     'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval             7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results         t)
  :config
  (auto-package-update-maybe))
