(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (message "%s" "Refreshing package set")
  (package-refresh-contents)
  (message "%s" "installing use-package")
  (package-install 'use-package)
  (message "%s" "done!"))

(eval-when-compile
  (require 'use-package))

(use-package zenburn-theme
  :config
  (progn
    (load-theme 'zenburn t t)
    (enable-theme 'zenburn)))

(use-package haskell-mode
  :mode "\\.hs$")
