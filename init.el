;;; init.el --- Emacs init -*- lexical-binding: t; -*-

;; URL: https://github.com/bkc39/.emacs.d
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Entry point that wires up configuration split across config.el,
;; packages.el, and gptel-tweaks.el.

;;; Code:

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

(require 'config)
(require 'packages)
(require 'gptel-tweaks)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("923cf2ebe5d4f7cf6cccf3f00ab3e6bf8070c2bc385b6a93bdd2bb8317368628"
     "f87c86fa3d38be32dc557ba3d4cedaaea7bc3d97ce816c0e518dfe9633250e34"
     "30d174000ea9cbddecd6cc695943afb7dba66b302a14f9db5dd65074e70cc744"
     "2b20b4633721cc23869499012a69894293d49e147feeb833663fdc968f240873"
     "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
     "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19"
     default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:background nil :foreground "gray")))))
