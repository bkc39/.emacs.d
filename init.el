;;; init.el --- Emacs init -*- lexical-binding: t; -*-

;; URL: https://github.com/bkc39/.emacs.d
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Entry point that wires up configuration split across config.el,
;; packages.el, and gptel-tweaks.el.

;;; Code:

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'config)
(require 'packages)
(require 'gptel-tweaks)

(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)
;;; init.el ends here
