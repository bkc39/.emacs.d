;;; init.el --- Emacs init -*- lexical-binding: t; -*-

;; URL: https://github.com/bkc39/.emacs.d
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Entry point that wires up configuration split across config.el,
;; packages.el, and gptel-tweaks.el.

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(require 'config)
(require 'packages)
(require 'gptel-tweaks)

(provide 'init)
;;; init.el ends here
