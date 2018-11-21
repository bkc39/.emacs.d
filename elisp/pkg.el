;;; pkg.el --- My list of packages
;;; Commentary:

;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; Required Packages
(defvar required-packages
  '(magit
    yasnippet
    color-theme
    clojure-mode
    clojure-snippets
    clojure-cheatsheet
    haskell-mode
    hindent
    hi2
    tuareg
    java-snippets
    javadoc-lookup
    ensime
    scheme-complete
    racket-mode
    flycheck
    google-c-style
    ess
    auctex
    markdown-mode
    exec-path-from-shell
    whitespace
    zenburn-theme
    color-theme-solarized
    request
    request-deferred
    json
    oauth
    oauth2
    swift-mode
    w3m
    geiser
    ;; pymacs
    paredit
    autopair
    cider
    anaconda-mode
    rust-mode
    sr-speedbar
    jedi-core
    jabber
    pydoc-info
    web-mode
    jsx-mode
    dash
    dockerfile-mode
    multiple-cursors)
  "A list of packages to ensure are installed at launch.")

;; function to ensure that all of the above packages are installed.
(eval-when-compile (require 'cl))
(defun packages-installed-p ()
  "Check if packages are installed."
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; If not all packages are installed, check them one by one and
;; install the missing ones.
(unless (packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'pkg)
;;; pkg.el ends here
