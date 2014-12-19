;;; pkg.el --- My list of packages
;;; Commentary:

;;; Code:
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Required Packages
(defvar required-packages
  '(magit
    yasnippet
    color-theme
    pretty-lambdada
    clojure-mode
    clojure-snippets
    clojure-cheatsheet
    clojure-test-mode
    haskell-mode
    tuareg
    java-snippets
    javadoc-lookup
    scala-mode2
    scheme-complete
    racket-mode
    auto-complete
    auto-complete-auctex
    auto-complete-c-headers
    auto-complete-clang
    ac-c-headers
    ac-math
    ac-nrepl
    ac-geiser
    flycheck
    flycheck-google-cpplint
    google-c-style
    ess
    auctex
    markdown-mode
    color-theme-wombat
    exec-path-from-shell
    whitespace
    zenburn-theme
    color-theme-solarized
    ido-ubiquitous
    request
    request-deferred
    json
    oauth
    oauth2
    swift-mode
    w3m
    rainbow-delimiters
    geiser
    pymacs
    paredit
    autopair
    cider
    anaconda-mode)
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
