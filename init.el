;;; init.el --- emacs initialization file.
;;; Commentary:
;;;   loaded on start.

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/.emacs.d/startup-utils")
(recursively-add-directory-to-load-path "~/.emacs.d/elisp")
(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
(add-hook 'after-init-hook
          '(lambda ()
             (require 'pkg)
             (require 'setup)
             (setup/setup-all)))
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (electric-indent-mode -1)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(company-ghc-show-info t)
 '(custom-safe-themes
   (quote
    ("c2cfe2f1440d9ef4bfd3ef4cf15bfe35ff40e6d431264b1e24af64f145cffb11" default)))
 '(ess-R-font-lock-keywords
   (quote
    ((ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters . t)
     (ess-fl-keyword:=)
     (ess-R-fl-keyword:F&T))))
 '(fci-rule-color "#383838")
 '(flycheck-check-syntax-automatically (quote (mode-enabled)))
 '(flycheck-idle-change-delay 1000)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-path-ghci "stack")
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote stack-ghci))
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(package-selected-packages
   (quote
    (dockerfile-mode tide multiple-cursors fill-column-indicator flycheck-haskell 0xc merlin yaml-mode ess-R-data-view jedi phabricator php-mode php+-mode jsx-mode web-mode pydoc-info jabber jedi-core sr-speedbar rust-mode anaconda-mode autopair paredit pymacs geiser w3m swift-mode oauth2 oauth request-deferred request ido-ubiquitous color-theme-solarized zenburn-theme exec-path-from-shell color-theme-wombat markdown-mode auctex ess google-c-style flycheck-google-cpplint flycheck ac-nrepl ac-math ac-c-headers auto-complete-clang auto-complete-c-headers auto-complete-auctex auto-complete racket-mode scheme-complete ensime scala-mode2 javadoc-lookup java-snippets tuareg hi2 hindent haskell-mode clojure-test-mode clojure-cheatsheet clojure-snippets clojure-mode pretty-lambdada color-theme yasnippet magit)))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(provide 'init)
;;; init.el ends here
