;;; setup-ocaml.el --- Setup for OCaml
;;; Commentary:
;;;   Customizations to OCaml with OCaml using tuareg-mode.

;;; Code:

;; Initialize tuareg
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(autoload 'tuareg-imenu-set-imenu "tuareg-imenu"
  "Configuration of imenu for tuareg" t)

(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

;; Merlin setup
;; Add opam emacs directory to the load-path
(defvar opam-share nil)
(setq opam-share
      (substring
       (shell-command-to-string "opam config var share 2> /dev/null")
       0 -1))

(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;; Load merlin-mode
(require 'merlin)

;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)

;; Enable auto-complete
(setq merlin-use-auto-complete-mode 'easy)

;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)

(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;; Custom OCaml stuff
(add-hook 'tuareg-mode-hook
          '(lambda ()
             ;; pressing "RETURN" also indents
             (local-set-key (kbd "RET") 'newline-and-indent)

             ;; clean up whitespace at save
             (make-local-variable 'before-save-hook)
             (add-hook 'before-save-hook 'whitespace-cleanup)

	     ;; fun is lambda
             (font-lock-add-keywords
              nil `(("(\\(fun\\>\\)"
                     (0 (progn
                          (compose-region (match-beginning 1) (match-end 1)
                                          ,(make-char 'greek-iso8859-7 235))
                          nil)))))))

(provide 'setup-ocaml)
;;; setup-ocaml.el ends here
