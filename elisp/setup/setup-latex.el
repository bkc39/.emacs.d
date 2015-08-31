;;; setup-latex.el --- Customizations for LaTeX
;;; Commentary:
;;;   Setup functions for LaTeX, mostly configuring AUCTeX.

;;; Code:

;; Set the configuation variables
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

;; some sensible hooks
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; Make latexmk available via C-c C-c
;; Note: SyncTeX is setup via $HOME/.latexmkrc
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (push
             '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
               :help "Run latexmk on file")
             TeX-command-list)
            (push
             '("pdflatex" "pdflatex %s" TeX-run-TeX nil t
               :help "Run latexmk on file")
             TeX-command-list)))

; TODO: switch this to latexmk
(add-hook 'TeX-mode-hook
          '(lambda ()
             (setq TeX-command-default "pdflatex")))

;; Set Preview as the default pdf viewer
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq
 TeX-view-program-list
 '(("PDF Viewer"
    "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

(provide 'setup-latex)
;;; setup-latex.el ends here
