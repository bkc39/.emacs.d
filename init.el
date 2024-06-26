;;; init.el --- emacs init file -*- lexical-binding: t; -*-

;; URL: https://github.com/bkc39/my-package
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Init file using straight.el and use-package.

;;; Code:

(defmacro on-system (system &rest body)
  "Execute BODY only if SYSTEM matches the current system type."
  `(when (eq system-type ',system)
     ,@body))

(defmacro on-macos (&rest body)
  "Execute BODY if the current system is macOS."
  `(on-system 'darwin ,@body))

(defmacro on-linux (&rest body)
  "Execute BODY if the current system is Linux."
  `(on-system 'gnu/linux ,@body))

(defmacro add-hookq (hook-name fn)
  "Add FN to the list of functions to be run by HOOK-NAME."
  `(add-hook ',hook-name ,fn))

(defmacro package-install-if-not-there* (&rest packages)
  "Install PACKAGES if they are not already installed."
  (let ((pkg (gensym))
        (to-install (gensym)))
    `(let ((,to-install
            (cl-remove-if #'package-installed-p '(,@packages))))
       (unless (null ,to-install)
         (package-refresh-contents))
       (dolist (,pkg ,to-install)
         (package-install ,pkg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Straight.el config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; configure straight.el
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat
          "https://raw.githubusercontent.com"
          "/radian-software/straight.el/develop/install.el")
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ag)

(use-package aweshell
  :straight (:host github
                   :repo "manateelazycat/aweshell"
                   :files ("*.el" "dist"))
  :bind ("C-c x" . #'aweshell-dedicated-toggle))

(use-package blacken
  :ensure t
  :config
  (setq blacken-line-length 80)
  :hook (python-mode . blacken-mode))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.3))

(use-package company-coq
  :after (proof-general)
  :hook (coq-mode . company-coq-mode)
  :init (setq company-coq-live-on-the-edge t))

(use-package cmake-mode)

(use-package ein)

(use-package eshell-prompt-extras
  :config
  (setq eshell-prompt-function 'epe-theme-lambda))

(use-package ess
  :mode ("\\.R\\'" . R-mode))

(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns))
          (memq system-type '(darwin gnu/linux)))
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t
  :hook (emacs-lisp-mode . flycheck-mode))

(use-package go-mode
  :after lsp-mode
  :hook (go-mode . (lambda ()
                     (electric-pair-mode 1)))
  :config
  (if (executable-find "gopls")
      (add-hookq go-mode-hook #'lsp-deferred)
    (message "go-mode LSP plugin gopls is not installed!")))

(use-package gptel
  :ensure t
  :config
  (setq gptel-model "gpt-4o"
        gptel-api-key (get-openai-api-key)
        gptel-directives (or (read-prompt-md-files "~/.llm-prompts")
                             gptel-directives))
  (setq-default
   gptel--system-message
   (alist-get 'default gptel-directives "You are a helpful assistant."))
  :bind (("C-c RET" . gptel-send)
         ("C-c q" . gptel-quick)
         ("C-c M-d" . gptel-diff)))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp
  :hook (js-mode . (lambda ()
                     (electric-indent-mode -1)
                     (lsp-deferred)))
  :hook (rust-mode . #'lsp-deferred))

(defun check-for-python-executable-in-dir (dir bin-name)
  (let ((executable (concat dir "/" bin-name)))
    (and (file-exists-p executable) executable)))

(use-package lsp-pyright
  :hook (python-mode . lsp-deferred)
  :hook (before-save . lsp-pyright-organize-imports)
  :config
  (progn
    (setq
     python-shell-interpreter (search-venv-for-python-executable)
     blacken-executable (search-venv-for-black-executable))
    (setq lsp-pyright-use-library-code-for-types t)
    (let* ((pyright-stubs-root-dir
            (getenv "PYRIGHT_TYPE_STUBS_ROOT"))
           (pyright-stubs-dir
            (concat pyright-stubs-root-dir
                    "/python-type-stubs")))
      (when (and pyright-stubs-root-dir
                 pyright-stubs-dir)
        (setq lsp-pyright-stubs-path
              pyright-stubs-dir)))))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)
  (setenv "WORKON_HOME"
          (expand-file-name "~/.python-virtual-envs"))
  (setq pyvenv-post-activate-hooks
        (list
         (lambda ()
           (setq python-shell-interpreter
                 (concat pyvenv-virtual-env "/bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list
         (lambda ()
           (setq python-shell-interpreter "python3")))))


(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (on-macos
   (setq lsp-sourcekit-executable
         (string-trim
          (shell-command-to-string
           "xcrun --find sourcekit-lsp")))))

(use-package magit
  :bind (("C-c m" . magit-status)))

(use-package multiple-cursors
  :bind (("C-c M-c" . mc/edit-lines)))

(use-package paredit
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook #'enable-paredit-mode)
    (add-hook 'racket-mode-hook #'enable-paredit-mode))
  :bind
  (:map paredit-mode-map
        ("{"     . paredit-open-curly)
        ("}"     . paredit-close-curly)
        ("C-M-[" . paredit-forward-slurp-sexp)
        ("C-M-]" . paredit-forward-barf-sexp)))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda ()
                           (display-line-numbers-mode -1))))

(use-package proof-general
  :hook (coq-mode . prettify-symbols-mode)
  :custom
  (coq-prog-args
   (list
    "-emacs"
    "-R"
    (or (getenv "CPDT_SRC_DIR")
        "/Users/bkc/dev/coq/cpdt/src")
    "Cpdt")))

(use-package racket-mode
  :mode ("\\.rkt\\'" . racket-mode)
  :hook (racket-mode . (lambda ()
                         (racket-xp-mode t)))
  :config
  (racket-unicode-input-method-enable)
  (add-hookq racket-xp-mode-hook
             (lambda ()
               (remove-hook 'pre-redisplay-functions
                            #'racket-xp-pre-redisplay
                            t))))


(defun rust-run-with-args ()
  "Run with cargo run and additional command line arguments"
  (interactive)
  (let ((args
         (read-string "Command line args: ")))
    (rust--compile
     "%s run -- %s"
     rust-cargo-bin
     args)))

(use-package rust-mode
  :hook (rust-mode . (lambda ()
                       (electric-pair-mode 1)))
  :custom
  (rust-format-on-save t)
  :bind (("C-c C-c m l" . #'lsp-rust-analyzer-open-cargo-toml)
         ("C-c C-c C-n" . #'rust-run-with-args)))

(use-package react-snippets)

(use-package swift-mode
  :hook (swift-mode . #'lsp-deferred))

(use-package solidity-mode)
;; (use-package tree-sitter)
;; (use-package tree-sitter-langs)

(defun setup-tide-mode ()
  "hook to setup tide"
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package web-mode
  :mode ("\\.tsx\\'" . web-mode)
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package whitespace
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face lines trailing empty))
  :hook (prog-mode . whitespace-mode)
  :hook (before-save . whitespace-cleanup))

(use-package yasnippet
  :hook (js-mode . yas-minor-mode)
  :hook (lsp-mode . yas-minor-mode))

(use-package zenburn-theme
  :config
  (progn
    (load-theme 'zenburn t t)
    (enable-theme 'zenburn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other configs

(if (<= 29 (car (version-to-list emacs-version)))
    (global-display-line-numbers-mode 1)
  (global-linum-mode 1))
(setq column-number-mode t)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)

;; set the font to anonymous pro when in GUI if its installed
(defun set-frame-font-to-anonymous-pro ()
  "set frame font to anonymous pro if GUI is there"
  (when (and (display-graphic-p)
             (x-list-fonts "Anonymous Pro"))
    (add-to-list 'default-frame-alist
                 '(font . "Anonymous Pro-12"))))

(set-frame-font-to-anonymous-pro)
(setq js-indent-level 2)

(defun copy-to-clipboard (beg end)
  (interactive "r")
  (let ((command
         (cond
           ((eq system-type 'darwin)
            "pbcopy")
           ((and (eq system-type 'gnu/linux)
                 ;; xclip needs the display or it will fail with null
                 ;; device
                 (getenv "DISPLAY"))
            "xclip -selection clipboard"))))
    (when command
      (shell-command-on-region beg end command))
    (deactivate-mark)))

(defun clipboard+kill-ring-save (beg end)
  "Copies selection to x-clipboard."
  (interactive "r")
  (copy-to-clipboard beg end)
  (kill-ring-save beg end))

(global-set-key (kbd "M-w") 'clipboard+kill-ring-save)

(global-set-key (kbd "C-x p")
                (lambda ()
                  (interactive)
                  (other-window -1)))

(global-set-key (kbd "C-x p")
                (lambda ()
                  (interactive)
                  (other-window -1)))

(add-hookq
 after-change-major-mode-hook
 #'(lambda ()
     (electric-indent-mode -1)))

;; open up the init file in the background on startup
(add-hookq
 emacs-startup-hook
 (lambda ()
   (find-file-noselect user-init-file)))

;; (add-hookq after-make-frame-functions
;;            #'set-frame-font-to-anonymous-pro)

(defun restart-server ()
  "Restart the emacs server and close all clients"
  (interactive)
  (if server-process
      (progn
        (server-force-delete)
        (server-start))
    (message "Not in a client. Exiting...")
    'ok))

(defun load-openai-api-key ()
  "Read the OpenAI API key from the ~/.openai file and store it in the
environment variable OPENAI_API_KEY."
  (let ((api-key-file (expand-file-name "~/.openai")))
    (if (file-readable-p api-key-file)
        (with-temp-buffer
          (insert-file-contents api-key-file)
          (let ((api-key (string-trim (buffer-string))))
            (setenv "OPENAI_API_KEY" api-key)
            (message "OpenAI API key loaded successfully.")))
      (message "Error: ~/.openai file not found or is not readable."))))

(defun get-openai-api-key ()
  "Get the OpenAI API key from the environment variable OPENAI_API_KEY.
If the environment variable is not defined, load the key from the
~/.openai file.  Return the API key as a string."
  (let ((api-key (getenv "OPENAI_API_KEY")))
    (unless api-key
      (load-openai-api-key)
      (setq api-key (getenv "OPENAI_API_KEY")))
    api-key))

(defun pytest-watch ()
  "Run pytest in watch mode and display the output in a buffer."
  (interactive)
  (let ((ptw-exec
         (lsp-pyright--locate-venv))
        (buffer (get-buffer-create "*pytest-watch*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer))
    (start-process-shell-command
     "pytest-watch"
     buffer
     (concat ptw-exec "/bin/ptw" " --clear")))
  (with-current-buffer "*pytest-watch*"
    (read-only-mode 1)
    (display-buffer (current-buffer))))

(defun pyright-watch ()
  "Run pyright and display the output in a buffer."
  (interactive)
  (let* ((buffer (get-buffer-create "*pyright*"))
         (venv-dir (or (lsp-pyright--locate-venv)
                       "venv"))
         (pyright-path (executable-find "pyright"))
         (cmd (concat pyright-path
                      " --pythonpath " python-shell-interpreter " --watch")))
    (message cmd)
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer))
    (start-process-shell-command "pyright" buffer cmd)
    (display-buffer buffer)))

(defmacro make-search-venv-executable-function (name executables fallback)
  "Create a function NAME to search for the right executable in the current
virtual environment.

EXECUTABLES is a list of executable names to search for.
FALLBACK is the fallback executable if none of the EXECUTABLES are found."
  `(defun ,name ()
     "Search for the right executable in the current virtual environment."
     (let ((venv-bin (concat (lsp-pyright--locate-venv) "/bin")))
       (or ,@(mapcar (lambda (exe)
                       `(check-for-python-executable-in-dir venv-bin ,exe))
                     executables)
           ,fallback))))

;; Usage
(make-search-venv-executable-function
 search-venv-for-python-executable
 ("ipython" "python3" "python")
 "python")

(make-search-venv-executable-function
 search-venv-for-black-executable
 ("black")
 "black")

(defun slurp-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (read (buffer-string))))

(defun read-prompt-md-files (directory)
  "Read files in DIRECTORY of the form PROMPT.md and return an
alist (PROMPT . CONTENTS-OF-FILE)."
  (let ((files (directory-files directory t "\\`[^.].*\\.md\\'"))
        result)
    (dolist (file files)
      (when (string-match "\\(.*\\)\\.md\\'" (file-name-nondirectory file))
        (let ((prompt (match-string 1 (file-name-nondirectory file)))
              (contents (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
          (push (cons (intern prompt) contents) result))))
    result))

(defvar gptel-quick--history nil
  "History list for `gptel-quick' prompts.")

(defun gptel-quick (prompt)
  "Send PROMPT to ChatGPT and display the response in a special buffer.
If the PROMPT is empty, signals a user error."
  (interactive (list (read-string "Ask ChatGPT: " nil gptel-quick--history)))
  (when (string= prompt "") (user-error "A prompt is required."))
  (unless (fboundp 'gptel-request)
    (require 'gptel))
  (gptel-request
      prompt
    :system (alist-get 'default gptel-directives "You are a helpful assistant.")
    :callback
    (lambda (response info)
      (if (not response)
          (message "gptel-quick failed with message: %s" (plist-get info
                                                                    :status))
        (with-current-buffer (get-buffer-create "*gptel-quick*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert response))
          (special-mode)
          (display-buffer (current-buffer)))))))

(defun gptel-diff ()
  "Generate a git commit message.

This function uses `magit-diff-staged` to obtain the current staged diffs
in a temporary buffer.  It then constructs a request string from these diffs
and sends it to the `gptel-request` function, which interacts with the GPT
model to generate a commit message.  Upon receiving the response, it places
the generated message in a special buffer named *gptel-diff* and copies it
to the kill ring.  If a buffer named 'COMMIT_EDITMSG' is also present, it
will switch to that buffer and notify the user that the commit message is
in the kill ring."
  (interactive)
  (let* ((diff-buffer
          (with-temp-buffer
            (magit-diff-staged)
            (buffer-name)))
         (diff-str
          (with-current-buffer diff-buffer
            (buffer-substring-no-properties (point-min) (point-max)))))
    (gptel-request
        diff-str
      :system
      (alist-get
       'commiter
       gptel-directives
       "Write a git commit message for this diff. Include ONLY the message.
Be terse. Provide messages whose lines are at most 80 characters")
      :callback
      (lambda (response info)
        (if (not response)
            (message
             "gptel-diff failed with message: %s"
             (plist-get info :status))

          (kill-new response)
          (with-current-buffer (get-buffer-create "*gptel-diff*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert response))
            (special-mode)
            (display-buffer (current-buffer)))
          (when (get-buffer "COMMIT_EDITMSG")
            (message "commit message in kill ring")
            (pop-to-buffer "COMMIT_EDITMSG")))))))

(provide 'init)
;;; init.el ends here
