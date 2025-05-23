;;; init.el --- emacs init file -*- lexical-binding: t; -*-

;; URL: https://github.com/bkc39/.emacs.d
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Init file using straight.el and use-package.

;;; Code:

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

(defmacro defun/who (name args &rest body)
  "Define a function NAME with arguments ARGS and body BODY.
Includes the function name as a local variable WHO within the body.

Special keyword arguments:
  :command -- If provided, make the function interactive."
  (let* ((docstring (if (stringp (car body)) (pop body)))
         (options (when (keywordp (car body)) (pop body)))
         (command (eq :command options)))
    `(defun ,name ,args
       ,@(when docstring (list docstring))
       ,@(when command '((interactive)))
       (let ((who ',name))
         ,@body))))

(defun indent-like-defun (sym)
  "Set the `lisp-indent-function` property of SYM to `defun`.

This function checks whether SYM has the `lisp-indent-function` property set.
If not, it assigns `defun` as its `lisp-indent-function`, allowing SYM to be
indented like a standard Emacs `defun`.  This function is typically used for
macros that define new constructs or syntax that follow the same indentation
rules as regular function definitions in Lisp languages.

Example usage:
  (indent-like-defun 'my-custom-macro)

Arguments:
  SYM -- The symbol to set the `lisp-indent-function` property for.

Returns:
  nil"
  (unless (get sym 'lisp-indent-function)
    (put sym 'lisp-indent-function 'defun)))

(indent-like-defun 'defun/who)

(defun/who expand-syscond-clause (clause)
  "Expand a single system type clause in a `syscond` macro.

CLAUSE should be a form where the car is a symbol representing a system
type (e.g., 'gnu, 'darwin, etc.) and the rest are forms to evaluate if
the current system type matches.

Valid system types include:
- `gnu`
- `gnu/linux`
- `gnu/kfreebsd`
- `darwin`
- `ms-dos`
- `windows-nt`
- `cygwin`
- `berkeley-unix`
- `aix`
- `hpux`
- `usg-unix-v`

During expansion, the function checks if the system type is valid,
signaling an error if it isn't."
  (let ((valid-system-types
         '(gnu
           gnu/linux
           gnu/kfreebsd
           darwin
           ms-dos
           windows-nt
           cygwin
           berkeley-unix
           aix
           hpux
           usg-unix-v)))
    (pcase clause
      (`(,x ,y . ,rest)
       (unless (memq x valid-system-types)
         (user-error "In %s during the expansion of %s: invalid system type %s"
                     'syscase
                     who
                     x))
       `((equal ',x system-type)
         (progn ,y ,@rest))))))

(defmacro syscase (&rest clauses)
  "Conditionally evaluate forms based on the system type.

CLAUSES should be a list of forms, where each form is structured as:
    (SYSTEM-TYPE BODY...)

SYSTEM-TYPE should be one of the valid system types recognized by
`expand-syscond-clause`.

BODY... is a series of forms that will be evaluated if the current system
type matches SYSTEM-TYPE.

Example usage:
    (syscase
     (darwin
      (message \"on macOS\"))
     (gnu/linux
      (message \"on Linux\")))"
  (let ((clause-expansions
         (mapcar #'expand-syscond-clause clauses)))
    `(cond
      ,@clause-expansions)))

(defmacro on-macos (&rest body)
  "Execute BODY if the current system is macOS.

This macro uses the `syscase` macro to check if the current system type is
`darwin` (which represents macOS).  If the system type matches, it executes
the BODY expressions.  If not, the BODY is not executed.

Example usage:
  (on-macos 'apple)

Arguments:
  BODY -- One or more forms to be executed if the current system type is macOS.

Returns:
  nil if the current system type is not macOS, otherwise executes the BODY
  and returns the result of the last form."
  `(syscase (darwin ,@body)))

(defmacro on-linux (&rest body)
  "Execute BODY if the current system type is GNU/Linux.

This macro uses the `syscase` macro to check if the current system type is
'gnu/linux'.  If the system type matches, it executes the BODY expressions.  If
not, the BODY is not executed.

Example usage:
  (on-linux
    (message \"This is a Linux system.\"))

Arguments:
  BODY -- One or more forms to be executed if the current system type is
          GNU/Linux.

Returns:
  nil if the current system type is not GNU/Linux, otherwise executes the BODY
  and returns the result of the last form."
  `(syscase (gnu/linux ,@body)))

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

(use-package agda2-mode
  :straight (agda :type git
                  :host github
                  :repo "agda/agda"
                  :tag "v2.7.0.1"
                  :files (:defaults "src/data/emacs-mode/*.el"))
  :mode ("\\.agda\\'" "\\.lagda\\.md\\'")
  :init
  ;; Optional: Add Agda binary path to exec-path, adjust the path as needed
  ;;(add-to-list 'exec-path "/path/to/agda/bin/")
  :config
  ;; Additional configuration can be added here
  )

(use-package doom-themes
  :ensure t)

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

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'lsp-deferred)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'cider-mode))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (define-key cider-mode-map (kbd "C-c RET") nil)
  ;; ^ delete the default key because we have that bound to gptel-send
  (define-key cider-mode-map
              (kbd "C-c C-r RET")
              'cider-macroexpand-1))

(use-package clang-format
  :ensure t
  :hook ((c-mode c++-mode) . #'clang-format-on-save)
  :config
  (let ((clang-format-exec
         (executable-find "clang-format")))
    (if clang-format-exec
        (setq clang-format-executable clang-format-exec)
      (warn "clang-format not found!"))))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-ide
  :ensure t
  :config
  (cmake-ide-setup))

(use-package compile
  :ensure t
  :after cc-mode
  :bind (:map c++-mode-map
              ("C-c C-f" . my/c-family-configure)
              ("C-c C-c" . my/c-family-build)
              ("C-c C-t" . my/c-family-test)))

(use-package ein)

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

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
  :hook ((emacs-lisp-mode . flycheck-mode)
         (c-mode . flycheck-mode)
         (c++-mode . flycheck-mode)))

(use-package go-mode
  :after lsp-mode
  :hook (go-mode . (lambda ()
                     (electric-pair-mode 1)))
  :config
  (if (executable-find "gopls")
      (add-hookq go-mode-hook #'lsp-deferred)
    (message "go-mode LSP plugin gopls is not installed!")))

(use-package uuidgen)

(use-package gptel
  :ensure t
  :config
  (setq gptel-model "o4-mini"
        gptel-api-key (get-openai-api-key))
  (ensure-gptel-directives-loaded)
  (setq-default
   gptel--system-message
   (alist-get 'default gptel-directives "You are a helpful assistant."))
  (gptel-make-ollama
      "Ollama"
    :host "localhost:11434"
    :models '("gemma2:27b" "llama")
    :stream t)
  (gptel-make-anthropic "Claude"
    :stream t
    :key (get-anthropic-api-key))
  :bind (("C-c RET" . gptel-send)
         ("C-c q" . gptel-quick)
         ("C-c M-d" . gptel-diff)
         ("C-c M-p" . gptel-pull-request)
         ("C-c M-s" . gptel-document-symbol-at-point)
         ("C-c M-t" . gptel-tests-for-symbol-at-point)))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp
  :hook (js-mode . (lambda ()
                     (electric-indent-mode -1)
                     (lsp-deferred)))
  :hook ((rust-mode . #'lsp-deferred)
         (c++-mode . #'lsp-deferred)
         (c-mode . #'lsp-deferred))
  :custom
  (lsp-file-watch-ignored
   '("[/\\\\]\\.venv$" "[/\\\\]venv$" "[/\\\\]\\.direnv$"
     "[/\\\\]\\.mypy_cache$" "[/\\\\]\\.pytest_cache$"
     "[/\\\\]__pycache__$" "[/\\\\]build$" "[/\\\\]dist$"))
  :config
  (setq lsp-enable-file-watchers nil
        lsp-file-watch-threshold 10000
        lsp-prefer-flymake nil
        lsp-clients-clangd-args '("--clang-tidy"))
  (awhen (executable-find "clangd")
    (setq lsp-clangd-binary-path it)))


(defun check-for-python-executable-in-dir (dir bin-name)
  (let ((executable (concat dir "/" bin-name)))
    (and (file-exists-p executable) executable)))

(defun organize-python-imports ()
  "Organize Python imports using lsp-pyright-organize-imports."
  (interactive)
  (when (and (eq major-mode 'python-mode) (bound-and-true-p lsp-mode))
    (lsp-pyright-organize-imports)))

(defun reset-lsp-python-workspace (folder)
  "Reset LSP Python workspace.

User can provide a FOLDER, defaulting to the result of
`locate-dominating-file` which looks for the nearest 'venv' or '.git'
directory."
  (interactive
   (let ((inferred-project-root
          (or (locate-dominating-file default-directory "venv")
              (locate-dominating-file default-directory ".git"))))
     (list (read-directory-name
            "Select new folder: "
            inferred-project-root))))
  (lsp-workspace-folders-remove (lsp-workspace-root))
  (lsp-workspace-folders-add folder)
  (lsp-restart-workspace))

(defun run-python-with-extra-pythonpaths (paths)
  "Run Python with additional PYTHONPATHS.
Optionally prompt for user-specified PATHS if prefix argument is supplied."
  (interactive
   (let ((default-path (lsp-workspace-root)))
     (if current-prefix-arg
         (list (read-directory-name
                "Specify additional PYTHONPATH: " default-path))
       (list default-path))))
  (unless (and (boundp 'pyvenv-virtual-env) pyvenv-virtual-env)
    (message "WARNING (%s): No virtual environment activated"
             'run-python-with-extra-pythonpaths))
  (setq python-shell-extra-pythonpaths
        (append (if (listp paths) paths (list paths))
                python-shell-extra-pythonpaths))
  (run-python)
  (python-shell-switch-to-shell
   "run-python-with-extra-pythonpaths: could not switch to python shell?"))

(defun rebind-run-python-hotkey ()
  "Rebind the the run python hotkey.

`run-python` ==> `run-python-with-extra-pythonpaths`."
  (define-key python-mode-map
              (kbd "C-c C-p")
              'run-python-with-extra-pythonpaths))

(use-package lsp-pyright
  :hook (python-mode . lsp-deferred)
  :hook (before-save . organize-python-imports)
  :hook (python-mode . rebind-run-python-hotkey)
  :config
  (progn
    (setq
     ;; python-shell-interpreter (search-venv-for-python-executable)
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

(defun try-locate-venv-named (venv-name)
  "Try to locate a virtual environment named VENV-NAME.

Looks in the current or any parent directory.  Return the full path
to the virtual environment if found, or nil otherwise."
  (let ((dir
         (locate-dominating-file default-directory venv-name)))
    (when dir
      (concat (file-name-as-directory dir) venv-name))))

(defun activate-default-venv (venv-path)
  "Activate a Python virtual environment located at VENV-PATH.

If called interactively, prompt the user for the virtual
environment directory.  If a 'venv' or '.venv' virtual
environment is found in the current or any parent directory, use
that as the default suggestion."
  (interactive
   (list
    (read-directory-name
     "venv: "
     (or (try-locate-venv-named "venv")
         (try-locate-venv-named ".venv")
         default-directory))))
  (pyvenv-activate venv-path))

(use-package pyvenv
  :ensure t
  :after (lsp-pyright)
  :bind (:map
         python-mode-map
         ("C-c v a" . activate-default-venv))
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
  :bind (("C-c m" . magit-status))
  :hook ((git-commit-mode . insert-issue-prefix)))

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
  :mode ("\\.scrbl'" . racket-hash-lang-mode)
  :hook (racket-mode . racket-unicode-input-method-enable)
  ;; :config
  ;; (with-temp-buffer
  ;;   (racket-unicode-input-method-enable)
  ;;   (let ((quail-current-package (assoc "racket-unicode"
  ;;                                       quail-package-alist)))
  ;;     (quail-define-rules
  ;;      ((append . t))
  ;;      ("oplus" ["⊕"])
  ;;      ("otimes" ["⊗"]))))
  )

(defun rust-run-with-args ()
  "Run with cargo run and additional command line arguments."
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

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

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

(defun copy-to-clipboard/macos (beg end)
  (interactive "r")
  (on-macos
   (shell-command-on-region beg end "pbcopy")
   (deactivate-mark)))


(defun/who xclip-buffer (selection-type)
  (if (memq selection-type '(primary secondary clipboard))
      (call-process-region
       (point-min) (point-max)
       "xclip" nil nil nil "-selection" (symbol-name selection-type))
    (message "%s: invalid selection type %s"
             who selection-type)))

(defun copy-to-clipboard/linux (beg end)
  "Copy the region from BEG to END to the system clipboard."
  (interactive "r")
  (on-linux
   (if (use-region-p)
       (let ((text (buffer-substring-no-properties beg end)))
         (with-temp-buffer
           (insert text)
           (xclip-buffer 'primary)
           (xclip-buffer 'clipboard)))
     (message "No region selected"))))

(defun clipboard+kill-ring-save (beg end)
  "Copy selection to kill ring and system clipboard.

This function copies the selected region (from BEG to END) to the
kill ring, which is Emacs' internal clipboard.  Additionally,
depending on the system type, it also copies the region to the
system clipboard, making the text accessible outside of Emacs.

On macOS, it uses the `pbcopy` command to copy the text to the
system clipboard.  On GNU/Linux, it uses `xclip` to copy the
text.

Arguments:
  BEG -- The beginning position of the selected region.
  END -- The ending position of the selected region.

Example usage:
  (clipboard+kill-ring-save BEG END)

This can be bound to a key for convenient access:
  (global-set-key (kbd \"M-w\") 'clipboard+kill-ring-save)"
  (interactive "r")
  (syscase
   (darwin
    (copy-to-clipboard/macos beg end))
   (gnu/linux
    (copy-to-clipboard/linux beg end)))
  (kill-ring-save beg end))

(global-set-key (kbd "M-w") 'clipboard+kill-ring-save)

(global-set-key (kbd "C-x p")
                (lambda ()
                  (interactive)
                  (other-window -1)))

(add-hookq
 after-change-major-mode-hook
 #'(lambda ()
     (electric-indent-mode -1)))

(add-hookq
 tex-mode-hook
 (lambda ()
   (local-unset-key (kbd "C-c RET"))))

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

(defun get-api-key-from-env-or-file (env-var api-key-file)
  "Get the API key from ENV-VAR or from API-KEY-FILE.

Arguments:
  ENV-VAR -- The environment variable to check for the API key.

  API-KEY-FILE -- The file to read the API key from if the environment
  variable is not set.

Returns:
  The API key as a string.

Example usage:
  (get-api-key-from-env-or-file
    \"MY_API_KEY_ENV\"
    \"~/.my_api_key_file\")"
  (let ((api-key (getenv env-var))
        (key-file (expand-file-name api-key-file)))
    (unless api-key
      (if (file-readable-p key-file)
          (with-temp-buffer
            (insert-file-contents key-file)
            (setq api-key (string-trim (buffer-string)))
            (setenv env-var api-key)
            (message "API key loaded successfully."))
        (message "Error: %s file not found or is not readable." key-file)))
    api-key))

(defun get-openai-api-key ()
  "Get the OpenAI API key from the environment variable OPENAI_API_KEY.
If the environment variable is not defined, load the key from the
~/.openai file.  Return the API key as a string."
  (get-api-key-from-env-or-file "OPENAI_API_KEY" "~/.openai"))

(defun get-anthropic-api-key ()
  "Get the Anthropic API key from the environment variable 'ANTHROPIC_API_KEY'.

If the environment variable is not defined, load the key from the
~/.anthropic file.  Returns the API key as a string.

Example Usage:
  (get-anthropic-api-key)

This will return the API key string from the environment variable
'ANTHROPIC_API_KEY' or from the ~/.anthropic file.

Returns:
  string: The Anthropic API key.

Raises:
  Error if the API key cannot be loaded."
  (get-api-key-from-env-or-file "ANTHROPIC_API_KEY" "~/.anthropic"))

(defmacro with-defined-functions (symbols &rest body)
  "Execute BODY only if all SYMBOLS are bound as functions.

If any are not then display an error message with the first
undefined symbol."
  (let ((cc (gensym)))
    `(catch ',cc
       (when (and
              ,@(mapcar
                 (lambda (sym)
                   `(or (fboundp ',sym)
                        (throw
                         ',cc
                         (format "%s is not bound as a function!" ',sym))))
                 symbols))
         ,@body))))

(defun pytest-watch ()
  "Run pytest in watch mode and display the output in a buffer."
  (interactive)
  (with-defined-functions
   (lsp-pyright--locate-venv lsp-workspace-root)
   (let ((buffer (get-buffer-create "*pytest-watch*"))
         (project-root (lsp-workspace-root)))
     (with-current-buffer buffer
       (read-only-mode -1)
       (erase-buffer))
     (start-process-shell-command
      "pytest-watch"
      buffer
      (concat
       "cd " project-root " && "
       (lsp-pyright--locate-venv)
       "/bin/pytest-watch --clear")))
   (with-current-buffer "*pytest-watch*"
     (read-only-mode 1)
     (display-buffer (current-buffer)))))

(defun pyright-watch ()
  "Run pyright and display the output in a buffer."
  (interactive)
  (let* ((buffer (get-buffer-create "*pyright*"))
         (pyright-path (executable-find "pyright"))
         (cmd (concat pyright-path
                      " --pythonpath "
                      (search-venv-for-python-executable)
                      " --watch")))
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
 search-venv-for-pytest-watch-executable
 ("pytest-watch" "ptw")
 "ptw")

(make-search-venv-executable-function
 search-venv-for-black-executable
 ("black")
 "black")

(defun slurp-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (read (buffer-string))))

(defun read-prompt-md-files (directory)
  "Read files in DIRECTORY of the form PROMPT.md.

Returns an alist (PROMPT . CONTENTS-OF-FILE)."
  (let ((files (directory-files directory t "\\`[^.].*\\.md\\'"))
        result)
    (dolist (file files)
      (when (string-match "\\(.*\\)\\.md\\'" (file-name-nondirectory file))
        (let ((prompt (match-string 1 (file-name-nondirectory file)))
              (contents (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
          (push (cons (intern prompt) contents) result))))
    (message "prompts loaded from %s" directory)
    result))

(defun gptel-request/require (&rest args)
  "Ensure gptel-request is bound, require 'gptel if not, then call
gptel-request with ARGS."
  (unless (fboundp 'gptel-request)
    (require 'gptel))
  (apply #'gptel-request args))

(defvar *llm-prompts-dir* "~/.llm-prompts"
  "Directory containing the PROMPT.md files.")

(defun/who reload-gptel-directives ()
  "Try reloading `gptel-directives` from `*llm-prompts-dir*`.

This function attempts to load GPT-3 directives from the files located in the
`*llm-prompts-dir*` directory and updates the `gptel-directives` variable with
the contents of these files.  This is useful for dynamically updating GPT
directives without having to restart Emacs.

`*llm-prompts-dir*` should point to a directory containing markdown files
with directives.  Each file should be named using the pattern `PROMPT.md`,
where `PROMPT` is a key representing the directive.  The contents of each
file will be read and stored as the value for the corresponding key in
the `gptel-directives` alist.

Usage:
  (try-reload-gptel-directives)

Returns: nil. Sets the variables `gptel-directives` with the prompts from
`*llm-prompts-dir*` as a side-effect."
  :command
  (if (file-exists-p *llm-prompts-dir*)
      (setq gptel-directives
            (read-prompt-md-files *llm-prompts-dir*))
    (message "%s: LLM prompts directory %s does not exist!"
             who
             *llm-prompts-dir*)))

(defun ensure-gptel-directives-loaded ()
  "Ensure that `gptel-directives` is defined."
  (unless (boundp 'gptel-directives)
    (reload-gptel-directives)))

(defvar gptel-quick--history nil
  "History list for `gptel-quick' prompts.")

(defun dynamic-prompt (make-prompt callback gptel-request-args)
  "Generate a dynamic prompt for ChatGPT queries.

This function orchestrates the process of dynamically generating a prompt,
sending it to GPT-3 (via `gptel-request`), and handling the response with a
given callback.  It uses `ensure-gptel-directives-loaded` to load default GPT-3
directives if they are not already defined.

Arguments:
- `MAKE-PROMPT`: A function that generates the prompt to be sent to GPT-3. This
  function should not take any parameters.
- `CALLBACK`: A function to handle the GPT-3 response.  It should accept two
  parameters: the GPT-3 response and the response information.
- `GPTEL-REQUEST-ARGS`: A thunk that generates additional arguments to be
  passed to `gptel-request`.  This thunk should not take any parameters.

Example usage:
  (dynamic-prompt
    (lambda () \"What is the weather like?\")
    (lambda (response info) (message \"Response: %s\" response))
    (lambda () '(:system \"default system message\")))"
  (let* ((prompt
          (funcall make-prompt))
         (request-args
          (cons prompt
                (append (funcall gptel-request-args)
                        (list :callback callback)))))
    ;; (message "prompt: %s" prompt)
    ;; (message "request args: %s" request-args)
    (ensure-gptel-directives-loaded)
    (apply #'gptel-request/require request-args)))

(defmacro defgptelfn (name args &rest stx)
  (let ((docstring
         (when (stringp (car stx))
           (pop stx)))
        (stx-kwargs (-partition 2 stx))
        (make-prompt (gensym))
        (request-callback (gensym))
        (request-args-thunk (gensym)))
    `(defun ,name ,args
       ,@(when docstring (list docstring))
       ,@(awhen (assoc :command stx-kwargs)
           `((interactive ,(cadr it))))
       (let ((,make-prompt
              (lambda ()
                ,(aif (assoc :prompt stx-kwargs)
                     (cadr it)
                   (error
                    "Error - defgptelfn: missing required keyword argument :prompt"))))
             (,request-callback
              (lambda (*gptel-response* *gptel-response-info*)
                ,(aif (assoc :body stx-kwargs)
                     (cadr it)
                   (error
                    "Error - defgptelfn: missing required keyword argument :body"))))
             (,request-args-thunk
              (lambda ()
                ,@(aif (assoc :extra-args stx-kwargs)
                      it
                    (list nil)))))
         (dynamic-prompt
          ,make-prompt
          ,request-callback
          ,request-args-thunk)))))

(indent-like-defun 'defgptelfn)

(defgptelfn gptel-quick (prompt)
  "Send PROMPT to ChatGPT and display the response in a special buffer.
If the PROMPT is empty, signals a user error."
  :command (list (read-string "Ask ChatGPT: " nil gptel-quick--history))
  :prompt (progn
            (if (string= prompt "")
                (user-error "A prompt is required")
              prompt))
  :body (progn
          (if (not *gptel-response*)
              (message "gptel-quick failed with message: %s"
                       (plist-get *gptel-info* :status))
            (with-current-buffer (get-buffer-create "*gptel-quick*")
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert *gptel-response*))
              (special-mode)
              (display-buffer (current-buffer)))))
  :extra-args
  (make-gptel-system-prompt-args 'default "You are a helpful assistant."))


(defgptelfn gptel-diff ()
  "Generate a git commit message.

This function uses `magit-diff-staged` to obtain the current staged diffs
in a temporary buffer.  It then constructs a request string from these diffs
and sends it to the `gptel-request` function, which interacts with the GPT
model to generate a commit message.  Upon receiving the response, it places
the generated message in a special buffer named *gptel-diff* and copies it
to the kill ring.  If a buffer named 'COMMIT_EDITMSG' is also present, it
will switch to that buffer and notify the user that the commit message is
in the kill ring."
  :command nil
  :prompt
  (let ((diff-buffer
         (with-temp-buffer
           (magit-diff-staged)
           (buffer-name))))
    (with-current-buffer diff-buffer
      (buffer-substring-no-properties (point-min) (point-max))))
  :body
  (if (not *gptel-response*)
      (message
       "gptel-diff failed with message: %s"
       (plist-get *gptel-request-info* :status))
    (kill-new *gptel-response*)
    (with-current-buffer (get-buffer-create "*gptel-diff*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert *gptel-response*))
      (special-mode)
      (display-buffer (current-buffer)))
    (when (get-buffer "COMMIT_EDITMSG")
      (message "commit message in kill ring")
      (pop-to-buffer "COMMIT_EDITMSG")))
  :extra-args
  (make-gptel-system-prompt-args
   'commiter
   "Write a git commit message for this diff. Include ONLY the message.
Be terse. Provide messages whose lines are at most 80 characters"))


(defgptelfn gptel-pull-request ()
  "Generate a GitHub pull request description by diffing with origin/master.

Upon receiving the response from gptel, it places the generated message
in a special buffer named *gptel-pull-request* and copies it to the
clipboard and kill ring."
  :command nil
  :prompt
  (let ((diff-buffer
         (with-temp-buffer
           (magit-diff-range "origin/master")
           (buffer-string))))
    (concat
     "Generate a pull request description summarizing the changes:\n\n"
     diff-buffer))
  :body
  (if (not *gptel-response*)
      (message "%s failed with message: %s"
               'gptel-pull-request
               (plist-get *gptel-request-info* :status))
    (with-current-buffer (get-buffer-create "*gptel-pull-request*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert *gptel-response*))
      (special-mode)
      (pop-to-buffer (current-buffer))
      (clipboard+kill-ring-save (point-min) (point-max))
      (message "pull request body in kill ring")))
  :extra-args
  (make-gptel-system-prompt-args
   'pullrequest
   "Summarize the changes for a GitHub pull request description."))


(defvar gptel-document-symbol-at-point--history nil)

(defgptelfn gptel-document-symbol-at-point (sym)
  "Generate documentation for the symbol at point.

This function prompts the user to input a symbol, suggests the symbol at
point by default, and generates documentation for that symbol. The
generated documentation is then displayed in a special mode buffer.

Usage:
  1. Place the cursor on or near the symbol you want to document.
  2. Run the command `gptel-document-symbol-at-point`.
  3. Confirm or modify the prompted symbol name.
  4. The generated documentation will be displayed in a special mode buffer.

Arguments:
  SYM: The symbol for which documentation is to be generated.

Prompts:
  - The function reads a symbol from the user, suggesting the
symbol at point by default.

Output:
  - Displays the generated documentation in a buffer named
`*gptel-document-symbol-at-point*`.

The function leverages ChatGPT to generate high-quality documentation.

Example:
```
  (defun example-fun (arg)
     \"An example function.\"
     ;; Place the cursor here and run `M-x gptel-document-symbol-at-point`
     ;; Confirm or modify the prompted symbol name (e.g., `example-fun`)
     ;; The generated documentation will appear in the buffer
     ;; `*gptel-document-symbol-at-point*`
     (message \"Example argument: %s\" arg))

History:
  The function maintains a history of user inputs for the symbol prompt."
  :command
  (list
   (read-string "Documentation for: "
                (symbol-name (symbol-at-point))
                gptel-document-symbol-at-point--history))
  :prompt
  (progn
    (format
     "Add documentation for %s defined below:\n%s"
     sym
     (buffer-string)))
  :body
  (if (not *gptel-response*)
      (message "%s failed with message: %s"
               'gptel-document-symbol-at-point
               (plist-get *gptel-response-info* :status))
    (with-current-buffer (get-buffer-create "*gptel-document-symbol-at-point*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert *gptel-response*))
      (special-mode)
      (display-buffer (current-buffer))))
  :extra-args
  (make-gptel-system-prompt-args 'documentation "Prefer making a docstring"))


(defmacro with-gptel-directives (&rest body)
  "Ensures GPTel directives are loaded before executing BODY.

This macro guarantees that the GPTel directives are available by
reloading them if necessary before evaluating the BODY.

Example usage:
  (with-gptel-directives
    (do-something-with-directives))

Arguments:
  BODY -- The body of code to execute after ensuring directives are loaded.

Returns:
  The result of evaluating BODY."
  `(progn
     (ensure-gptel-directives-loaded)
     ,@body))

(defun make-gptel-system-prompt-args (directive default-directive)
  "Generate system prompt arguments for GPTel requests.

This function ensures GPTel directives are available and retrieves the
system directive for the specified DIRECTIVE.  If the directive is not
found, it uses the DEFAULT-DIRECTIVE.

Example usage:
  (make-gptel-system-prompt-args 'testing \"Default directive text\")

Arguments:
  DIRECTIVE -- The directive key to look up in the GPTel directives.
  DEFAULT-DIRECTIVE -- The fallback directive text to use if DIRECTIVE
  is not found.

Returns:
  A list containing the system directive to be used for GPTel requests."
  (with-gptel-directives
   (list
    :system (alist-get directive gptel-directives default-directive))))


(defvar gptel-tests-for-symbol-at-point nil
  "History for `gptel-tests-for-symbol-at-point`.")

(defgptelfn gptel-tests-for-symbol-at-point (sym)
  :command
  (list
   (read-string "Make tests for: "
                (symbol-name (symbol-at-point))
                gptel-document-symbol-at-point--history))
  :prompt
  (progn
    (format
     "Add test cases for %s defined below:\n%s"
     sym
     (buffer-string)))
  :body
  (if (not *gptel-response*)
      (message "%s failed with message: %s"
               'gptel-document-symbol-at-point
               (plist-get *gptel-response-info* :status))
    (with-current-buffer (get-buffer-create "*gptel-tests-for-symbol-at-point*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert *gptel-response*))
      (special-mode)
      (display-buffer (current-buffer))))
  :extra-args
  (make-gptel-system-prompt-args
   'testing
   "Generate test cases using the idiomatic language features and libraries
for the code provided"))


(defun insert-issue-prefix ()
  (interactive)
  (unless (fboundp 'magit-get-current-branch)
    (require 'magit))
  (let* ((current-branch
          (magit-get-current-branch))
         (issue-number
          (infer-issue-number-from-branch-name current-branch)))
    (unless (or (issue-prefix-is-there)
                (string= issue-number ""))
      (goto-char (point-min))
      (insert (format "[#%s] " issue-number)))))

(defun tree-to-buffer (&optional directory tree-args)
  "Run 'tree' on DIRECTORY (defaults to current buffer's directory) with TREE-ARGS and dump output into a buffer.
Checks if 'tree' is installed first.

With prefix argument, prompts for DIRECTORY and TREE-ARGS."
  (interactive
   (if current-prefix-arg
       (list (read-directory-name "Directory for tree: " default-directory)
             (read-string "Arguments for tree (e.g., -L 2 -a): "))
     (list nil nil)))
  (if (executable-find "tree")
      (let* ((dir (expand-file-name (or directory default-directory)))
             (args (or tree-args ""))
             (buffer (generate-new-buffer "*tree output*")))
        (with-current-buffer buffer
          (insert (shell-command-to-string
                   (string-join (list "tree" args (shell-quote-argument dir)) " ")))
          (read-only-mode 1))
        (pop-to-buffer buffer))
    (error "The 'tree' command is not installed")))

(defun infer-issue-number-from-branch-name (branch-name)
  "Gets the implied issue number out of BRANCH-NAME."
  (if (string-match "\\([[:alpha:]]+\\)\\([[:digit:]]+\\).*" branch-name)
      (match-string 2 branch-name)
    (progn
      (message "failed to infer branch name")
      "")))

(defun issue-prefix-is-there ()
  "Check if the buffer is prefixed by the issue prefix [#ISSUE-NUMBER]."
  (let ((buffer-prefix (car (split-string (buffer-string)))))
    (string-match "\\[\\#[[:digit:]]+\\]" buffer-prefix)))

(defun clang-format-on-save ()
  "Add clang-format-buffer to before-save-hook for the current buffer."
  (when (executable-find "clang-format")
    (add-hook 'before-save-hook #'clang-format-buffer nil 'local)))

(defun my/c-family-inferred-build-system ()
  "Return the C-family build system inferred from the current directory.
Checks for \"CMakeLists.txt\", \"autoconf.ac\" and \"Makefile\".
Returns one of the symbols: 'cmake, 'autotools, 'make, or 'unknown."
  (cond
   ((locate-dominating-file default-directory "CMakeLists.txt") 'cmake)
   ((locate-dominating-file default-directory "autoconf.ac")    'autotools)
   ((locate-dominating-file default-directory "Makefile")      'make)
   (t                                                           'unknown)))

(defun my/c-family-project-root ()
  "Return the root directory of the current C-family project.
Checks for an LSP workspace root, or the nearest directory containing
.git, CMakeLists.txt, autoconf.ac, or Makefile."
  (or (when (fboundp 'lsp-workspace-root) (lsp-workspace-root))
      (locate-dominating-file default-directory ".git")
      (locate-dominating-file default-directory "CMakeLists.txt")
      (locate-dominating-file default-directory "autoconf.ac")
      (locate-dominating-file default-directory "Makefile")))

(defun my/cmake-configure-command (project-root-dir)
  "Return a CMake configure command for PROJECT-ROOT-DIR."
  (format "cmake -S %s -B %s -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
          project-root-dir
          (file-name-concat project-root-dir "build")))

(defun my/cmake-compile-command (project-root-dir)
  "Return a CMake build command for PROJECT-ROOT-DIR."
  (format "cmake --build %s"
          (file-name-concat project-root-dir "build")))

(defun my/cmake-test-command (project-root-dir)
  "Return a CTest command for PROJECT-ROOT-DIR."
  (format "ctest --test-dir %s --output-on-failure"
          (file-name-concat project-root-dir "build")))

(defun my/c-family-infer-configure-command ()
  "Infer and return the configure command for the current C-family project."
  (let ((root   (my/c-family-project-root))
        (system (my/c-family-inferred-build-system)))
    (if root
        (cond
         ((eq system 'cmake) (my/cmake-configure-command root))
         (t (error "my/c-family-infer-configure-command: %s not supported" system)))
      (error "my/c-family-infer-configure-command: unable to infer project root"))))

(defun my/c-family-infer-build-command ()
  "Infer and return the build command for the current C-family project."
  (let ((root   (my/c-family-project-root))
        (system (my/c-family-inferred-build-system)))
    (if root
        (cond
         ((eq system 'cmake) (my/cmake-compile-command root))
         (t (error "my/c-family-infer-build-command: %s not supported" system)))
      (error "my/c-family-infer-build-command: unable to infer project root"))))

(defun my/c-family-infer-test-command ()
  "Infer and return the test command for the current C-family project."
  (let ((root   (my/c-family-project-root))
        (system (my/c-family-inferred-build-system)))
    (if root
        (cond
         ((eq system 'cmake) (my/cmake-test-command root))
         (t (error "my/c-family-infer-test-command: %s not supported" system)))
      (error "my/c-family-infer-test-command: unable to infer project root"))))

(defun my/c-family-configure ()
  "Run the configure command for the current C-family project."
  (interactive)
  (compile (my/c-family-infer-configure-command)))

(defun my/c-family-build ()
  "Run the build command for the current C-family project."
  (interactive)
  (compile (my/c-family-infer-build-command)))

(defun my/c-family-test ()
  "Run the test command for the current C-family project."
  (interactive)
  (compile (my/c-family-infer-test-command)))

(provide 'init)
;;; init.el ends here
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line

(let ((opam-user-setup-file
       "~/.emacs.d/opam-user-setup.el"))
  (when (file-exists-p opam-user-setup-file)
    (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")))

;; ## end of OPAM user-setup addition for emacs / base ## keep this line
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-file-watch-ignored-directories
   '("[/\\\\]\\.venv$" "[/\\\\]venv$" "[/\\\\]\\.direnv$" "[/\\\\]\\.mypy_cache$" "[/\\\\]\\.pytest_cache$" "[/\\\\]__pycache__$" "[/\\\\]build$" "[/\\\\]dist$") nil nil "Customized with use-package lsp-mode")
 '(safe-local-variable-values '((project-root . "."))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda --emacs-mode locate")))
