;;; layout.el --- setting the buffer layouts for bot lab dev work
;;; Commentary:

;;; Code:

(defvar bllp:bllp-platform-dir
  "/home/bkc/botlab/platform/"
  "Directory where the bllp-platform repository is located")

(defvar bllp:bllp-ludus-dir
  "/home/bkc/botlab/ludus/"
  "Directory where the BotLab ludus repository is located")

(defun bllp:platform-layout ()
  "Sets the default window layout for bllp-platform dev work."
  (interactive)

  (magit-status bllp:bllp-platform-dir)
  (magit-log-head)
  (let ((platform-shell-buffer-name "*platform-shell*"))
    (bllp:open-shell-in-dir-with-name
     bllp:bllp-platform-dir
     platform-shell-buffer-name))

  (bllp:layout-standard-six-windows)

  (bllp:layout-buffer-with-name "magit: platform")
  (other-window 1)

  (bllp:layout-buffer-with-name "magit-log: platform")
  (other-window 1)

  (bllp:layout-last-visited-haskell-files)

  (bllp:layout-file-in-current-buffer
   (concat bllp:bllp-platform-dir "src/BotLab"))
  (other-window 1)

  (bllp:layout-buffer-with-name "*platform-shell*")
  (other-window 3))

(defun bllp:trading-layout ()
  "Sets the default window layout for trading in the Bot Lab
  ludus repository."
  (interactive)

  (magit-status bllp:bllp-ludus-dir)
  (magit-log-head)
  (let ((ludus-shell-buffer-name "*ludus-shell*"))
    (bllp:open-shell-in-dir-with-name
     bllp:bllp-ludus-dir
     ludus-shell-buffer-name))

  ;; check if R has a shell open in ludus already and opens one if
  ;; not.
  (unless (get-buffer "*R:ludus*")
    (bllp:start-R-in-ludus))

  (bllp:layout-standard-six-windows)

  (bllp:layout-buffer-with-name "*R:ludus*")
  (other-window 1)

  (bllp:layout-buffer-with-name "*ludus-shell*")
  (other-window 1)

  (bllp:layout-file-in-current-buffer
   (concat bllp:bllp-ludus-dir
           "bkc/pcdata.R"))
  (other-window 1)

  (bllp:layout-file-in-current-buffer
   (concat bllp:bllp-ludus-dir
           "bkc/mosekffi/integer_markowitz.py"))
  (other-window 1)

  (bllp:layout-file-in-current-buffer
   (concat bllp:bllp-ludus-dir
           "bkc/mosekffi/sim.R"))
  (other-window 1)

  (bllp:layout-buffer-with-name "magit: ludus")
  (other-window 1))

(defun bllp:layout-standard-six-windows ()
  "Splits the current frame into six windows laid out in a 2x3
  grid. Puts the cursor in the top left window."
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (split-window-below)
  (other-window 2)
  (split-window-below)
  (other-window 2)
  (split-window-below)
  (balance-windows)
  (other-window 2))

(defun bllp:layout-file-in-current-buffer (filename)
  "Opens the file file in the current buffer."
  (let ((buffer-visiting-file (find-file filename)))
    (switch-to-buffer buffer-visiting-file)))

(defun bllp:layout-buffer-with-name (buffer-name &optional open-buffer-thunk)
  "Opens the buffer BUFFER-NAME in the current window. If
  BUFFER-NAME is not open then call OPEN-BUFFER-THUNK and open
  the buffer it returns in the current window. If
  OPEN-BUFFER-THUNK is nil then open OTHER-BUFFER instead."
  (switch-to-buffer
   (let ((buffer-to-open (get-buffer buffer-name)))
     (cond
      (buffer-to-open buffer-to-open)
      (open-buffer-thunk (funcall open-buffer-thunk))
      (t
       (other-buffer))))))

(defun bllp:open-shell-in-dir-with-name (dir shell-name)
  "Opens a shell in the working directory DIR with the buffer
  name SHELL-NAME."
  (let ((default-directory dir))
    (shell shell-name)))

(defun bllp:is-visiting-haskell-file-p (buffer)
  "Predicate that it non-nil it BUFFER is visiting a Haskell file
  and nil otherwise."
  (let ((file-visited (buffer-file-name buffer)))
    (if file-visited
        (string= "hs"
                 (file-name-extension file-visited))
      nil)))

(defun bllp:layout-last-visited-haskell-files ()
  "Returns the last two buffers visiting Haskell files, if
  available. If there are no active buffers visiting a haskell
  file then BLLP:BLLP-PLATFORM-DIR/src/BotLab/Prelude.hs will be
  opened."
  (let* ((buffers-visiting-haskell-files
          (seq-filter #'bllp:is-visiting-haskell-file-p
                      (buffer-list)))
         (num-haskell-files-visited
          (length buffers-visiting-haskell-files)))
    (cond
     ((= 0 num-haskell-files-visited)
      ;; no haskell files were open. Open Prelude.hs and delete a
      ;; window
      (other-window 1)
      (delete-window)
      (bllp:layout-file-in-current-buffer
       (concat bllp:bllp-platform-dir
               "src/BotLab/Prelude.hs"))
      (other-window 1))
     ((= 1 num-haskell-files-visited)
      ;; one haskell file was open. Open it and delete one window
      (other-window 1)
      (delete-window)
      (switch-to-buffer (car buffers-visiting-haskell-files))
      (other-window 1))
     (t
      ;; at least two files were open. Open them in the next two buffers
      (let ((first-buffer (car buffers-visiting-haskell-files))
            (second-buffer (cadr buffers-visiting-haskell-files)))
        (switch-to-buffer first-buffer)
        (other-window 1)
        (switch-to-buffer second-buffer)
        (other-window 1))))))

(defun bllp:start-R-in-ludus ()
  "Start an ESS R process in BLLP:BLLP-LUDUS-DIR. This will be in
a buffer with BUFFER-NAME set to *R:ludus*."
  (unwind-protect
      (progn
        (setq ess-ask-for-ess-directory nil)
        (setq ess-directory bllp:bllp-ludus-dir)
        (setq ess-gen-proc-buffer-name-function
              #'(lambda (proc-name)
                  (concat "*"
                          proc-name
                          ":ludus*")))
        (run-ess-r))
    (setq ess-ask-for-ess-directory t)
    (setq ess-directory nil)
    (setq ess-gen-proc-buffer-name 'ess-gen-proc-buffer-name:project-or-simple)))

(provide 'bllp-layout)
