;;; layout.el --- setting the buffer layouts for bot lab dev work
;;; Commentary:

;;; Code:

(defvar bllp:bllp-platform-dir
  "/home/bkc/botlab/platform/"
  "Directory where the bllp-platform repository is located")

(defun bllp:platform-layout ()
  "Sets the default window layout for bllp-platform dev work."
  (interactive)

  (bllp:layout-standard-six-windows)

  (bllp:layout-buffer-with-name
   "magit: platform"
   #'(lambda ()
       (magit-status bllp:bllp-platform-dir)))
  (other-window 1)

  (bllp:layout-buffer-with-name
   "magit-log: platform"
   #'(lambda ()
       (magit-status bllp:bllp-platform-dir)
       (magit-log-head)))
  (other-window 1)

  (bllp:layout-last-visited-haskell-files)

  (bllp:layout-file-in-current-buffer (concat bllp:bllp-platform-dir
                                              "src/BotLab"))
  (other-window 1)

  (let ((plaform-shell-buffer-name "*platform: shell*"))
    (bllp:layout-buffer-with-name
     plaform-shell-buffer-name
     #'(lambda ()
         (bllp:open-shell-in-dir-with-name
          bllp:bllp-platform-dir
          plaform-shell-buffer-name))))
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
      (open-buffer-thunk (open-buffer-thunk))
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
      (delete-window)
      (bllp:layout-file-in-current-buffer
       (concat bllp:bllp-platform-dir
               "src/BotLab/Prelude.hs"))
      (other-window 1))
     ((= 1 num-haskell-files-visited)
      ;; one haskell file was open. Open it and delete one window
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

(provide 'layout)
