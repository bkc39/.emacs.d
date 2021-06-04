;;; layout.el --- setting the buffer layouts for bot lab dev work
;;; Commentary:

;;; Code:

(defvar bllp:bllp-platform-dir
  "/home/bkc/botlab/platform/"
  "Directory where the bllp-platform repository is located")

(defun bllp:platform-layout ()
  "Sets the default window layout for bllp-platform dev work."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (split-window-below)
  (other-window 2)
  (split-window-below)
  (other-window 2)
  (split-window-below)
  (balance-windows)
  (other-window 2)

  (bllp:layout-buffer-with-name
   "magit: platform"
   #'(lambda ()
       (magit-status bllp:bllp-platform-dir)))
  (other-window 1)

  (let ((plaform-shell-buffer-name "*platform: shell*"))
    (bllp:layout-buffer-with-name
     plaform-shell-buffer-name
     #'(lambda ()
         (bllp:open-shell-in-dir-with-name
          bllp:bllp-platform-dir
          plaform-shell-buffer-name))))
  (other-window-1))

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

(provide 'layout)
