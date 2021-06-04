;;; layout.el --- setting the buffer layouts for bot lab dev work
;;; Commentary:

;;; Code:

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
  (other-window 2))

(provide 'layout)
