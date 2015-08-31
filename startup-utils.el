;;; startup-utils.el --- utility functions used in emacs startup
;;; Commentary:
;;;   Mostly helper functions to reduce clutter in init.el. This
;;;   module is loaded at the top of init-el.

;;; Code:
(with-no-warnings (require 'cl))

(defun recursively-add-directory-to-load-path (dir &optional debug)
  "Add DIR and all subdirectories to LOAD-PATH.

If DEBUG is non-nil then the added directories are logged in the
*Messages* buffer for easy path debugging."
  (let ((directories-to-add (list (expand-file-name dir))))
    (while directories-to-add
      (let* ((current-directory (car directories-to-add))
             (current-directory-contents
              (mapcar
               #'(lambda (subdirectory)
                   (concat current-directory "/" subdirectory))
               (directory-files current-directory)))
             (subdirectories
              (remove-if-not
               #'(lambda (file)
                   (and (file-directory-p file)
                        (file-readable-p file)
                        (not (string-equal "." (substring file -1)))
                        (not (string-equal "." (substring file -2)))))
               current-directory-contents)))
        (when debug
          (message "Adding directory: %s %s"
                   current-directory
                   "to load path"))
        (add-to-list 'load-path current-directory)
        (setq directories-to-add
              (append (cdr directories-to-add)
                      subdirectories))))))

(defun system-is-mac ()
  "Determines if the system type is a mac."
  (eq system-type 'darwin))

(defun system-is-linux ()
  "Determines if the system type is a mac."
  (eq system-type 'gnu/linux))

(defun startup-utils:byte-compile-all ()
  "Byte compile all files in the Emacs home directory."
  (interactive)
  (byte-recompile-file (expand-file-name "~/.emacs.d/startup-utils.el")
                       nil 0 nil)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/elisp")
                            0 nil))

(provide 'startup-utils)
;;; startup-utils.el ends here
