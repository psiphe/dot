;;; init-local.el --- Load machine local config.

;;; Commentary:
;; Load all of the `.el` files in $HOME/.config/emacs.

;;; Code:

(let* ((local-config-dir "~/.config/emacs"))
  (unless (file-exists-p local-config-dir)
    (make-directory local-config-dir))
  (cl-loop for file in (directory-files-recursively local-config-dir "\\.el$")
           do (condition-case err
                  (load file)
                ('error (message (format "failed to load local config file: %s %s" file err))))))

(provide 'init-local)
;;; init-local.el ends here
