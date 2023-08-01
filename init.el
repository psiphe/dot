;;; init.el --- Emacs configuration.

;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-pkg)
(require 'init-keymap)
(require 'init-sys)
(require 'init-ui)

(require 'user-dired)
(require 'user-org)
(require 'user-vc)
(require 'user-window)
(require 'user-editing)
(require 'user-minibuffer)

(require 'user-coding-common)
(require 'user-c-lang)
(require 'user-config-lang)
(require 'user-elisp-lang)
(require 'user-python-lang)

;; Load all of the `.el` files in $HOME/.config/emacs - I use this for (machine)
;; local configuration.
(let* ((local-config-dir "~/.config/emacs"))
  (unless (file-exists-p local-config-dir)
    (make-directory local-config-dir))
  (cl-loop for file in (directory-files-recursively local-config-dir "\\.el$")
           do (condition-case err
                  (load file)
                ('error (message (format "failed to load local config file: %s %s" file err))))))

(provide 'init)
;;; init.el ends here
