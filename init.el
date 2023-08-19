;;; init.el --- Emacs configuration.

;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-pkg)
(require 'init-keymap)
(require 'init-sys)
(require 'init-ui)

(require 'user-org)
(require 'user-vc)
(require 'user-window)
(require 'user-editing)
(require 'user-dired)
(require 'user-minibuffer)

;; programming
(require 'user-coding-common)
(require 'user-c-lang)
(require 'user-config-lang)
(require 'user-elisp-lang)
(require 'user-python-lang)

(require 'init-local)
(provide 'init)
;;; init.el ends here
