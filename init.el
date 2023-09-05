;;; init.el --- Emacs configuration.

;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-pkg)
(require 'init-keymap)
(require 'init-sys)
(require 'init-ui)

(require 'ui-tabs)

(use-package vterm
  :bind
  (:map vterm-mode-map
        ("C-c C-j" . vterm-copy-mode))
  (:map vterm-copy-mode-map
        ("C-c C-j" . vterm-copy-mode)))

(require 'user-org)
(require 'user-vc)
(require 'user-window)
(require 'user-editing)
(require 'editing-yasnippet)
(require 'user-dired)
(require 'user-minibuffer)

;; programming
(require 'user-coding-common)
(require 'user-c-lang)
(require 'user-c++-lang)
(require 'user-config-lang)
(require 'user-elisp-lang)
(require 'user-python-lang)

(require 'init-local)
(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/Users/sayeef/org/learncpp.org" "/Users/sayeef/org/quick.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
