;;; init-elisp-lang.el --- Elisp support
;;; Commentary:
;;; Code:

(define-key lisp-interaction-mode-map (kbd "C-j") nil)
(use-package paren-face
  :hook (emacs-lisp-mode))
(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

(provide 'init-elisp-lang)

;;; init-elisp-lang.el ends here
