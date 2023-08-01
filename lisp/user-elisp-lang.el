;;; user-elisp-lang.el ---

;;; Commentary:

;;; Code:

(add-hook 'emacs-lisp-mode-hook 'flymake-mode)
(define-key lisp-interaction-mode-map (kbd "C-j") nil) ; u-map prefix conflict

;; Make parentheses less visible for lisp.
(use-package paren-face
  :hook
  (emacs-lisp-mode))

(provide 'user-elisp-lang)
;;; user-elisp-lang.el ends here
