;;; user-minibuffer.el ---

;;; Commentary:

;;; Code:

(setq completion-styles '(basic partial-completion flex))

;; Show documentation in the minibuffer margin.
(use-package marginalia
  :hook (emacs-startup))

;; Minibuffer narrowing.
(use-package vertico
  :hook (emacs-startup))

(provide 'user-minibuffer)
;;; user-minibuffer.el ends here
