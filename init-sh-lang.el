;;; init-sh-lang.el --- Sh support
;;; Commentary:
;;; Code:

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
  (add-hook 'sh-mode-hook 'flymake-mode))

(provide 'init-sh-lang)

;;; init-sh-lang.el ends here
