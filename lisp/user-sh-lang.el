;;; user-sh-lang.el ---

;;; Commentary:

;;; Code:

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
  (add-hook 'sh-mode-hook 'flymake-mode))

(provide 'user-sh-lang)
;;; user-sh-lang.el ends here
