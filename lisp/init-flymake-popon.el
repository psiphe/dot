;;; init-flymake-popon --- flymake diagnostic overlays
;;; Commentary:
;;; Code:

(use-package flymake-popon
  :straight (:type git :repo "https://codeberg.org/akib/emacs-flymake-popon.git")
  :after (flymake)
  :hook (flymake-mode))

(provide 'init-flymake-popon)

;;; init-flymake-popon.el ends here
