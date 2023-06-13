;;; init-yaml-lang.el --- YAML support
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  :config
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode))

(provide 'init-yaml-lang)

;;; init-yaml-lang.el ends here
