;;; user-config-lang.el ---

;;; Commentary:

;;; Code:

(add-to-list 'auto-mode-alist '("\\.toml\\'" . conf-mode))

(add-hook 'conf-mode-hook 'display-line-numbers-mode)

(use-package markdown-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook 'display-line-numbers-mode))

(use-package yaml-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  :config
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode))

(provide 'user-config-lang)
;;; user-config-lang.el ends here
