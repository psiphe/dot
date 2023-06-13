;;; init-markdown-lang.el --- Markdown support
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook 'display-line-numbers-mode))

(provide 'init-markdown-lang)

;;; init-markdown-lang.el ends here
