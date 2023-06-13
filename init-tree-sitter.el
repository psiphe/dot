;;; init-tree-sitter.el --- Better syntax highlighting
;;; Commentary:
;;; Code:

(use-package tree-sitter
  :hook (emacs-startup . global-tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after (tree-sitter))

(provide 'init-tree-sitter)

;;; init-tree-sitter.el ends here
