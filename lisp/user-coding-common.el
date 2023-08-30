;;; user-coding-common.el ---

;;; Commentary:

;;; Code:

(setq-default compilation-scroll-output 'first-error
              fill-column 80)

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; A grep-based xref backend.
(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate))

;; On-the-fly syntax checking & linting.
;; Enable flymake on a per-language basis.
(use-package flymake
  :straight (:type built-in)
  :bind
  (:map u-map
        ("e n" . flymake-goto-next-error)
        ("e p" . flymake-goto-prev-error)
        ("e l" . flymake-show-buffer-diagnostics)))

;; Highlight typical todo keywords (FIXME, TODO, etc.)
(use-package hl-todo
  :hook (prog-mode))

;; Tree-sitter powered syntax highlighting + semantic analysis.
;; TODO: lazy load tree-sitter.
(use-package tree-sitter
  :hook (emacs-startup . global-tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode))

;; Tree-sitter backend bundle.
(use-package tree-sitter-langs
  :after (tree-sitter))

(provide 'user-coding-common)
;;; user-coding-common.el ends here
