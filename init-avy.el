;;; init-avy.el --- Tree-based cursor movement
;;; Commentary:
;;; Code:

(use-package avy
  :bind
  (:map u-map
        ("C-f" . avy-goto-char-timer)
        ("C-l" . avy-goto-line)
        ("c l" . avy-copy-line)
        ("c r" . avy-copy-region)
        ("k l" . avy-kill-whole-line)
        ("k r" . avy-kill-region)))

(provide 'init-avy)

;;; init-avy.el ends here
