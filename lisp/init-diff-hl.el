;;; init-diff-hl.el --- Git diff in the margin
;;; Commentary:
;;; Code:

(use-package diff-hl
  :hook (prog-mode)
  :bind
  (:map u-map/vc
        ("a" . diff-hl-stage-current-hunk)
        ("n" . diff-hl-next-hunk)
        ("p" . diff-hl-previous-hunk))
  :init
  (global-diff-hl-mode 1)
  :config
  (setq diff-hl-show-staged-changes nil)
  (add-hook 'diff-hl-mode-hook 'diff-hl-margin-mode))

(provide 'init-diff-hl)

;;; init-diff-hl.el ends here
