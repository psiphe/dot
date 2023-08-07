;;; user-vc.el --- Version control

;;; Commentary:

;;; Code:

;; Show version control changes in the margin.
(use-package diff-hl
  :hook (prog-mode)
  :config
  (add-hook 'diff-hl-mode-hook 'diff-hl-margin-mode)
  (setq diff-hl-show-staged-changes nil)
  :bind
  (:map u-map/vc
        ("a" . diff-hl-stage-current-hunk)
        ("n" . diff-hl-next-hunk)
        ("p" . diff-hl-previous-hunk)))

;; Visit previously committed versions of the current file.
(use-package git-timemachine
  :bind
  (:map u-map/vc
        ("t" . git-timemachine)))

;; Git UI.
(use-package magit
  :defer t
  :init
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

;; Show TODOs in the magit status window.
(use-package magit-todos
  :hook (magit-mode))

(provide 'user-vc)
;;; user-vc.el ends here
