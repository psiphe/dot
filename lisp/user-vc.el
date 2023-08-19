;;; user-vc.el --- Version control.

;;; Commentary:

;;; Code:

;; Visit previously committed versions of the current file.
(use-package git-timemachine
  :config
  (add-hook 'git-timemachine-mode-hook 'display-line-numbers-mode)
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
  :after (magit)
  :hook (magit-mode))

(provide 'user-vc)
;;; user-vc.el ends here
