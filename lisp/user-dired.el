;;; user-dired.el --- Dired & extensions.

;;; Commentary:

;;; Code:

(use-package dired
  :straight (:type built-in)
  :bind
  (:map dired-mode-map
        ("p" . dired-up-directory)))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  (dirvish-side-follow-mode)
  (setq dirvish-side-width 25)
  :bind
  (:map u-map
        ("C-v f" . dirvish-side))
  (:map dirvish-mode-map
        ("'" . dirvish-layout-toggle)))

(provide 'user-dired)
;;; user-dired.el ends here
