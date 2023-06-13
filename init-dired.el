;;; init-dired.el --- dired mode
;;; Commentary:
;;; Code:

(use-package dired
  :straight (:type built-in)
  :bind
  (:map dired-mode-map
        ("p" . dired-up-directory)))

(provide 'init-dired)

;;; init-dired.el ends here
