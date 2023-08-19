;;; user-dired.el --- Dired & extensions.

;;; Commentary:

;;; Code:

(use-package dired
  :straight (:type built-in)
  :bind
  (:map dired-mode-map
        ("p" . dired-up-directory)))

(provide 'user-dired)
;;; user-dired.el ends here
