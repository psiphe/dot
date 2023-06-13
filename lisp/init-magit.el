;;; init-magit.el --- Git UI
;;; Commentary:
;;; Code:

(use-package magit
  :bind
  (:map u-map/vc
        ("s" . magit-status)))

(provide 'init-magit)

;;; init-magit.el ends here
