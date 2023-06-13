;;; init-expand-region.el --- Select region based on semantic units
;;; Commentary:
;;; Code:

(use-package expand-region
  :bind
  (:map u-map ("C-e" . er/expand-region)))

(provide 'init-expand-region)

;;; init-expand-region.el ends here
