;;; init-company.el --- Completion
;;; Commentary:
;;; Code:

(use-package company
  :hook (prog-mode conf-mode)
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0))

(provide 'init-company)

;;; init-company.el ends here
