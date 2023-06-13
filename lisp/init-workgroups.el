;;; init-workgroups.el --- Save buffer/file arrangements
;;; Commentary:
;;; Code:

(use-package workgroups2
  :bind
  (:map u-map
        ("w c" . wg-create-workgroup)
        ("w k" . wg-kill-workgroup)
        ("w f" . wg-open-workgroup)))

(provide 'init-workgroups)

;;; init-workgroups.el ends here
