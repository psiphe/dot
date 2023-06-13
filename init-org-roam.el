;;; init-org-roam.el --- Enhanced notes with org
;;; Commentary:
;;; Code:

(use-package org-roam
  :bind
  (:map u-map/org
        ("n" . org-roam-node-find))
  :config
  (let* ((roam-dir org-directory))
    (unless (file-exists-p roam-dir)
      (make-directory roam-dir))
    (setq org-roam-directory (file-truename roam-dir))
    (org-roam-db-autosync-mode)))

(provide 'init-org-roam)

;;; init-org-roam.el ends here
