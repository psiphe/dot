;;; user-org.el ---

;;; Commentary:

;;; Code:

(use-package org
  :defer t
  :straight (:type built-in)
  :config
  (setq org-hide-emphasis-markers t
        org-pretty-entities t
        org-startup-folded t
        org-todo-keywords '((sequence
                             "TODO(t)" "CURR(c)" "WAIT(w)" "REVW(r)" "DEPL(s)"
                             "|"
                             "DONE(d)")))
  :bind
  (:map org-mode-map
        ("C-j" . nil)))

;; Eye-candy for org.
(use-package org-modern
  :hook (org-mode))

;; Managed notes (a zettelkasten mode).
(use-package org-roam
  :commands org-roam-node-find
  :config
  (let* ((roam-dir org-directory))
    (unless (file-exists-p roam-dir)
      (make-directory roam-dir))
    (setq org-roam-directory (file-truename roam-dir))
    (org-roam-db-autosync-mode))
  :bind
  (:map u-map
        ("o n" . org-roam-node-find)))

(provide 'user-org)
;;; user-org.el ends here
