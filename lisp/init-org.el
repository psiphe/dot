;;; init-org.el ---
;;; Commentary:
;;; Code:

(use-package org
  :straight (:type built-in)
  :bind
  (:map u-map/org
        ("a" . org-agenda)
        ("c" . org-capture))
  (:map org-mode-map
        ("C-j" . nil)) ; u-map prefix key
  :config
  (setq org-agenda-files (list org-directory)
        org-hide-emphasis-markers t
        org-babel-default-header-args (cons '(:result . "drawer") (assq-delete-all :result org-babel-default-header-args)) ; replace output on execute
        org-confirm-babel-evaluate nil
        org-default-notes-file (concat org-directory "/notes.org")
        org-startup-folded t)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((C . t)
                                 (emacs-lisp . t)
                                 (shell . t))))

(provide 'init-org)

;;; init-org.el ends here
