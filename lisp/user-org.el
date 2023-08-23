;;; user-org.el ---

;;; Commentary:
;; TODO: I think use-package is loading org right away

;;; Code:

(use-package org
  :straight (:type built-in)
  :config
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$")
        org-capture-templates '(("q" "Quick" entry (file+headline org-default-notes-file "Spam")
                                 "** TODO %?\n:PROPERTIES:\n:Source:%a\n:END:\nDEADLINE: %t\n%i\n")
                                ("i" "Idea" entry (file+headline org-default-notes-file "Ideas")
                                 "** IDEA %?\n:PROPERTIES\n:Source:%a\n:END:\n%i\n")
                                ("t" "Todo" entry (file+headline org-default-notes-file "Todo")
                                 "** TODO %?\n:PROPERTIES:\n:Source:%a\n:END:\n%i\n"))
        org-default-notes-file (concat org-directory "/quick.org")
        org-pretty-entities t
        org-startup-folded t
        org-todo-keywords '((sequence
                             "IDEA(i)" "TODO(t)" "CURR(c)" "WAIT(w)" "REVW(r)" "DEPL(s)"
                             "|"
                             "DONE(d)")))
  :bind
  (:map u-map/org
        ("a" . org-agenda)
        ("c" . org-capture)
        ("f" . (lambda () (interactive) (ido-find-file-in-dir "~/org"))))
  (:map org-mode-map
        ("C-j" . nil)))

;; Eye-candy.
(use-package org-modern
  :hook (org-mode))

(provide 'user-org)
;;; user-org.el ends here
