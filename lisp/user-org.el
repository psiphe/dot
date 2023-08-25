;;; user-org.el ---

;;; Commentary:
;; TODO: I think use-package is loading org right away

;;; Code:

(setq org-agenda-custom-commands
           '(("w" "Work summary"
              ((agenda "d" ((org-agenda-overriding-header "\n\n")))
               (todo "CURR" ((org-agenda-overriding-header "In Progress:")))
               (todo "REVW" ((org-agenda-overriding-header "Pending Review:")))
               (todo "WAIT" ((org-agenda-overriding-header "Blocked:")))
               (todo "DEPL" ((org-agenda-overriding-header "Deploying:")))))))
(setq org-agenda-window-setup 'only-window)

(define-key u-map (kbd "C-o C-a") (lambda ()
                                    (interactive)
                                    (org-agenda nil "w")))
(setq org-agenda-prefix-format
      '((agenda . "%s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))
;; ((agenda . " %i %-12:c%?-12t% s") (todo . " %i %-12:c") (tags . " %i %-12:c")
;;  (search . " %i %-12:c"))
(use-package org
  :straight (:type built-in)
  :config
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$")
        org-agenda-block-separator ""
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
        ("s" . org-search-view)
        ("a" . org-agenda)
        ("c" . org-capture)
        ("f" . (lambda () (interactive) (ido-find-file-in-dir "~/org"))))
  (:map org-mode-map
        ("C-j" . nil)))

;; Eye-candy.
(use-package org-modern
  :init
  (with-eval-after-load 'org (global-org-modern-mode)))

(provide 'user-org)
;;; user-org.el ends here
