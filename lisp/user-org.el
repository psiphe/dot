;;; user-org.el --- Org & co.
;;; Commentary:
;; TODO: capture from code should add a tag or label with the project
;;; Code:

(use-package org
  :straight (:type built-in)
  :config
  (org-indent-mode)
  (org-clock-persistence-insinuate)
  (add-hook 'org-after-todo-state-change-hook
            (lambda ()
              (if (string= org-state "WIP")
                  (org-clock-in)
                (org-clock-out))))
  (setq org-archive-subtree-save-file-p t
        org-clock-into-drawer "TIME"
        org-clock-persist t
        org-default-notes-file (concat org-directory "/mailbox.org")
        org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t
        org-hide-leading-stars t
        org-log-done 'note
        org-log-into-drawer "LOGBOOK"
        org-startup-folded t
        org-todo-keywords '((sequence "TODO" "WIP(@)" "REVIEW(@)" "DEPLOYING(@)" "|" "DONE")))
  :bind
  (:map org-mode-map
        ("C-j" . nil)
        ("C-j C-a" . org-archive-subtree)
        ("C-j C-w" . org-toggle-narrow-to-subtree)
        ("C-j C-q" . org-todo)
        ("C-j RET" . org-insert-heading-respect-content)
        ("C-j i ." . org-time-stamp)
        ("C-j i d" . org-deadline)
        ("C-j i l" . org-set-tags-command)
        ("C-j i n" . org-add-note)
        ("C-j i p" . org-priority)
        ("C-j i s" . org-schedule)
        ("C-j i t" . org-table-create-or-convert-from-region))
  (:map u-map/org
        ("f" . (lambda () (interactive) (ido-find-file-in-dir org-directory)))
        ("w" . org-clock-goto)))

(use-package org-agenda
  :straight (:type built-in)
  :config
  (setq org-agenda-block-separator ""
        org-agenda-custom-commands
        '(("i" "Ideas" tags "+idea"
           ((org-agenda-overriding-header "Ideas")))
          ("o" "Overview" ((agenda "d"
                                   ((org-agenda-overriding-header "\n")
                                    (org-agenda-span 'week)))
                           (todo "WIP"
                                 ((org-agenda-overriding-header "In Progress:")))
                           (todo "REVW"
                                 ((org-agenda-overriding-header "In Review:")))
                           (todo "DEPLOYING"
                                 ((org-agenda-overriding-header "Deploying:"))))))
        org-agenda-deadline-leaders '("Due today:      " "Due in %d days:  " "")
        org-agenda-entry-types '(:deadline :scheduled)
        org-agenda-files (directory-files-recursively org-directory "\\.org$")
        org-agenda-prefix-format '((agenda . "%i %s") (search "%i %s") (tags . "%i %s") (todo . "%i %s"))
        org-agenda-restore-windows-after-quit t
        org-agenda-scheduled-leaders '("Scheduled:      " "")
        org-agenda-start-on-weekday 1
        org-agenda-tags-column -80
        org-agenda-window-setup 'current-window
        org-deadline-past-days 0
        org-deadline-warning-days 0)
  :bind
  (:map org-agenda-mode-map
        ("a" . org-agenda-archive)
        ("c" . org-agenda-capture)
        ("i" . nil) ; unbind org-agenda-diary
        ("i d" . org-agenda-deadline)
        ("i n" . org-agenda-add-note)
        ("i p" . org-agenda-priority)
        ("i s" . org-agenda-schedule)
        ("o" . org-agenda-tree-to-indirect-buffer))
  (:map u-map/org
        ("a" . org-agenda))
  (:map u-map/org-alt
        ("i" . (lambda () (interactive) (org-agenda nil "i")))
        ("a" . (lambda () (interactive) (org-agenda nil "o")))
        ("t" . (lambda () (interactive) (org-agenda nil "t")))))

(use-package org-capture
  :straight (:type built-in)
  :config
  (setq org-capture-templates '(("m" "Meeting Notes" entry (file+headline org-default-notes-file "Meeting Notes")
                                 "** %? %^G\nDate: %T\n%i\n")
                                ("i" "Idea" entry (file+headline org-default-notes-file "Ideas")
                                 "** %? :idea:%^G\nCreated: %T\n%i\n")
                                ("t" "Todo" entry (file+headline org-default-notes-file "Todos")
                                 "** TODO %? %^G\nCreated: %T\n%i\n")
                                ("w" "Task" entry (file+headline org-default-notes-file "Todos")
                                 "** TODO [#B] %? %^G\nSCHEDULED: %^t DEADLINE: %^t\nCreated: %T\n%i\n")))
  :bind
  (:map u-map/org
        ("c" . org-capture))
  (:map u-map/org-alt
        ("m" . (lambda () (interactive) (org-capture nil "m")))
        ("q" . (lambda () (interactive) (org-capture nil "t")))
        ("w" . (lambda () (interactive) (org-capture nil "w")))))

(provide 'user-org)
;;; user-org.el ends here
