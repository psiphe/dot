;;; user-org.el ---
;;; Commentary:
;; TODO: I think use-package is loading org right away
;; TODO: explore org tables / gnuplot
;; TODO: hotkey for specific org captures
;; TODO: evil C-o C-i
;;
;; - Use org-add-note
;; - Use drawers?
;;
;; I want:
;; - To quickly create todos/ideas/project notes
;; - Project notes should be able to be opened easily
;; - Agenda views for:
;;   + Week
;;   + Ideas
;; - Better todo keyword sequences
;;; Code:

(setq org-use-speed-commands t
      org-agenda-custom-commands
      '(("f" occur-tree "haha")
        ("w" "Work summary"
         ((agenda "d" ((org-agenda-overriding-header "\n\n")))
          (todo "CURR" ((org-agenda-overriding-header "In Progress:")))
          (todo "REVW" ((org-agenda-overriding-header "Pending Review:")))
          (todo "WAIT" ((org-agenda-overriding-header "Blocked:")))
          (todo "DEPL" ((org-agenda-overriding-header "Deploying:")))))))
(setq org-agenda-window-setup 'only-window)
(setq org-agenda-restore-windows-after-quit t)
;; TODO: add a created timestamp to all captures
;; TODO: add deadlines to captures
(define-key u-map (kbd "C-o a") (lambda ()
                                    (interactive)
                                    (org-agenda nil "w")))
(setq org-table-header-line-mode t) ;; sticky table header
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
  (setq org-special-ctrl-a/e t) ;; Move to the beginning/end of the heading excluding leading *s, repeat to include
  (org-indent-mode)
  (setq org-cycle-separator-lines 1)
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$")
        org-agenda-block-separator ""
        org-capture-templates '(("m" "Meeting Notes" entry (file+headline org-default-notes-file "Meeting Notes")
                                 "** %? %^G\n:CREATED: %T\n%i\n")
                                ("i" "Idea" entry (file+headline org-default-notes-file "Ideas")
                                 "** %? :idea:%^G\n:CREATED: %T\n%i\n")
                                ("t" "Todo" entry (file+headline org-default-notes-file "Todos")
                                 "** TODO %? %^G\nCREATED: %T\n%i\n")
                                ("s" "Scheduled Work" entry (file+headline org-default-notes-file "Todos")
                                 "** TODO %? %^G\nDEADLINE: %^t\nCREATED: %T\n%i\n"))
        org-default-notes-file (concat org-directory "/quick.org")
        org-pretty-entities t
        org-startup-folded t
        org-enforce-todo-dependencies t
        org-log-done 'note
        org-enforce-todo-checkbox-dependencies t
        org-todo-keywords '(
                            (sequence "TODO" "WIP(@)" "REVIEW(@)" "DEPLOYING(@)" "|" "DONE")
                            ));; TODO fix keyword faces
  (defun org-clock-todo-change ()
    (if (string= org-state "WIP")
        (org-clock-in)
      (org-clock-out)))
  (add-hook 'org-after-todo-state-change-hook
            'org-clock-todo-change)
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-clock-into-drawer "TIME")
  (setq org-clock-persist t)
  (setq org-archive-subtree-save-file-p t)
  (org-clock-persistence-insinuate)
  :bind
  (:map u-map
        ("C-O /" . org-sparse-tree)
           ("C-O t" . org-todo-list)
           ("C-O l" . (lambda () (interactive) (org-tags-view t)))) ; TODO: fix this
  (:map u-map/org
        ("s" . org-search-view)
        ("a" . org-agenda)
        ("c" . org-capture)
        ("t" . org-clock-goto)
        ("l" . org-clock-display)
        ("f" . (lambda () (interactive) (ido-find-file-in-dir "~/org"))))
  (:map org-mode-map
        ("C-j" . nil)
        ("C-j RET" . org-insert-heading-respect-content)
        ("C-j i d" . org-deadline)
        ("C-j i e" . org-schedule)
        ("C-j i k" . org-set-property)
        ("C-j i t" . org-table-create-or-convert-from-region)
        ("C-j i ." . org-time-stamp)
        ("C-j i p" . org-priority)
        ("C-j i R" . org-clock-report)
        ("C-j i n" . org-add-note)
        ("C-j C-j" . org-toggle-narrow-to-subtree)
        ("C-j i l" . org-set-tags-command)
        ("C-j C-a" . org-archive-subtree)))
;; setup org-priority-faces
;; TODO: capture for meeting notes
;; TODO: add priority to captures
;; TODO: add tags to capures
;; TODO: use tags for agenda views
;; TODO: setup my own org-mode keybinds
;; Eye-candy.
;; (use-package org-modern
;;   :init
;;   (with-eval-after-load 'org (global-org-modern-mode)))

(provide 'user-org)
;;; user-org.el ends here
