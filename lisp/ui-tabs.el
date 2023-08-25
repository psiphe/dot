;;; ui-tabs.el --- Tab bar
;;; Commentary:
;;; Code:

(use-package centaur-tabs
  :hook
  (emacs-startup)
  :init
  (setq centaur-tabs-common-group-name "ghost"
        centaur-tabs-set-icons t)
  :config
  (setq centaur-tabs--buffer-show-groups t
        centaur-tabs-ace-jump-keys '(?a ?s ?d ?f ?j ?k ?l)
        centaur-tabs-close-button ""
        centaur-tabs-enable-ido-completion nil
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-show-new-tab-button nil)
  ;; Disable tabs in certain modes (magit, dired, etc.)
  (add-hook 'compilation-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'dired-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'org-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'org-agenda-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'flymake-diagnostics-buffer-mode-hook 'centaur-tabs-local-mode)
  ;; Override a few of the default functions, mostly project-oriented.
  (defun centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       (window-dedicated-p (selected-window))
       (string-prefix-p "*" name)
       (string-equal major-mode "org-mode")
       (string-equal major-mode "org-agenda-mode")
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name)))
       )))
  (defun centaur-tabs-project-name ()
    "Get project name for tabs."
    (let* ((project-current (project-current))
           (project-name (if (proper-list-p project-current)
                             (car (last project-current))
                           (cdr project-current))))
      (if project-name
          (format "%s" project-name)
        centaur-tabs-common-group-name)))
  (defun centaur-tabs-buffer-groups ()
    "Group by project, excluding most Emacs buffers."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              dired-mode
                              helpful-mode
                              help-mode
                              )))
       "emacs")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :bind
  (:map u-map
        ("t t" . centaur-tabs-toggle-groups)
        ("t n" . centaur-tabs-forward)
        ("t p" . centaur-tabs-backward)
        ("t o" . centaur-tabs-ace-jump)))

(provide 'ui-tabs)
;;; ui-tabs.el ends here
