;;; init-centaur-tabs.el --- Nice looking tabs with project-based grouping
;;; Commentary:
;;; Code:

(use-package centaur-tabs
  :init
  (centaur-tabs-mode t)
  :config
  (add-hook 'compilation-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'flymake-diagnostics-buffer-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'dired-mode-hook 'centaur-tabs-local-mode)
  (centaur-tabs-toggle-groups)
  (setq centaur-tabs-plain-icons t)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-ace-jump-keys '(?a ?s ?d ?f ?j ?k ?l))
  (setq centaur-tabs-ace-jump-dim-buffer nil)
  (setq centaur-tabs-show-new-tab-button nil
        centaur-tabs--buffer-show-groups nil
        centaur-tabs-close-button ""
        centaur-tabs-enable-ido-completion nil)
  (defun centaur-tabs-buffer-groups ()
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
                              )))
       "Emacs")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :bind
  (:map u-map
        ("t o" . centaur-tabs-ace-jump)
        ("t t" . centaur-tabs-toggle-groups)
        ("t n" . centaur-tabs-forward)
        ("t p" . centaur-tabs-backward)
        ("t s" . centaur-tabs-switch-group)
        ("t r" . (lambda() (interactive)(centaur-tabs-mode -1)(centaur-tabs-mode 1)))))

(provide 'init-centaur-tabs)

;;; init-centaur-tabs.el ends here
