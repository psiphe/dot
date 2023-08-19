;;; init-ui.el ---

;;; Commentary:

;;; Code:

(menu-bar-mode -1)
(global-hl-line-mode 1)
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(use-package centaur-tabs
  :hook
  (emacs-startup)
  :init
  (setq centaur-tabs-set-icons t)
  :config
  (setq centaur-tabs--buffer-show-groups t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-show-new-tab-button nil
        centaur-tabs-close-button ""
        centaur-tabs-enable-ido-completion nil
        centaur-tabs-ace-jump-keys '(?a ?s ?d ?f ?j ?k ?l))
  ;; disable tabs in a few major modes
  (add-hook 'compilation-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'dired-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'flymake-diagnostics-buffer-mode-hook 'centaur-tabs-local-mode)
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
      "Meta")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :bind
  (:map u-map
        ("t t" . centaur-tabs-toggle-groups)))

(use-package doom-modeline
  :hook
  (emacs-startup))

(use-package glitch-theme
  :straight (:type git :repo "https://github.com/psiphe/glitch-theme")
  :init
  (add-hook 'emacs-startup-hook (load-theme 'glitch t)))

;; A replacement for `all-the-icons` with terminal support.
;; Enables icons via unicode glyphs.
;; Requires a nerd font: https://github.com/ryanoasis/nerd-fonts
(use-package nerd-icons)

(use-package nerd-icons-dired
  :after
  (nerd-icons)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package writeroom-mode
  :hook
  (emacs-startup . global-writeroom-mode)
  :config
  (setq writeroom-major-modes '(conf-mode org-mode prog-mode)
        writeroom-mode-line t
        writeroom-width 120
        writeroom-maximize-window nil)) ; allow multiple buffers to be open at once.

;; Highlight mutating changes (e.g. undo)
(use-package volatile-highlights
  :hook (emacs-startup))

(provide 'init-ui)
;;; init-ui.el ends here
