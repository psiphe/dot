;;; init-ui.el ---

;;; Commentary:

;;; Code:

(menu-bar-mode -1)
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(use-package centaur-tabs
  :hook (emacs-startup)
  :init
  (setq centaur-tabs-set-icons t)
  :config
  (setq centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-show-new-tab-button nil
        centaur-tabs-close-button ""
        centaur-tabs-enable-ido-completion nil
        centaur-tabs-ace-jump-keys '(?a ?s ?d ?f ?j ?k ?l)
        centaur-tabs-ace-jump-dim-buffer nil)
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
        ("t t" . centaur-tabs-toggle-groups)
        ("t o" . centaur-tabs-ace-jump)))

(use-package doom-modeline
  :hook
  (emacs-startup))

(use-package glitch-theme
  :straight (:type git :repo "https://github.com/psiphe/glitch-theme")
  :init
  (add-hook 'emacs-startup-hook (load-theme 'glitch t)))

(use-package hl-line
  :straight (:type built-in)
  :hook (emacs-startup . global-hl-line-mode))

;; Highlight the cursor whenever a function in `pulsar-pulse-functions` is called.
(use-package pulsar
  :hook (emacs-startup . pulsar-global-mode)
  :config
  (with-eval-after-load 'ace-window
    (add-to-list 'pulsar-pulse-functions 'ace-window))
  (with-eval-after-load 'avy
    (add-to-list 'pulsar-pulse-functions 'avy-goto-line)
    (add-to-list 'pulsar-pulse-functions 'avy-goto-word-or-subword-1)))

;; A replacement for `all-the-icons` with terminal support.
;; Enables icons via unicode glyphs.
;; Requires a nerd font: https://github.com/ryanoasis/nerd-fonts
(use-package nerd-icons
  :after
  (centaur-tabs doom-modeline))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; Dim certain buffers.
(use-package solaire-mode
  :hook (emacs-startup . solaire-global-mode)
  :config
  (defun user-solaire-filter ()
    "Custom solaire selection."
    (cond ((memq major-mode `(dired-mode)) nil)
          (t t)))
  (setq solaire-mode-real-buffer-fn 'user-solaire-filter)
  (remove-hook 'solaire-global-mode-hook #'solaire-mode-fix-minibuffer))

;; Center text instead of left-justifying.
(use-package writeroom-mode
  :hook (emacs-startup . global-writeroom-mode)
  :config
  (setq writeroom-major-modes '(text-mode conf-mode prog-mode)
        writeroom-mode-line t
        writeroom-width 120
        writeroom-maximize-window nil))

;; Highlight mutating changes (e.g. undo)
(use-package volatile-highlights
  :hook (emacs-startup))

(provide 'init-ui)
;;; init-ui.el ends here
