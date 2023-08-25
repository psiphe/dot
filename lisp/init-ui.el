;;; init-ui.el ---

;;; Commentary:

;;; Code:

(menu-bar-mode -1)
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

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
  (setq writeroom-major-modes '(conf-mode org-mode org-agenda-mode prog-mode)
        writeroom-mode-line t
        writeroom-width 120
        writeroom-maximize-window nil)) ; allow multiple buffers to be open at once.

;; Highlight mutating changes (e.g. undo)
(use-package volatile-highlights
  :hook (emacs-startup))

(provide 'init-ui)
;;; init-ui.el ends here
