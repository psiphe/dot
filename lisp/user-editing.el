;;; user-editing.el ---

;;; Commentary:

;;; Code:

(setq-default indent-tabs-mode nil
              tab-width 4
              truncate-lines nil)

(setq make-backup-files nil)

;; Hotkey based search / edit.
(use-package avy
  :demand t
  :config
  (setq avy-keys '(?a ?s ?d ?f ?j ?k ?l))
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "s") 'avy-goto-word-or-subword-1))
  :bind
  (:map u-map
        ("C-l" . avy-goto-line)
        ("c l" . avy-copy-line)
        ("c r" . avy-copy-region)
        ("k l" . avy-kill-whole-line)
        ("k r" . avy-kill-region)))

;; Autocomplete
(use-package company
  :hook
  (prog-mode conf-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0))

;; Builtin commands integrated with `completing-read` + previews.
(use-package consult
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.0
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window)
  :bind
  (:map global-map
        ("C-x b" . consult-buffer)
        ("C-x p b" . consult-project-buffer)
        ("M-y" . consult-yank-pop)))

(use-package evil
  :hook (emacs-startup))

;; Mid-keystroke popup showing possible completions.
(use-package which-key
  :hook (emacs-startup)
  :config
  (which-key-setup-side-window-right-bottom))

(provide 'user-editing)
;;; user-editing.el ends here
