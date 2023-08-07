;;; user-editing.el ---

;;; Commentary:

;;; Code:

(setq-default indent-tabs-mode nil
              tab-width 4
              truncate-lines nil)

(setq make-backup-files nil)

(defun open-line-below (arg)
  "Create ARG new lines below the current line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (indent-according-to-mode))

(defun open-line-above (arg)
  "Create ARG new lines above the current line."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

(defun store-fast-point()
  "Store current point to the self-managed `fast user point register`."
  (interactive)
  (point-to-register :user-fast-point))

(defun load-fast-point()
  "Jump to the self-managed `fast user point register`, swap current point in."
  (interactive)
  (let* ((cur-point (point-marker)))
    (jump-to-register :user-fast-point)
    (set-register :user-fast-point cur-point)))

(global-set-key (kbd "M-;") 'comment-line)
(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "C-q") 'open-line-above)
(global-set-key (kbd "C-k") 'kill-whole-line)
(define-key u-map (kbd "C-d") 'zap-up-to-char)
(define-key u-map (kbd "C-o") 'load-fast-point)
(define-key u-map (kbd "RET") 'store-fast-point)
(define-key u-map (kbd "C-q") 'pop-global-mark)
(define-key u-map (kbd "M-8") 'point-to-register)
(define-key u-map (kbd "M-9") 'jump-to-register)

(advice-add 'isearch-forward :before 'store-fast-point)
(advice-add 'isearch-backward :before 'store-fast-point)

;; Hotkey based search / edit.
(use-package avy
  :demand t
  :config
  (setq avy-keys '(?a ?s ?d ?f ?j ?k ?l))
  (advice-add 'avy-goto-word-or-subword-1 :before 'store-fast-point)
  (advice-add 'avy-goto-char-timer :before 'store-fast-point)
  (advice-add 'avy-goto-line :before 'store-fast-point)
  :bind
  (:map u-map
        ("C-f" . avy-goto-word-or-subword-1)
        ("C-s" . avy-goto-char-timer)
        ("C-l" . avy-goto-line)
        ("c l" . avy-copy-line)
        ("c r" . avy-copy-region)
        ("k l" . avy-kill-whole-line)
        ("k r" . avy-kill-region)))

;; Vim `ci`.
(use-package change-inner
  :demand t
  :bind
  (:map u-map
        ("TAB" . change-inner)))

;; Autocomplete.
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

;; Mid-keystroke popup showing possible completions.
(use-package which-key
  :hook (emacs-startup)
  :config
  (which-key-setup-side-window-right-bottom))

(provide 'user-editing)
;;; user-editing.el ends here
