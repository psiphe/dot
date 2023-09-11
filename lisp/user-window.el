;;; user-window.el --- Window/buffer/frame management.
;;; Commentary:
;;; Code:

;; Do not prompt when killing process buffers.
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(global-set-key (kbd "M-m") 'scroll-other-window)      ; HACK: mapped in iterm to C-.
(global-set-key (kbd "M-o") 'scroll-other-window-down) ; HACK: mapped in iterm to C-,
(define-key u-map (kbd "C-k") 'kill-current-buffer)

;; Better `other-window`.
(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("C-x o" . ace-window))

;; Undo/redo for the window arrangement.
(use-package winner
  :straight (:type built-in)
  :hook (emacs-startup)
  :bind
  (:map u-map
        ("C-p" . winner-undo)
        ("C-n" . winner-redo)))

;; Persistent buffer/window configurations.
(use-package workgroups2
  :config
  (setq wg-no-confirm-on-destructive-operation t
        wg-session-file (concat user-emacs-directory "/.emacs_workgroups"))
  :bind
  (:map u-map
        ("f w" . wg-open-workgroup)
        ("k w" . wg-kill-workgroup)
        ("n w" . wg-create-workgroup)))

(provide 'user-window)
;;; user-window.el ends here
