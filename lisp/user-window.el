;;; user-window.el --- Window/buffer/frame management.

;;; Commentary:

;;; Code:

;; Do not prompt when killing process buffers.
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(global-set-key (kbd "M-m") 'scroll-other-window)      ; hack: mapped in iterm to C-.
(global-set-key (kbd "M-o") 'scroll-other-window-down) ; hack: mapped in iterm to C-,
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
  (setq wg-session-file (concat user-emacs-directory "/.emacs_workgroups")
        wg-no-confirm-on-destructive-operation t)
  :bind
  (:map u-map/window
        ("c" . wg-create-workgroup)
        ("w" . wg-kill-workgroup)
        ("f" . wg-open-workgroup)))

(provide 'user-window)
;;; user-window.el ends here
