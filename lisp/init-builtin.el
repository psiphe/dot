;;; init-builtin.el --- Configure builtin modes/vars/etc
;;; Commentary:
;;; Code:

(setq-default compilation-scroll-output t
              fill-column 80
              indent-tabs-mode nil
              tab-width 4
              truncate-lines nil)
(setq make-backup-files nil)

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'window-setup-hook 'winner-mode)

(global-set-key (kbd "C-M-j") 'zap-up-to-char)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "C-q") 'open-line-above)
(global-set-key (kbd "M-;") 'comment-line)
(global-set-key (kbd "M-m") 'scroll-other-window)      ; hack: C-.
(global-set-key (kbd "M-o") 'scroll-other-window-down) ; hack: C-,
(global-set-key (kbd "C-x C-b") 'ibuffer)

(define-key u-map (kbd "C-SPC") 'jump-to-register)
(define-key u-map (kbd "C-j") 'point-to-register)
(define-key u-map (kbd "C-c") 'kill-buffer-and-window)
(define-key u-map (kbd "C-k") 'kill-current-buffer)
(define-key u-map (kbd "1") 'kill-other-buffers)
(define-key u-map (kbd "C-d") 'change-inside-word)
(define-key u-map (kbd "C-n") 'winner-redo)
(define-key u-map (kbd "C-p") 'winner-undo)
(define-key u-map (kbd "C-o") (lambda()(interactive)(set-mark-command 1)))
(define-key u-map (kbd "C-r") (lambda() (interactive)(set-mark-command nil)(isearch-backward)))
(define-key u-map (kbd "C-s") (lambda() (interactive)(set-mark-command nil)(isearch-forward)))

(defun open-line-below (arg)
  "Create ARG new lines below the current line, and update Point."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (indent-according-to-mode))
(defun open-line-above (arg)
  "Create ARG new lines above the current line, and update Point."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))
(defun change-inside-word ()
  "Kill the word at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (if bounds
        (kill-region (car bounds) (cdr bounds)))))
(defun kill-other-buffers ()
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (cl-remove-if-not 'buffer-file-name (buffer-list)))))

(provide 'init-builtin)

;;; init-builtin.el ends here
