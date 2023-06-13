;;; init-flymake.el --- Linting
;;; Commentary:
;;; Code:



(use-package flymake
  :straight (:type built-in)
  ;; :init
  :config
  (defun flymake-list-buffer-diagnostics ()
    (interactive)
    (let ((split-width-threshold nil)
          (split-height-threshold 0))
      (call-interactively 'flymake-show-buffer-diagnostics)))
  (define-key u-map (kbd "e l") (lambda() (interactive)(flymake-list-buffer-diagnostics)))
  :bind
  (:map u-map
        ("e n" . flymake-goto-next-error)
        ("e p" . flymake-goto-prev-error)))

(provide 'init-flymake)

;;; init-flymake.el ends here
