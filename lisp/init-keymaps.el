;;; init-keymaps.el --- Initialize custom keymaps & prefixes
;;; Commentary:
;;; Code:

(defvar u-map)
(defvar u-map/org)
(defvar u-map/vc)
(define-prefix-command 'u-map)
(define-prefix-command 'u-map/org)
(define-prefix-command 'u-map/vc)
(global-set-key (kbd "C-j") u-map)
(define-key u-map (kbd "o") u-map/org)
(define-key u-map (kbd "v") u-map/vc)

(provide 'init-keymaps)

;;; init-keymaps.el ends here
