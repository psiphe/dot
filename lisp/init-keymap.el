;;; init-keymap.el ---

;;; Commentary:
;; Create a dedicated user keymap.
;; This is a properly isolated form of the `C-c` keymap, which is now used
;; by many builtin & external packages.

;;; Code:

(defvar u-map)
(defvar u-map/misc)
(defvar u-map/org)
(defvar u-map/vc)
(define-prefix-command 'u-map)
(define-prefix-command 'u-map/misc)
(define-prefix-command 'u-map/org)
(define-prefix-command 'u-map/vc)
(global-set-key (kbd "C-j") u-map)
(define-key u-map (kbd "q") u-map/misc)
(define-key u-map (kbd "o") u-map/org)
(define-key u-map (kbd "v") u-map/vc)

(provide 'init-keymap)
;;; init-keymap.el ends here
