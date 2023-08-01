;;; init-keymap.el ---

;;; Commentary:
;; Create a dedicated user keymap.
;; This is a properly isolated form of the `C-c` keymap, which is now used
;; by many builtin & external packages.

;;; Code:

(defvar u-map)
(defvar u-map/misc)
(define-prefix-command 'u-map)
(define-prefix-command 'u-map/misc)
(global-set-key (kbd "C-j") u-map)
(define-key u-map (kbd "C-u") u-map/misc)

(provide 'init-keymap)
;;; init-keymap.el ends here
