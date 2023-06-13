;;; init-theme.el --- Load theme
;;; Commentary:
;;; Code:

(menu-bar-mode -1)
;; TODO get this into a straight recipe
(add-to-list 'custom-theme-load-path "~/.config/emacs/nox-theme")
(load-theme 'nox t)
;; continuous line for the window divider
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(provide 'init-theme)

;;; init-theme.el ends here
