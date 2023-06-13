;;; init-ace-window.el --- C-x o with hotkeys when > 2 windows
;;; Commentary:
;;; Code:

(use-package ace-window
  :bind
  (("C-x o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)))

(provide 'init-ace-window)

;;; init-ace-window.el ends here
