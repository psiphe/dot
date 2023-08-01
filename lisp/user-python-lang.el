;;; user-python-lang.el ---

;;; Commentary:

;;; Code:

(add-hook 'python-mode-hook 'eglot-ensure)

(use-package reformatter
  :config
  (reformatter-define black-reformatter :program "black" :args '("-"))
  (add-hook 'python-mode-hook 'black-reformatter-on-save-mode))

(provide 'user-python-lang)
;;; user-python-lang.el ends here
