;;; init-python-lang.el --- Python support
;;; Commentary:
;;; Code:

(add-hook 'python-mode-hook 'eglot-ensure)

(use-package reformatter
  :defer t
  :config
  (reformatter-define black-reformatter :program "black" :args '("-"))
  (add-hook 'python-mode-hook 'black-reformatter-on-save-mode))

(provide 'init-python-lang)

;;; init-python-lang.el ends here
