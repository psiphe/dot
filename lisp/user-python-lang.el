;;; user-python-lang.el ---
;;; Commentary:
;;; Code:

(use-package eglot
  :straight (:type built-in)
  :defer t
  :hook (python-mode . eglot-ensure))

(use-package python
  :straight (:type built-in)
  :config
  (setq python-shell-completion-native-enable nil)
  :bind
  (:map u-map
        ("O" . run-python)))

(use-package pyvenv
  :hook (python-mode)
  :bind
  (:map python-mode-map
        ("C-j a e" . pyvenv-activate)
        ("C-j a d" . pyvenv-deactivate)))

(use-package reformatter
  :config
  (reformatter-define black-reformatter :program "black" :args '("-"))
  (add-hook 'python-mode-hook 'black-reformatter-on-save-mode))

(provide 'user-python-lang)
;;; user-python-lang.el ends here
