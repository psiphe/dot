;;; user-python-lang.el ---
;;; Commentary:
;;; Code:

(use-package eglot
  :straight (:type built-in)
  :hook (python-mode . eglot-ensure))

(use-package python
  :straight (:type built-in)
  :config
  (setq python-shell-completion-native-enable nil)
  :bind
  (:map python-mode-map
        ("C-j f i" . run-python)))

(use-package pyvenv
  :hook (python-mode)
  :bind
  (:map python-mode-map
        ("C-j f e" . pyvenv-activate)
        ("C-j k e" . pyvenv-deactivate)
        ("C-j n e" . pyvenv-create)))

(use-package reformatter
  :config
  (reformatter-define black-reformatter :program "black" :args '("-"))
  (add-hook 'python-mode-hook 'black-reformatter-on-save-mode))

(provide 'user-python-lang)
;;; user-python-lang.el ends here
