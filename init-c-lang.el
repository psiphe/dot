;;; init-c-lang.el --- C support
;;; Commentary:
;;; Code:

(setq c-basic-offset 4)
(add-hook 'c-mode 'eglot-ensure)

(use-package reformatter
  :defer t
  :config
  (reformatter-define clang-reformatter :program "clang-format" :args '("-style={BasedOnStyle: Google, IndentWidth: 4, BreakBeforeBraces: Stroustrup, AlwaysBreakAfterReturnType: AllDefinitions}"))
  (add-hook 'c-mode-hook 'clang-reformatter-on-save-mode))

(provide 'init-c-lang)

;;; init-c-lang.el ends here
