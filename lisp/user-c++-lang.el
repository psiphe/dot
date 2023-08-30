;;; user-c++-lang.el ---
;;; Commentary:
;;; Code:

(use-package reformatter
  :config
  (reformatter-define clang-reformatter :program "clang-format" :args '("-style={BasedOnStyle: Google, IndentWidth: 4, BreakBeforeBraces: Stroustrup, AlwaysBreakAfterReturnType: AllDefinitions}"))
  (add-hook 'c++-mode-hook 'clang-reformatter-on-save-mode))

(provide 'user-c++-lang)
;;; user-c++-lang.el ends here
