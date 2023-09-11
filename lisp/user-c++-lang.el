;;; user-c++-lang.el ---
;;; Commentary:
;;; Code:

(use-package cc-mode
  :straight (:type built-in)
  :bind
  (:map c-mode-map
        ("C-j f p" . ff-find-other-file))
  (:map c++-mode-map
        ("C-j f p" . ff-find-other-file)))

(use-package reformatter
  :config
  (reformatter-define clang-reformatter :program "clang-format" :args '("-style={BasedOnStyle: Google, IndentWidth: 4, BreakBeforeBraces: Stroustrup, AlwaysBreakAfterReturnType: AllDefinitions, SortIncludes: false}"))
  (add-hook 'c++-mode-hook 'clang-reformatter-on-save-mode))

(provide 'user-c++-lang)
;;; user-c++-lang.el ends here
