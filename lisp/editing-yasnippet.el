;;; editing-yasnippet.el --- Configurable text templates.
;;; Commentary:
;;; Code:

(use-package yasnippet
  :hook
  (emacs-startup . yas-global-mode)
  :bind
  (:map u-map
        ("f s" . yas-visit-snippet-file)
        ("i s" . yas-insert-snippet)
        ("n s" . yas-new-snippet)))

(provide 'editing-yasnippet)
;;; editing-yasnippet.el ends here
