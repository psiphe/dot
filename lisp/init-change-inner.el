;;; init-change-inner.el --- Like vim `ci`
;;; Commentary:
;;; Code:

(use-package change-inner
  :ensure t                      ; avoid lag on first use
  :bind
  (:map u-map
        ("TAB" . change-inner)   ; hack-y: in term C-i sends TAB
        ("C-a" . change-outer)))

(provide 'init-change-inner)

;;; init-change-inner.el ends here
