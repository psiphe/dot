(use-package consult
  :demand t
  :init
  (setq register-preview-delay 0.0
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  (:map global-map
        ("C-x b" . consult-buffer)
        ("C-x p b" . consult-project-buffer)
        ("M-y" . consult-yank-pop)))

(provide 'init-consult)
