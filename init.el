;;; init.el
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; pre-init
(require 'init-packaging)          ; 0 - make external packages available
(require 'init-keymaps)            ; 1 - initialize globally used custom keymaps

(require 'init-theme)              ; 2 - load ui changes early to avoid jitter
(require 'init-centaur-tabs)
;; builtins
(require 'init-builtin)
(require 'init-dired)
(require 'init-org)
;; more org
(require 'init-org-roam)
;; minibuffer
(require 'init-marginalia)
(require 'init-vertico)
;; editing & navigation
(require 'init-ace-window)
(require 'init-avy)
(require 'init-change-inner)
(require 'init-company)
(require 'init-consult)
(require 'init-expand-region)
(require 'init-whitespace-cleanup)
(require 'init-which-key)
(require 'init-workgroups)
;; coding
(require 'init-diff-hl)
(require 'init-flymake)
(require 'init-flymake-popon)
(require 'init-hl-todo)
(require 'init-magit)
(require 'init-tree-sitter)

;; language support
(require 'init-c-lang)
(require 'init-elisp-lang)
(require 'init-markdown-lang)
(require 'init-python-lang)
(require 'init-sh-lang)
(require 'init-toml-lang)
(require 'init-yaml-lang)

(require 'init-gcmh)

(provide 'init)
;;; init.el ends here
