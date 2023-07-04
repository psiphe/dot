;;; init.el --- Emacs configuration.
;;;
;;; Commentary:
;; Divided into sections:
;; * Bootstrapping: essential configuration (package management, gc config, etc.)
;; * Emacs Builtins
;; * External Packages
;; * Local Overrides: machine local config
;;;
;;; Code:

;;; 0 - Bootstrapping:

;; Use straight.el as the package manager in place of the default (package.el).
;; https://github.com/radian-software/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(if (version< emacs-version "30.0") ; before use-package was builtin
    (straight-use-package 'use-package))
(setq straight-use-package-by-default t)

;; Gcmh-mode manages `gc-cons-threshold` to prefer gc while idling.
;; `gc-cons-threshold` is purposefully set too high in `early-init.el` to
;; load faster. Without `gchm-mode`, `gc-cons-threshold` should be
;; set to something more reasonable somewhere in `init.el`.
(use-package gcmh
  :hook (emacs-startup))

;; Create a user keymap.
(defvar u-map)
(define-prefix-command 'u-map)
(global-set-key (kbd "C-j") u-map)

;; Load UI modifications early to avoid visual jitter.
(add-to-list 'custom-theme-load-path "~/.config/emacs/nox-theme")
(load-theme 'nox t)
(menu-bar-mode -1)
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(use-package doom-modeline
  :hook (emacs-startup))

(use-package nerd-icons)
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package centaur-tabs
  :init
  (setq centaur-tabs-set-icons t)
  (centaur-tabs-mode t)
  :config
  (add-hook 'compilation-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'dired-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'flymake-diagnostics-buffer-mode-hook 'centaur-tabs-local-mode)
  (setq centaur-tabs-gray-out-icons 'buffer ; gray out icons of inactive tabs
        centaur-tabs-show-new-tab-button nil
        centaur-tabs-close-button ""
        centaur-tabs-enable-ido-completion nil)
  (defun centaur-tabs-buffer-groups ()
    "Group by project, excluding most Emacs buffers."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              helpful-mode
                              help-mode
                              )))
      "Meta")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :bind
  (:map u-map
        ("t t" . centaur-tabs-toggle-groups)
        ("t s" . centaur-tabs-switch-group)))

;;; 1 - Emacs Builtins

(setq-default compilation-scroll-output t
              fill-column 80
              indent-tabs-mode nil
              tab-width 4
              truncate-lines nil)

(setq completion-styles '(basic partial-completion flex)
      kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions)
      make-backup-files nil)

(defun open-line-below (arg)
  "Create ARG new lines below the current line, and update Point."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (indent-according-to-mode))

(defun open-line-above (arg)
  "Create ARG new lines above the current line, and update Point."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'window-setup-hook 'winner-mode)

(global-set-key (kbd "M-m") 'scroll-other-window)      ; hack: C-.
(global-set-key (kbd "M-o") 'scroll-other-window-down) ; hack: C-,
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "C-q") 'open-line-above)
(global-set-key (kbd "M-;") 'comment-line)
(define-key u-map (kbd "C-n") 'winner-redo)
(define-key u-map (kbd "C-p") 'winner-undo)
(define-key u-map (kbd "C-k") 'kill-current-buffer)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "p") 'dired-up-directory))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-j") nil) ; u-map prefix key
  (setq org-hide-emphasis-markers t
        org-startup-folded t))

;;; 2 - External Packages

;;; - - Editing & Navigation

;; Hotkey based editing commands.
(use-package avy
  :bind
  (:map u-map
        ("C-l" . avy-goto-line)
        ("c l" . avy-copy-line)
        ("c r" . avy-copy-region)
        ("k l" . avy-kill-whole-line)
        ("k r" . avy-kill-region)))

;; Pluggable autocompletion.
(use-package company
  :hook (prog-mode conf-mode)
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0))

;; Builtin commands integrated with `completing-read` + previews.
(use-package consult
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

;; An improved dired interface.
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :bind
  (:map dirvish-mode-map
        ("C-j C-s" . dirvish-layout-toggle)))

;; Persistent buffer configurations.
(use-package workgroups2
  :config
  (setq wg-session-file (concat user-emacs-directory "/.emacs_workgroups"))
  :bind
  (:map u-map
        ("w c" . wg-create-workgroup)
        ("w k" . wg-kill-workgroup)
        ("w f" . wg-open-workgroup)))

;; Automatically remove extra whitespace.
(use-package whitespace-cleanup-mode
  :hook (prog-mode conf-mode))

;; Mid-keystroke popup showing all possible completions.
(use-package which-key
  :hook (emacs-startup)
  :config
  (which-key-setup-side-window-right-bottom))

;;; - - Minibuffer

;; Show documentation in the minibuffer margin.
(use-package marginalia
  :hook (emacs-startup))

;; Interactive completion.
(use-package vertico
  :hook (emacs-startup))

;;; - - Coding Support
;; - As most of their config depends on these common packages, specific
;;   programming language support is split out further down.

;; On-the-fly syntax checking & linting.
(use-package flymake
  :straight (:type built-in)
  :config
  (defun u/flymake-show-buffer-diagnostics ()
    (interactive)
    (let ((split-width-threshold nil)
          (split-height-threshold 0))
      (call-interactively 'flymake-show-buffer-diagnostics)))
  (define-key u-map (kbd "e l") (lambda() (interactive)(u/flymake-show-buffer-diagnostics)))
  :bind
  (:map u-map
        ("e n" . flymake-goto-next-error)
        ("e p" . flymake-goto-prev-error)))

;; Highlight typical todo keywords (FIXME, TODO, etc.)
(use-package hl-todo
  :hook (prog-mode))

;; Git UI
(use-package magit
  :defer t
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

;; Pluggable code autoformat - noop placeholder here, each language is configured individually.
(use-package reformatter
  :defer t)

;; Tree-sitter powered syntax highlighting + semantic analysis.
;; TODO: lazy load tree-sitter.
(use-package tree-sitter
  :hook (emacs-startup . global-tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode))

;; Tree-sitter backend bundle.
(use-package tree-sitter-langs
  :after (tree-sitter))

;;; - - Programming Language (& Config Format) Support

;; C
(setq c-basic-offset 4)
(add-hook 'c-mode 'eglot-ensure)
(use-package reformatter
  :defer t
  :config
  (reformatter-define clang-reformatter :program "clang-format" :args '("-style={BasedOnStyle: Google, IndentWidth: 4, BreakBeforeBraces: Stroustrup, AlwaysBreakAfterReturnType: AllDefinitions}"))
  (add-hook 'c-mode-hook 'clang-reformatter-on-save-mode))

;; Elisp
(add-hook 'emacs-lisp-mode-hook 'flymake-mode)
(define-key lisp-interaction-mode-map (kbd "C-j") nil) ; u-map prefix conflict
(use-package paren-face ; make parentheses less visible for lisp
  :hook (emacs-lisp-mode))

;; Markdown
(use-package markdown-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook 'display-line-numbers-mode))

;; Python
(add-hook 'python-mode-hook 'eglot-ensure)
(use-package reformatter
  :defer t
  :config
  (reformatter-define black-reformatter :program "black" :args '("-"))
  (add-hook 'python-mode-hook 'black-reformatter-on-save-mode))

;; Sh
(use-package flymake-shellcheck ; shellcheck backend for flymake
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
  (add-hook 'sh-mode-hook 'flymake-mode))

;; TOML
(add-to-list 'auto-mode-alist '("\\.toml\\'" . conf-mode))

;; YAML
(use-package yaml-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  :config
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode))

;;; 3 - Local Overrides

;; load all .el files in $HOME/.config/emacs
(let* ((local-config-dir "~/.config/emacs"))
  (unless (file-exists-p local-config-dir)
    (make-directory local-config-dir))
  (cl-loop for file in (directory-files-recursively local-config-dir "\\.el$")
           do (condition-case err
                  (load file)
                ('error (message (format "failed to load local config file: %s %s" file err))))))

(provide 'init)
;;; init.el ends here
