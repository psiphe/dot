;;; init.el --- Emacs configuration.
;;;
;;; Commentary:
;; Divided into sections:
;; * Bootstrapping
;;   - "System" configuration; package management, gc config, the like.
;; * Emacs Builtins
;; * External Packages
;; * Load Local Overrides
;;;
;;; Code:

;;; Bootstrapping:

;; - Package Management
;; Use straight.el, instead of package.el, as the package manager.
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

;; - Garbage Collection
;; `gcmh-mode` actively manages the memory usage threshold to trigger garbage
;; collection (`gc-cons-threshold`/`gc-cons-percentage`), with the intent to
;; trigger gcs while idling.
;;
;; `gc-cons-threshold` is set excessively high in `early-init.el` to load Emacs
;; faster by avoiding gcs. `gcmh-mode` starts as soon as config is finished
;; loading on the `emacs-startup-hook`, and should reset the thresholds to values
;; that don't result in memory pressure (fingers crossed).
;;
;; The thresholds do not need to be dynamic, and it should be equally reasonable
;; to find static values that work well for the hardware. Lsp-mode has a few
;; suggestions on how to do that:
;; - https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold
(use-package gcmh
  :hook (emacs-startup))

;; - User Prefix(es)
;; Create a dedicated user keymap.
;; The C-c keymap is assumed to be available for use by many 3p packages; this
;; is a properly isolated version of C-c.
(defvar u-map)
(define-prefix-command 'u-map)
(global-set-key (kbd "C-j") u-map)

;; - UI
;; Rendering UI is one of the heavier tasks at startup. Frontload it to avoid
;; weird visuals at launch.
(menu-bar-mode -1)
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
;; TODO: convert this to a use-package declaration -  I tried the obvious way
;; and it didn't work for some reason.
(straight-use-package
 '(glitch
   :type git
   :repo "https://github.com/psiphe/glitch-theme"))
(load-theme 'glitch t)

(use-package doom-modeline
  :hook (emacs-startup))

;; Just as compatible as `all-the-icons` + terminal support. Enables
;; glyphs for `centaur-tabs`, `dired`, `mode-line`, etc.
;; REQUIRES A NERD FONT: https://github.com/ryanoasis/nerd-fonts
(use-package nerd-icons)
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; A nice looking tabbar.
(use-package centaur-tabs
  :init
  (setq centaur-tabs-set-icons t)
  (centaur-tabs-mode t)
  :config
  ;; disable tabs in a few major modes
  (add-hook 'compilation-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'dired-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'flymake-diagnostics-buffer-mode-hook 'centaur-tabs-local-mode)
  (setq centaur-tabs-gray-out-icons 'buffer    ; gray out icons of inactive tabs
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

;;; Emacs Builtins

(setq-default compilation-scroll-output t
              fill-column 80
              indent-tabs-mode nil
              tab-width 4
              truncate-lines nil)

(setq completion-styles '(basic partial-completion flex)                                                 ; minibuffer filtering
      kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions) ; do not prompt to kill live process buffers
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
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'window-setup-hook 'winner-mode) ; undo/redo for the window arrangement

(global-set-key (kbd "M-m") 'scroll-other-window)      ; hack: mapped in iterm to C-.
(global-set-key (kbd "M-o") 'scroll-other-window-down) ; hack: mapped in iterm to C-,
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "C-q") 'open-line-above)
(global-set-key (kbd "M-;") 'comment-line)
(define-key u-map (kbd "C-n") 'winner-redo)
(define-key u-map (kbd "C-p") 'winner-undo)
(define-key u-map (kbd "C-k") 'kill-current-buffer)
(define-key u-map (kbd "C-o") (lambda()(interactive)(set-mark-command 1)))
(define-key u-map (kbd "C-d") 'zap-up-to-char)

(use-package dired
  :straight (:type built-in)
  :bind
  (:map dired-mode-map
        ("p" . dired-up-directory)))

(use-package org
  :straight (:type built-in)
  :config
  (setq org-hide-emphasis-markers t
        org-pretty-entities t
        org-startup-folded t
        org-todo-keywords '((sequence "TODO(t)" "CURR(c)" "REVW(r)" "WAIT(w)"
                                      "|"
                                      "DONE(d)")))
  :bind
  (:map org-mode-map
        ("C-j" . nil)))

;;; External Packages

;;; - Editing & Navigation

;; Better switch-buffer.
(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("C-x o" . ace-window))

;; Hotkey based editing commands.
(use-package avy
  :bind
  (:map u-map
        ("C-s" . avy-goto-word-or-subword-1)
        ("C-f" . avy-goto-char-timer)
        ("C-l" . avy-goto-line)
        ("c l" . avy-copy-line)
        ("c r" . avy-copy-region)
        ("k l" . avy-kill-whole-line)
        ("k r" . avy-kill-region)))

;; Like vim ci
(use-package change-inner
  :bind
  (:map u-map
        ("TAB" . change-inner)))

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
        ("M-8" . consult-register-store) ; hack: mapped in iterm to C-;
        ("M-9" . consult-register) ; hack: mapped in iterm to C-'
        ("C-x b" . consult-buffer)
        ("C-x p b" . consult-project-buffer)
        ("M-y" . consult-yank-pop)))

;; An improved dired interface.
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :bind
  (:map dirvish-mode-map
        ("C-j C-t" . dirvish-layout-toggle)))

;; A grep-based xref backend.
(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Highlight around the cursor.
(use-package expand-region
  :bind
  (:map u-map
        ("C-e" . er/expand-region)))

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

;; Center text files instead left-justifying.
(use-package writeroom-mode
  :init
  (setq writeroom-major-modes '(text-mode conf-mode prog-mode)
        writeroom-mode-line t
        writeroom-width 100
        writeroom-maximize-window nil)
  (global-writeroom-mode))

;; Highlight mutating changes.
(use-package volatile-highlights
  :init
  (volatile-highlights-mode 1))

;;; - Minibuffer

;; Show documentation in the minibuffer margin.
(use-package marginalia
  :hook (emacs-startup))

;; Interactive completion.
(use-package vertico
  :hook (emacs-startup))

;;; - Coding Support
;; As most of their config depends on these common packages, specific
;; programming language support is split out further down.

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

;; Visit previously committed verisons of the current file.
(use-package git-timemachine
  :bind
  (:map u-map
        ("v t" . git-timemachine)))

;; Highlight typical todo keywords (FIXME, TODO, etc.)
(use-package hl-todo
  :hook (prog-mode))

;; Git UI
(use-package magit
  :defer t
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :after (magit)
  :init
  (magit-todos-mode 1))

;; Pluggable code autoformat - nop placeholder here, each language is configured individually.
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

;;; Org Extensions

(use-package org-modern
  :init
  (with-eval-after-load 'org (global-org-modern-mode))
  :config
  ;; TODO put this into the theme
  (setq org-modern-todo-faces
        '(("TODO" :background "#37363f" :foreground "#e0af68" :weight bold)
          ("CURR" :background "#273644" :foreground "#4c9e8a" :weight bold)
          ("REVW" :background "#192a4d" :foreground "#7aa2f7" :weight bold)
          ("WAIT" :background "#342c3c" :foreground "#f7768e" :weight bold)
          ("DONE" :background "#1f2335" :foreground "#313750" :weight bold))))

(use-package org-roam
  :bind
  (:map u-map
        ("o n" . org-roam-node-find))
  :config
  (let* ((roam-dir org-directory))
    (unless (file-exists-p roam-dir)
      (make-directory roam-dir))
    (setq org-roam-directory (file-truename roam-dir))
    (org-roam-db-autosync-mode)))

;;; Programming Language (& Config Format) Support

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

;;; Load Local Overrides

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
