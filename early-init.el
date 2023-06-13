;;; early-init.el --- Emacs preconfiguration
;;; Commentary:
;;
;; Emacs 27.1+ will load this file prior to GUI initialization.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html
;;
;;; Code:

(setq gc-cons-threshold most-positive-fixnum ; avoid garbage collection during startup
      inhibit-splash-screen t
      package-enable-at-startup nil)         ; do not use package.el

(provide 'early-init)
;;; early-init.el ends here
