;;; early-init.el --- pre-gui & pre-package.el configuration.
;;;
;;; Commentary:
;; Emacs 27+: https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html
;;;
;;; Code:

;; Max `gc-cons-threshold` to avoid gc at startup - must be reset later.
(setq gc-cons-threshold most-positive-fixnum
      inhibit-splash-screen t
      package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
