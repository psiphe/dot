;;; early-init.el --- Pre-gui & pre-package.el configuration.

;;; Commentary:
;; Loaded by Emacs 27.1+ before `init.el`.

;;; Code:

;; Max `gc-cons-threshold` to avoid gc at startup - must be reset later.
(setq gc-cons-threshold most-positive-fixnum
      inhibit-splash-screen t
      package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
