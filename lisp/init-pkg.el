;;; init-pkg.el --- Initialize package management.

;;; Commentary:
;; Use straight.el instead of package.el as the package manager.
;; https://github.com/radian-software/straight.el#getting-started
;;
;; Make sure package.el is disabled via `package-enable-at-startup` (see early-init.el)

;;; Code:

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

;; Configure `use-package` to use `straight` instead of `package`.
;; Install `use-package` in Emacs versions before it was builtin (29.1).
(if (version< emacs-version "29.1")
    (straight-use-package 'use-package))
(setq straight-use-package-by-default t)

(provide 'init-pkg)
;;; init-pkg.el ends here
