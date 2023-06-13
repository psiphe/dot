;;; init-packaging.el --- Setup package management
;;; Commentary:
;;; Code:

;; use straight.el in place of package.el
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

(provide 'init-packaging)

;;; init-packaging.el ends here
