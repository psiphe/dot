;;; early-init.el ---

;;; Commentary:

;;; Code:

;; Max out `gc-cons-threshold` to speed up startup. This should be reset later
;; to avoid excessively long garbage collections.
(setq gc-cons-threshold most-positive-fixnum
      inhibit-splash-screen t
      package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
