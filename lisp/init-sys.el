;;; init-sys.el --- Miscellaneous emacs tuning.

;;; Commentary:

;;; Code:

;; Measure startup time, broken down by package.
(use-package benchmark-init
  :demand t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; `gcmh-mode` actively manages the memory usage threshold to trigger garbage
;; collection (`gc-cons-threshold`/`gc-cons-percentage`) with the intent to
;; trigger gcs while idling.
;;
;; `gc-cons-threshold` is set excessively high in `early-init.el` to load Emacs
;; faster by avoiding gcs. `gcmh-mode` starts as soon as init.el is loaded
;; on the `emacs-startup-hook`, and should reset the thresholds to something
;; sensible.
;;
;; The thresholds do not need to be dynamic, and it should be similarly reasonable.
;; to find static values that work well for the hardware. Lsp-mode has a few
;; suggestions on how to do that:
;; - https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold
(use-package gcmh
  :hook (emacs-startup))

(provide 'init-sys)
;;; init-sys.el ends here
