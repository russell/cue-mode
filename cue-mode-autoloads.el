;;; cue-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "cue-mode" "cue-mode.el" (24767 46588 8297
;;;;;;  445000))
;;; Generated autoloads from cue-mode.el

(autoload 'cue-mode "cue-mode" "\


\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons "\\.cue\\'" 'cue-mode))

(autoload 'cue-eval-buffer "cue-mode" "\
Run cue with the path of the current file." t nil)

(autoload 'cue-reformat-buffer "cue-mode" "\
Reformat entire buffer using the Cue format utility." t nil)

(register-definition-prefixes "cue-mode" '("cue-" "verbose-cue-smie-rules"))

;;;***

(provide 'cue-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cue-mode-autoloads.el ends here
