;; -*- mode: elisp; lexical-binding: t; -*-
;; TODO A high level institution authoring mode

(define-derived-mode acab-inst-mode fundamental-mode "Institution Mode"
  "Major Mode for creating institutions"
  (interactive)
  ;;TODO Specify a working directory
  ;;load the working directory
  (kill-all-local-variables)

  ;;TODO set up windows

  (setq major-mode 'acab-inst-mode)
  (setq mode-name "INST")
  (run-mode-hooks)
  )




(provide 'acab-inst-mode)
