


(define-derived-mode inst-mode fundamental-mode "Institution Mode"
  "Major Mode for creating institutions"
  (interactive)
  ;;TODO Specify a working directory
  ;;load the working directory
  (kill-all-local-variables)

  ;;TODO set up windows

  (setq major-mode 'inst-mode)
  (setq mode-name "INST")
  (run-mode-hooks)
  )




(provide 'inst-mode)
