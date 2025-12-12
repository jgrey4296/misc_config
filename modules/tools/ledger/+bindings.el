;;; +bindings.el -*- lexical-binding: t; no-byte-compile: t; -*-


(map! :map ledger-mode-map
      :localleader
      :desc "Reconcile" "f" #'ledger-reconcile
      :desc "Occur"     "s" #'ledger-occur
      :desc "Report"    "r" #'ledger-report
      :desc "Cancel"    "q" #'ledger-occur-remove-overlays
      )

;;; +bindings.el ends here
