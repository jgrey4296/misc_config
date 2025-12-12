;;; config.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defer-load! jg-bindings-total "+bindings")

(use-package! ledger-mode
  :config
  (setq-default ledger-reports
                '(;; Balances:
                  ("Asset (bal)ance" "%(binary) -f %(ledger-file) bal Assets Liabilities")
                  ("Totals"          "%(binary) -f %(ledger-file) bal")
                  ("uncleared (bal)" "%(binary) -f %(ledger-file) bal --uncleared")

                  ; Stats
                  ("stats"            "%(binary) -f %(ledger-file) stats")
                  ("tags (list)"      "%(binary) -f %(ledger-file) tags")
                  ("equity"          "%(binary) -f %(ledger-file) equity Assets or Liabilities")

                  ;; Transactions:
                  ("Transaction (reg)istry"  "%(binary) -f %(ledger-file) reg")
                  ("this month (reg)istry"  "%(binary) -f %(ledger-file) reg --period 'from this month to next month'")
                  ("by payee (reg)"          "%(binary) -f %(ledger-file) reg @%(payee)")
                  ("by account (reg)"        "%(binary) -f %(ledger-file) reg %(account)")
                  ("daily (reg)"             "%(binary) -f %(ledger-file) reg --daily")
                  ("weekly (reg)"            "%(binary) -f %(ledger-file) reg --weekly")
                  ("monthly (reg)"           "%(binary) -f %(ledger-file) reg --monthly")
                  ("expenses (reg)"          "%(binary) -f %(ledger-file) reg Expenses --monthly --period-sort '(amount)' ")
                  ("liabilities (reg)"       "%(binary) -f %(ledger-file) reg Liabilities --monthly")
                  ("budget (reg)"            "%(binary) -f %(ledger-file) reg --budget --monthly")


                  )
                )
  )


(speckler-add! auto-modes ()
  '(ledger
    ("\\.ledger\\'" . ledger-mode)
    )
  )


;;; config.el ends here
