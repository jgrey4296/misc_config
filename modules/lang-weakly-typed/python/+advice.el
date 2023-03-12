;;; +advice.el -*- lexical-binding: t; -*-

;; (define-advice python-info-dedenter-statement-p (:override ()
;;                                                  +jg-python-dedenter-rx-override)

;;   "Return point if current statement is a dedenter.
;; Sets `match-data' to the keyword that starts the dedenter
;; statement."
;;   (save-excursion
;;     (python-nav-beginning-of-statement)
;;     (when (and (not (python-syntax-context-type))
;;                (or (looking-at (python-rx dedenter))
;;                    (looking-at "\n\n+")
;;                    )
;;                )
;;       (point)
;;       )
;;     )
;;   )

;; (define-advice outline-next-heading (:before-until()
;;                                      +jg-python-override)
;;   (interactive)
;;   (if (and (bolp) (not (eobp))) (forward-char 1))
;;   (if (eq major-mode 'python-mode)
;;       (let* ((indent (python-indent--calculate-indentation))
;;              (total outline-regexp)) ;;(eval `(rx (or ,outline-regexp (** 1 ,indent " ") )))))
;;         (if (re-search-forward (concat "^\\(?:" total "\\)") nil 'move)
;;             (goto-char (match-beginning 0))
;;           )
;;         )
;;     )
;;   )
