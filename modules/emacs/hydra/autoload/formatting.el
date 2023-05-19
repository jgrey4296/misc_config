;;; +formatting.el -*- lexical-binding: t; -*-
(require 's)
(require 'dash)

(defun +jg-hydra-first-char-format (text &optional index)
  (let ((index (or index 1))
        (fmtter (lambda (i x) (format "%s_%s_%s^^"
                                      (if (< 0 (1- i )) (substring x 0 (1- i)) "")
                                      (substring x (1- i) i)
                                      (substring x i))
                  ))
        )
    (cond
     ((and (symbolp text) (s-contains? "_" (symbol-name text)))
      (concat (symbol-name text)
              (make-string (s-count-matches "_" (symbol-name text)) ?^)))
     ((symbolp text)
      (funcall fmtter index (symbol-name text)))
     ((not (stringp text))
      (error "bad arg to hydraw char formatter" text))
     ((or (string-equal "T/F" text) (s-contains? "%" text))
      text)
     ((not (s-contains? "_" text))
      (funcall fmtter index text))
     (t
      (concat text (make-string (s-count-matches "_" text) ?^)))
     )
     )
    )

;;;###autoload
(defun +jg-hydra-format-columns (&rest columns)
  (let* ((fmt (mapcar (-partial #'mapcar #'+jg-hydra-first-char-format) columns))
         (titles (mapcar #'car columns))
         (max-row (apply #'max (mapcar #'length columns)))
         (pad-str " ")
         (empty-pad-amnt 4)
         (concat-str "^^^^")
         (norm-pad-amnt 2)
         padded
         fmt-columns
         header
         )
    (cl-loop for column in fmt
             do
             (let* ((longest (if (string-equal (car column) "T/F") 6 (apply #'max (mapcar #'length column))))
                    (empty-lines (mapcar (-partial (-compose (-partial #'concat concat-str) #'make-string) (- longest empty-pad-amnt))
                                         (make-list (max 0 (- max-row (length column))) (string-to-char pad-str))))
                   )
               (push (mapcar (-partial #'s-pad-right (+ longest norm-pad-amnt) pad-str)
                             (append column empty-lines))
                     padded)
               )
             )
    (setq fmt-columns (mapcar (-rpartial #'string-join " | ")
                              (apply #'-zip-lists (reverse padded))
                              )
          header (car fmt-columns)
          )
    (string-join (append (list header
                               (make-string (- (length header) (* 4 (length columns))) ?-))
                         (cdr fmt-columns))
     "\n")
    )
  )

;; (defhydra jg-test-hydra ()
;;   (format "%s\n" (+jg-hydra-format-columns
;;                   '(vis_u_als hl-line modeline)
;;                   '(gu_i_des whitespace bloo)
;;                   '(wrapping truncate smartparen)
;;                   '(navigation auto-hide cursor eww preview)
;;                   )
;;           )

;;   ("a" nil)
;;   ("c" nil)
;;   ("e" nil)
;;   ("g" nil)
;;   ("h" nil)
;;   ("m" nil)
;;   ("n" nil)
;;   ("p" nil)
;;   ("s" nil)
;;   ("t" nil)
;;   ("v" nil)
;;   ("i" nil)
;;   ("w" nil)
;;   ("b" nil)
;;   ("u" nil)
;;   )

;; (+jg-hydra-format-columns
;;  '(Visuals
;;    "_g_ Evil goggles          "
;;    "_H_ Highlight Symbols     "
;;    "_h_ Hl-line               "
;;    "_I_ Ignore Invisible      "
;;    "_p_ Highlight Parens      "
;;    "_r_ Rainbow Mode          "
;;    "_s_ Prettify Symbols Mode "
;;    )
;;  '(Markers
;;    " %(+jg-hydra-doc evil-goggles-mode)"
;;    " %(+jg-hydra-doc auto-highlight-symbol-mode)"
;;    " %(+jg-hydra-doc hl-line-mode)"
;;    " %(+jg-hydra-doc line-move-ignore-invisible)"
;;    " %(+jg-hydra-doc global-highlight-parentheses-mode)"
;;    " %(+jg-hydra-doc rainbow-mode)"
;;    " %(+jg-hydra-doc prettify-symbols-mode)"
;;    )
;;  )

;; (+jg-hydra-format-columns '(
;;                             "Visuals
;; -------------------------
;; _g_ Evil goggles          %(+jg-hydra-doc evil-goggles-mode)
;; _H_ Highlight Symbols     %(+jg-hydra-doc auto-highlight-symbol-mode)
;; _h_ Hl-line               %(+jg-hydra-doc hl-line-mode)
;; _I_ Ignore Invisible      %(+jg-hydra-doc line-move-ignore-invisible)
;; _p_ Highlight Parens      %(+jg-hydra-doc global-highlight-parentheses-mode)
;; _r_ Rainbow Mode          %(+jg-hydra-doc rainbow-mode)
;; _s_ Prettify Symbols Mode %(+jg-hydra-doc prettify-symbols-mode)"

;; ""
;; ))
