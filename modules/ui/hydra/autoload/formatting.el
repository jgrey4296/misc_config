;;; +formatting.el -*- lexical-binding: t; -*-
(require 's)
(require 'dash)

(defun +jg-ui-combine-columns (textlst)
  (let* ((rows-of-cols (mapcar (lambda (x)
                                 (mapcar 's-trim (split-string x "\n")))
                               textlst))
         (longest-in-col (mapcar (-partial 'apply 'max)
                                 (mapcar (-partial 'mapcar 'length)
                                         rows-of-cols)))
         (padded (cl-loop for col in (-zip-lists longest-in-col rows-of-cols)
                          collect
                          (mapcar (-partial 's-pad-right (car col) " ") (cadr col))
                          ))
         (line-counts (mapcar #'length rows-of-cols))
        )
    (message "Got %s columns of %s lines of lengths %s"
             (length rows-of-cols) line-counts longest-in-col)
    (concat "\n" (string-join (cl-loop for cols in (apply '-zip-fill " " (make-list (length rows-of-cols) " ") padded)
                          collect
                          (string-join cols " | ")
                          )
                 "\n"))
    )
  )

;;;###autodef
(defun +jg-hydra-format-columns (textlst)
  " particularly for hydra docs "

  (let* (;; Splt columns into lists of rows
         (rows-of-cols (mapcar (lambda (x)
                                 (mapcar 's-trim (split-string x "\n")))
                               textlst))
         ;; Get the longest row
         (longest-in-col (funcall (-compose (-partial 'mapcar
                                                      (-partial 'apply 'max))
                                            (-partial 'mapcar
                                                      (-partial 'mapcar 'length)))
                                  rows-of-cols))
         ;; Pad all columns in each row
         (padded (cl-loop for col in (-zip-lists longest-in-col rows-of-cols)
                          collect
                          (mapcar (-partial 's-pad-right (car col) "^") (cadr col))
                          ))
         ;; Count the number of columns in each column
         (row-counts (mapcar #'length rows-of-cols))
         ;; make a tuple of the row count with the padded columns
         (tuple-pad (apply '-zip-fill " " (make-list (length rows-of-cols) " ") padded))
        )
    ;; Join all column's for each row
    ;; Then join all rows
    (concat "\n" (string-join
                  (cl-loop for cols in tuple-pad
                           collect
                           (string-join cols " | ")
                          )
                  "\n"))
    )
  )
