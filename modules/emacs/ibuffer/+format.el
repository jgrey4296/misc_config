;;; +format.el -*- lexical-binding: t; -*-

;; (define-ibuffer-column SYMBOL (&key NAME INLINE PROPS SUMMARIZER) &rest BODY)

;; Redefine size column to display human readable size
(define-ibuffer-column size
  (:name "Size"
   :inline t
   )
  (file-size-human-readable (buffer-size)))

(spec-handling-add! ibuffer-formats nil
                    ('normal
                     mark modified read-only locked
                     " " (name 18 18 :left :elide)
                     " " (size 10 10 :right)
                     " " (mode 16 16 :left :elide)
                     " " project-relative-file)
                    ('vc-status
                     mark modified read-only locked
                     " " (name 18 18 :left :elide)
                     " " (size 10 10 :right)
                     " " vc-status
                     )
                    ('project
                     mark " " (name 18 18 :left :elide)
                     " " (project-name 10 10 :left)
                     " " project-relative-file
                     )
                    )
