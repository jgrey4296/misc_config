;;; invisible.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar-local jg-invis-names '(jg-text-invis jg-comment-invis))

;;;###autoload
(defun +jg-text-manipulation-make-comments-invisible ()
  " from https://stackoverflow.com/questions/17628985
         https://emacs.stackexchange.com/questions/13343
 "
  (interactive)
  (save-excursion
    (save-restriction
      (goto-char (point-max))
      (+evil/previous-comment 1)
      (while (and (not (bobp)) (evil-in-comment-p (point)))
        (-when-let (comment (evilnc-get-comment-bounds))
          (add-text-properties (car comment) (cdr comment) '(invisible jg-comment-invis))
          )
        (+evil/previous-comment 1)
        )
      )
    )
  )

;;;###autoload
(defun +jg-text-manipulate-invis-spec ()
  " add or remove symbols to the invis spec "
  (interactive)
  (unless (listp buffer-invisibility-spec)
    (setq-local buffer-invisibility-spec (list t)))
  (ivy-read "Invis Spec: "
            (mapcar #'(lambda (x)
                        (cons (format "%-20s : %s" x
                                      (if (assoc x buffer-invisibility-spec)
                                          "active"
                                        " - "))
                              x))
                    jg-invis-names)
            :action #'(lambda (x) (+jg-text-toggle-invisible-spec (cdr x)))
            )
  )

;;;###autoload (autoload '+jg-text-toggle-invisible-spec "editor/text-manipulation/autoload/invisible" nil t)
(defun +jg-text-toggle-invisible-spec (&optional name)
  (interactive)
  (let ((name-sym (cond ((symbolp name)
                         name)
                        ((stringp name)
                         (intern name))
                        ((and (consp name) (symbolp (cdr name)))
                         (cdr name))
                        (t 'jg-text-invis)))
        )
    (cond ((and (listp buffer-invisibility-spec)
                (assoc name-sym buffer-invisibility-spec))
           (setq buffer-invisibility-spec
                 (-reject #'(lambda (x) (eq (car-safe x) name-sym))
                          buffer-invisibility-spec)))
          ((listp buffer-invisibility-spec)
           (push `(,name-sym . t) buffer-invisibility-spec)
           )
          (t
           (setq buffer-invisibility-spec (cons `(,name-sym . t)
                                                buffer-invisibility-spec))
           )
          )
    )
  )

;;;###autoload (autoload '+jg-text-make-invisible "editor/text-manipulation/autoload/invisible" nil t)
(evil-define-operator +jg-text-make-invisible (beg end type)
  " Operator to easily annotate text to be hidden "
  :type inclusive
  (interactive "<R>")
  (put-text-property beg end 'invisible 'jg-text-invis)
  )

;;;###autoload (autoload '+jg-text-delete-invisible "editor/text-manipulation/autoload/invisible" nil t)
(evil-define-operator +jg-text-delete-invisible (beg end type)
  " Operator to easily annotate text to be hidden "
  :type inclusive
  (interactive "<R>")
  (put-text-property beg end 'invisible nil)
  )

;;;###autoload (autoload '+jg-text-toggle-invisible "editor/text-manipulation/autoload/invisible" nil t)
(evil-define-operator +jg-text-toggle-invisible (beg end type prefix)
  " Operator to show invisible text again "
  :type inclusive
  ;; :keep-visual t
  (interactive "<R>P")
  ;; Toggle the property
  (alter-text-property beg end 'invisible
                       (lambda (val)
                         (cond (prefix nil)
                               ((eq val 'jg-text-invis)
                                'jg-text-invis-disabled
                                )
                               ((eq val 'jg-text-invis-disabled)
                                'jg-text-invis)
                               (t 'jg-text-invis)
                               )
                         )
                       )
  )

;;;###autoload (autoload '+jg-text-name-invisible "editor/text-manipulation/autoload/invisible" nil t)
(evil-define-operator +jg-text-name-invisible (beg end type prefix)
  " Operator to show invisible text again "
  :type inclusive
  ;; :keep-visual t
  (interactive "<R>P")
  (let ((sym (intern (read-string "Invisiblity Name: "))))
    ;; Toggle the property
    (put-text-property beg end 'invisible sym)
    (pushnew! buffer-invisibility-spec '(sym . t))
    (pushnew! jg-invis-names sym)
    )
  )
