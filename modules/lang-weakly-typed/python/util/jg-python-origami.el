;;; origami-parser.el --- custom origami parsers -*- lexical-binding: t -*-
(require 'dash)
(require 's)


(defun +jg-line-starts-with? (text)
  (s-starts-with? text (s-trim-left (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position))))
  )

(defun +jg-origami-python-parser (create)
    (lambda (content)
      (with-temp-buffer
        (insert content)
        (python-mode)
        (goto-char (point-min))
        (beginning-of-defun -1)
        (let (beg (end (point-max)) offset acc class_params children)
          (while (not (= (point) end))
            ;;if gone beyond a class
            (if (and class_params (> (point) (car class_params)))
                (setq acc (cons (funcall create (caddr class_params) (car class_params) (cadr class_params) children) acc)
                      class_params nil
                      children nil
                      ))
            (setq beg (point))
            (search-forward-regexp ":" nil t)
            (setq offset (- (point) beg))
            (if (+jg-line-starts-with? "class")
                ;; go to end of class, then return to do methods
                (save-excursion
                  (end-of-defun)
                  (setq end (point))
                  (when (and (> offset 0) (> end beg))
                    (setq class_params `(,end ,offset ,beg)))) ;;want car most
              ;; else not a class
              (save-excursion
                (end-of-defun)
                (setq end (point))
                (when (and (> offset 0) (> end beg))
                  (let ((new_node (funcall create beg end offset nil)))
                    (if class_params (push new_node children)
                      (setq acc (cons new_node acc)))
                    ))))

            (let ((current (point)))
                  (beginning-of-defun -1)
                  (if (eq current (point))
                      (end-of-defun)))

            )
          ;; cleanup last class if necessary
          (if class_params
              (setq acc (cons (funcall create (caddr class_params) (car class_params) (cadr class_params) children) acc)
                    class_params nil
                    children nil
                    ))
          (reverse acc))))
    )

(provide 'jg-python-origami)
