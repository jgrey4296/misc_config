;;; editor/project-walk/+bindings.el -*- lexical-binding: t; -*-

;; (map! :after project-walk
;;       :leader
;;       (:prefix "p"
;;        "W" :desc "Project Walk" #'project-walk-minor-mode
;;        "n" :desc "Walk to next" #'project-walk-next
;;        )
;;       )

(doom--define-leader-key :infix "p"
                         "W" :desc "Project Walk" #'project-walk-minor-mode
                         "n" :desc "Walk to next" #'project-walk-next)
