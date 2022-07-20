;;; +file-templates.el -*- lexical-binding: t; -*-

(after! jg-file-templates
  (set-file-templates!
   '(org-mode :trigger "__")
   '(org-journal-mode :ignore t)

   '("README\\.org$"        :trigger "__doom-readme"            :mode org-mode :when +file-templates-in-emacs-dirs-p )
   '("contact\\.org$"       :trigger "__contact"                :mode org-mode)
   '("invoice\\.org$"       :trigger "__invoice"                :mode org-mode)
   '("project\\.org$"       :trigger "__project"                :mode org-mode)
   '("design_doc\\.org$"    :trigger "__designDocNotes"         :mode org-mode)
   '("inst_pipeline\\.org$" :trigger "__institution_pipeline"   :mode org-mode)
   '("lit_review\\.org$"    :trigger "__lit_review"             :mode org-mode)
   '("two_pager\\.org$"     :trigger "__pacheco_vega_two_pager" :mode org-mode)
   )
  )
