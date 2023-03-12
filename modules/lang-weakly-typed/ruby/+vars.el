;;; +vars.el -*- lexical-binding: t; -*-

(sp-local-pair 'ruby-mode "{" "}"
               :pre-handlers '(:rem sp-ruby-pre-handler)
               :post-handlers '(:rem sp-ruby-post-handler))
