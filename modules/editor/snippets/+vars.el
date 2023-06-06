;;; +vars.el -*- lexical-binding: t; -*-

;; Reduce default verbosity. 3 is too chatty about initializing yasnippet. 2
;; is just right (only shows errors).

;;-- defs
(defvar yas-verbosity 2)

(defvar +snippets--smartparens-enabled-p t)

(defvar +snippets--expanding-p nil)

(defvar +file-templates-default-trigger "__")

(defvar +file-templates-inhibit nil)

(defvar jg-snippet-dirs nil)

(defvar jg-snippets-code-templates-dir    (expand-file-name "templates/code" doom-user-dir))

(defvar jg-snippets-file-templates-dir    (expand-file-name "templates/files" doom-user-dir))

(defvar jg-snippets-project-templates-dir (expand-file-name "templates/projects" doom-user-dir))

(defvar +snippets-dir       jg-snippets-code-templates-dir)

(defvar +file-templates-dir jg-snippets-file-templates-dir)

(defvar yas-snippet-dirs    (list jg-snippets-code-templates-dir))

;;-- end defs

;; default snippets library, if available
(add-to-list 'load-path jg-snippets-code-templates-dir)
(setq yas-indent-line 'fixed)


(spec-handling-add! file-templates
                    '(general
                      ("/docker-compose\\.yml$" :mode yaml-mode)
                      ;; direnv
                      ("/\\.envrc$" :trigger "__envrc" :mode direnv-envrc-mode)
                      ;; Markdown
                      (markdown-mode)
                      ;; Markdown
                      (sh-mode :priority -100)
                      (gitignore-mode :priority -100)
                      (dockerfile-mode)
                      (snippet-mode)
                      )
 )

(spec-handling-setq! snippets
                     +file-templates-dir jg-snippets-file-templates-dir
                     +snippets-dir       jg-snippets-code-templates-dir
                     yas-snippet-dirs    (-filter #'identity (append (list jg-snippets-code-templates-dir jg-snippets-file-templates-dir)
                                                                     jg-snippet-dirs))
                     yas--default-user-snippets-dir jg-snippets-code-templates-dir
                     yas-prompt-functions '(+jg-snippets-yas-prompt-fn)
                     )

(spec-handling-add! lookup-regular
                    '(snippet-mode-map
                     ("Yasnippet Manual" . "https://joaotavora.github.io/yasnippet/snippet-development.html")
                     )
                    )

(spec-handling-add! company
                    '(yas-minor-mode (:back . company-yasnippet))
                    )
