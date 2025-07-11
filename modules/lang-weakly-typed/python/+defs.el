;; +defs.el -*- lexical-binding: t; -*-

(defvar jg-python-current-interpreter nil
  "The current command to use when starting a repl"
  )

(defvar jg-python-stock-repl '("python3" "-i")
  "the default python interpreter")

(defvar +python-ipython-command '("python3" "-m" "IPython" "-i" "--simple-prompt")
  "Command to initialize the ipython REPL for `+python/open-ipython-repl'.")

(defvar python-shell-interpreter-path-args nil
  "arguments to go at the end of the python interpreter call if a file is not provided to run"
  )

(defvar +python-jupyter-command '("jupyter" "console" "--simple-prompt")
  "Command to initialize the jupyter REPL for `+python/open-jupyter-repl'.")

(defvar jg-python-dev-mode nil)

(defvar jg-python-dev-cmd-args '("-X" "dev"))

(defvar jg-python-docs-url           "https://docs.python.org/3/")

(defvar jg-python-lib-url-suffix     "library/%s.html")

(defvar jg-python-import-block-end-re "^\\(__all__\\|[[:graph:]]+?\\s-+=\\|def\\|class\\|if TYPE_CHECKING:\\)")

(defvar jg-python-summary-buffer      "*Python-Summary*")

(defvar jg-python-mode-map (make-sparse-keymap))
(defvar jg-python-ts-mode-map (make-sparse-keymap))
