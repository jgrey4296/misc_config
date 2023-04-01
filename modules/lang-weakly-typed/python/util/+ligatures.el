
  (set-ligatures! 'python-mode
    ;; Functional
    :def    "def"
    :lambda "lambda"
    ;; Types
    :null   "None"
    :true   "True" :false "False"
    :int    "int"  :str "str" :float  "float" :bool   "bool" :tuple  "tuple"
    ;; Flow
    :not    "not"
    :in     "in"  :not-in "not in"
    :and    "and" :or "or"
    :for    "for"
    :return "return" :yield "yield")
