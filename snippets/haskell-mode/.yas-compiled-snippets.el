;;; Compiled snippets and support files for `haskell-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'haskell-mode
                     '(("typeclass" "class BasicEq a where\n  isEqual :: a -> a -> Bool\n  isEqual x y = not (isEqual x y)\n\ninstance BasicEq Bool where\n  isEqual True  True  = True\n  isEqual False False = True\n  isEqual _     _     = False\n" "typeclass" nil nil nil "/Users/jgrey/github/jg_emacs_files/snippets/haskell-mode/typeclass" nil nil)
                       ("regex" "--\"my test\" =~ \"test\" :: Bool\n--\"my test\" =~ \"t.*\" :: String\n\n" "regex" nil nil nil "/Users/jgrey/github/jg_emacs_files/snippets/haskell-mode/regex" nil nil)
                       ("record" "data Test = Test { theInt :: Int, theString :: String }\nchng x y = x { theInt = y }" "record" nil nil nil "/Users/jgrey/github/jg_emacs_files/snippets/haskell-mode/record" nil nil)
                       ("precedence" "(+) :: a -> a -> a\n-- infixl 5 +\n-- infixr 5 +" "precedence" nil nil nil "/Users/jgrey/github/jg_emacs_files/snippets/haskell-mode/precedence" nil nil)
                       ("pragmas" "{-# LANGUAGE TypeSynonymInstances, OverlappingInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}" "pragmas" nil nil nil "/Users/jgrey/github/jg_emacs_files/snippets/haskell-mode/pragmas" nil nil)
                       ("module" "module name\n(\n    exportedValues\n    , ExportedType(...)\n\n) where\n$0" "module" nil nil nil "/Users/jgrey/github/jg_emacs_files/snippets/haskell-mode/module" nil nil)
                       ("map" "map (\\x -> x + 2) [1,2,3,4]" "map" nil nil nil "/Users/jgrey/github/jg_emacs_files/snippets/haskell-mode/map" nil nil)
                       ("main" "main = do\n     $1" "main" nil nil nil "/Users/jgrey/github/jg_emacs_files/snippets/haskell-mode/main" nil nil)
                       ("listComprehension" "--let toPairs xs ys = [(x,y) | x <- xs, y <- ys]\n--let withGuards xs ys = [(x,y) | (x,y) <- zip xs ys, x+y < 60, x+y > 20]" "listComprehension" nil nil nil "/Users/jgrey/github/jg_emacs_files/snippets/haskell-mode/listComprehension" nil nil)
                       ("lambda" "\\x -> x + 1" "lambda" nil nil nil "/Users/jgrey/github/jg_emacs_files/snippets/haskell-mode/lambda" nil nil)
                       ("import" "import Control.Monad\nimport Control.Monad.State\n--import System.Environment (getArgs)\n--import qualified Data.Map as Map\n--import Text.Regex.Posix\n--import System.FilePath\n--import qualified Data.ByteString.Lazy as L" "import" nil nil nil "/Users/jgrey/github/jg_emacs_files/snippets/haskell-mode/import" nil nil)
                       ("fold" "foldl (\\m v -> m + v) 0 [1,2,3,4]\n--foldr" "fold" nil nil nil "/Users/jgrey/github/jg_emacs_files/snippets/haskell-mode/fold" nil nil)
                       ("do" "do { state <- get; put (state-1); return (val+1) }" "do" nil nil nil "/Users/jgrey/github/jg_emacs_files/snippets/haskell-mode/do" nil nil)
                       ("case" "case $1 of\n     $2 -> $3\n     _  -> $4" "case" nil nil nil "/Users/jgrey/github/jg_emacs_files/snippets/haskell-mode/case" nil nil)
                       ("association" "let a = [(1, \"blah\"), (2, \"bloo\")]\nlookup 2 a" "association" nil nil nil "/Users/jgrey/github/jg_emacs_files/snippets/haskell-mode/association" nil nil)))


;;; Do not edit! File generated at Sun Feb  5 10:57:51 2017
