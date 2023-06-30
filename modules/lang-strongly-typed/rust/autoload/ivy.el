;;; +ivy.el -*- lexical-binding: t; -*-
(require 'ivy)

;; TODO add action for inserting new entries into dependency list

;;;###autoload
(defvar jg-rust-ivy-stack-types '("bool" "u8" "u16" "u32" "u64" "u128" "i8" "i16" "i32" "i64" "i128" "f32" "f64" "char" "str" "!" "(u8, u8)" "[u8; 5]"))
;;;###autoload
(defvar jg-rust-ivy-heap-types '("Box<dyn Fn(u8) -> u8>" "String" "Vec<u8>" "VecDeque<T>" "LinkedList<T>" "HashMap<K, V>" "BTreeMap<K, V>" "HashSet<T>" "BTreeSet<T>" "BinaryHeap<T>" "Option<T>" "Result<T, Err>" "Rc<T>" "Weak<T>" "Cell<T>" "RefCell<T>"))
;;;###autoload
(defvar jg-rust-ivy-derives '("Debug" "PartialEq" "Eq" "PartialOrd" "Ord" "Clone" "Copy" "Hash" "Default"))
;;;###autoload
(defvar jg-rust-ivy-traits '("Iterator" "Add" "Sized"))
;;;###autoload
(defvar jg-rust-ivy-val '("Some(v)" "None" "Ok(v)" "Err(E)"))

(defvar jg-rust-dependency-loc (doom-module-expand-path :lang-strongly-typed 'rust  "_data/dependencies.ivy"))
(defvar jg-rust-dependency-collection nil)


;;;###autoload
(defun +jg-rust-dependency-ivy ()
  (interactive)
  ;; Read dependencies.ivy
  (unless jg-rust-dependency-collection
    (setq jg-rust-dependency-collection
          (with-temp-buffer (insert-file jg-rust-dependency-loc)
                            (split-string (buffer-string) "\n" t))))

  ;; provide ivy for them
  (insert (ivy-read "Add Dependency: " jg-rust-dependency-collection))
  )

;;;###autoload
(defun +jg-rust-main-ivy ()
  (interactive)
  (ivy-read "Insert: " (hash-table-keys jg-rust-company-kws) :require-match t
            :action #'(lambda (x)
                        (ivy-read (format "%s: " x)
                                  (gethash x jg-rust-company-kws)
                                  :require-match t
                                  :action #'insert)))
  )
