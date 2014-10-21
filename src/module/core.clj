(ns module.core)

;;; A toy module system for Clojure.

(require 'clojure.set)

(defonce ^:dynamic
  ^{;;:private true
    :doc "A ref to a map of module names to sets of identifiers."}
  *module-registry* (ref {}))

(defonce def-forms #{'def 'defn 'defmacro})

(defn definition-id [form]
  (and (seq form)
       (>= (count form) 2) ;; FIXME: throw error if `(def id)` ?
       (contains? def-forms (first form))
       (second form)))

(defn- redefinition-error [id]
  (throw (Exception. (str "Redefinition of `" id "'"))))

(defn module-body [set form]
  (println "expand-module-body-form " form)
  (if-let [id (definition-id form)]
    (do
      (when (contains? set id)
        (redefinition-error id))
      (conj set id))
    set))

(defmacro module
  "Declares a \"module\", closer to the spirit of Racket.

  Like `ns`, a module creates a namespace and defines vars within that
  namespace. But also:

  1. Forward declarations don't require you to use explicit `declare`.

  2. Defining a var more than once is an explicit redefinition error,
  not a silent change to the new value.

  3. Removing a definition causes the original definition to be
  unmapped from the namespace upon the next evaluation of the module
  form. i.e. Deleted vars don't hang around to be used accidentally.

  Using `module` instead of `ns` has the disadvantage of increasing
  indent for the whole file. That's why eventually Racket added the
  #lang concept to create a module form automatically from the name of
  the source file. Maybe (??) even that would be possible to recreate
  in Clojure by modifying the readtable -- if Clojure supports that.

  On the other hand it has the advantage that one file may define
  multiple modules i.e. namespaces."
  [name base & forms]
  `(do
     (println "Expanding module" '~name)
     (in-ns '~name)
     (refer '~base)
     ;; First pass: `declare` all definition identifiers.
     (declare ~@(filter identity (map definition-id forms)))
     ;; Second pass: Build new set of identifiers; detect
     ;; redefinitions and undefinitions.
     (let [old-set# (or (get module.core/*module-registry* '~name) #{})
           new-set# (reduce module-body #{} (quote ~forms))]
       ;; FIXME: old-set# always seems to be empty, therefore the
       ;; unmaps are never happening.
       (println old-set# new-set#)
       (doseq [x# (clojure.set/difference old-set# new-set#)]
         (println "Unmapping disappeared definition" x#)
         (ns-unmap '~name x#))
       (dosync (commute @#'*module-registry* assoc '~name new-set#))
       nil)
     ~@forms
     (in-ns 'user)
     nil))

;; Example expansion:

(use 'clojure.pprint)
(pprint
 (macroexpand
  '(module mod-name clojure.core
           (def x 10)
           (def y 10)
           (def z 42)
           (defn g [] (f))
           (defn f [] 42))))

;; Example actual usage:

(module mod clojure.core
        (def x 10)
        (def y 20)
        (def z 42)
        (defn g [] (f))
        (defn f [] 42))
;; (ns-publics (find-ns 'mod))
