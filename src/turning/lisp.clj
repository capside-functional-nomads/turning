(ns turning.lisp
  (:require [turning.parser :as p]))

(def lowercase (clojure.string/join (map char (range (int \a) (int \z)))))
(def uppercase (clojure.string/join (map char (range (int \A) (int \Z)))))

(def lower (p/p-any-char lowercase))
(def upper (p/p-any-char uppercase))
(def alpha (p/p-or lower upper))
(def digit (p/p-any-char "0123456789"))
(def space (p/parse-char \space))

(def other-chars-in-symbol (p/p-any-char "-"))
(def psymbol
  (p/p-or alpha
          (p/p-and alpha (p/p-many other-chars-in-symbol))))

(def pnumber (p/p-many digit))

(def digit-val
  (apply hash-map (flatten (map vector "0123456789" (range 10)))))

(defn eval-number [s]
  (let [ns (map digit-val s)]
    (reduce (fn [acc n] (+ n (* 10 acc))) ns)))
(def number (p/p-apply pnumber eval-number))

(def literal psymbol #_(p/p-or psymbol number))

(def lparen (p/parse-char \())
(def rparen (p/parse-char \)))

;; ()
;; (a)
;; (a b)
;; (a b )
(def list-item (p/p-or literal
                       (p/p-seq literal space)))
(def plist (p/p-seq lparen (p/p-many list-item) rparen))
