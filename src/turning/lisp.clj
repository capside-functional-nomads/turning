(ns turning.lisp
  (:require [turning.parser :as p]))

(def lowercase (clojure.string/join (map char (range (int \a) (int \z)))))
(def uppercase (clojure.string/join (map char (range (int \A) (int \Z)))))

(def lower (p/p-any-char lowercase))
(def upper (p/p-any-char uppercase))
(def alpha (p/p-or lower upper))
(def digit (p/p-any-char "0123456789"))

(def other-chars-in-symbol (p/p-any-char "-"))
(def psymbol
  (p/p-or alpha
          (p/p-and alpha (p/p-many other-chars-in-symbol))))

(def pnumber (p/p-many digit))

(defn digit-val [d]
  (condp = d
    \0 0
    \1 1
    \2 2
    \3 3
    \4 4
    \5 5
    \6 6
    \7 7
    \8 8
    \9 9))
(defn eval-number [s]
  (let [ns (map digit-val s)]
    (reduce (fn [acc n] (+ n (* 10 acc))) ns)))
(def number (p/p-apply pnumber eval-number))

(def literal (p/p-or symbol number))

(def lparen (p/parse-char "("))
(def rparen (p/parse-char ")"))
(def plist (p/p-seq lparen (p/p-many literal) rparen))

