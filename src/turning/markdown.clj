(ns turning.markdown
  (:require [turning.parser :as p]))

(def lowercase (clojure.string/join (map char (range (int \a) (int \z)))))
(def uppercase (clojure.string/join (map char (range (int \A) (int \Z)))))
(def accentedletters "àèéíòóúÀÈÉÍÒÓÚ")
(def punctuationletters "!?")

(def lower (p/p-any-char lowercase))
(def upper (p/p-any-char uppercase))
(def accented (p/p-any-char accentedletters))
(def punctuation (p/p-any-char punctuationletters))
(def alpha (p/p-or (p/p-or lower upper) accented))
(def word (p/p-many alpha))
(def space (p/p-char \space))
(def whitespace (p/p-many space))

(def star (p/p-char \*))
(def underscore (p/p-char \_))

(def bold (p/p-and (p/p-and star word) star))
(def italic (p/p-and (p/p-and underscore word) underscore))

(defn chop-first
  ([s] (subs s 1))
  ([s n] (subs s n)))
(defn chop-last
  [s]
  (let [c (count s)]
    (subs s 0 (- c 1))))
(defn b [s]
  (str "<b>" s "</b>"))
(defn i [s]
  (str "<i>" s "</i>"))

(def bold->b (p/p-apply bold
                      (fn [s] (->> s chop-first chop-last b))))
(def italic->i (p/p-apply italic
                          (fn [s] (->> s chop-first chop-last i))))

(def text (p/p-many
           (p/p-or
            (p/p-or
             (p/p-or word whitespace) punctuation)
            (p/p-or bold->b italic->i)))
  #_(p/p-any word whitespace bold italic))

(def nl (p/p-char \newline))
(def indent (p/p-times space 2))

(def uli (p/p-seq (p/p-times space 2)
                  star
                  space
                  text
                  nl))

(defn li [s] (str "<li>" s "</li>"))
(def uli->li (p/p-apply uli
                        (fn [s] (->> (chop-first s 4) chop-last li))))
(def unordered-list (p/p-many uli->li))

(defn ul [s] (str "<ul>" s "</ul>"))
(def ulist
  (p/p-apply unordered-list
             (fn [s] (->> s ul))))
