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
(def space (p/parse-char \space))
(def whitespace (p/p-many space))

(def star (p/parse-char \*))
(def underscore (p/parse-char \_))

(def bold (p/p-and (p/p-and star word) star))
(def italic (p/p-and (p/p-and underscore word) underscore))

(defn chop-first [s] (subs s 1))
(defn chop-last [s]
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

;; (defn bold-parser []
;;   (let [lowercase (clojure.string/join (map char (range (int \a) (int \z))))
;;         uppercase (clojure.string/join (map char (range (int \A) (int \Z))))
;;         lower (p-any-char lowercase)
;;         upper (p-any-char uppercase)
;;         alpha (p-or lower upper)
;;         word (p-many alpha)
;;         star (parse-char \*)]
;;     (p-and (p-and star word) star)))

;; (defn transform-with-bolds []
;;   (let [chop-first (fn [s] (subs s 1))
;;         chop-last (fn [s] (let [c (count s)] (subs s 0 (- c 1))))
;;         strong (fn [s] (str "<strong>" s "</strong>"))]
;;     (p-apply (bold-parser)
;;              (fn [s]
;;                (->> s chop-first chop-last strong)))))

