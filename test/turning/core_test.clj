(ns turning.core-test
  (:require [clojure.test :refer :all]
            [turning.core :refer :all]
            [turning.parser :refer :all]
            [turning.markdown :as m]))
#_(require 'turning.parser)

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 0))))

(deftest parse-char-a-test
  (testing "Parse char 'a'"
    (is (= {:success ["a" "bc"]} (parse-char-a "abc")))
    (is (= {:failure "b"} (parse-char-a "b")))))

(deftest parse-char-test
  (testing "Parse any char"
    (let [parse (parse-char \b)]
      (is (= {:success ["b" "ac"]} (parse "bac")))
      (is (= {:failure "abc"} (parse "abc"))))))

(deftest parse-or
  (testing "Parse a char or other char"
    (let [p1 (parse-char \a)
          p2 (parse-char \b)
          parse (p-or p1 p2)]
      (is (= {:success ["a" "bc"]} (parse "abc")))
      (is (= {:success ["b" "ac"]} (parse "bac"))))))

(deftest parse-and
  (testing "Parse a char and another char next"
    (let [p1 (parse-char \a)
          p2 (parse-char \b)
          parseab (p-and p1 p2)
          parseba (p-and p2 p1)]
      (is (= {:success ["ab" "c"]} (parseab "abc")))
      (is (= {:success ["ba" "c"]} (parseba "bac"))))))

(deftest parse-or-and
  (testing "Parse ab or cd"
    (let [a (parse-char \a)
          b (parse-char \b)
          c (parse-char \c)
          d (parse-char \d)
          ab (p-and a b)
          cd (p-and c d)
          parse (p-or ab cd)]
      (is (= {:success ["ab" "e"]} (parse "abe")))
      (is (= {:success ["cd" "e"]} (parse "cde"))))))

(deftest with-apply
  (testing "With apply"
    (let [a (parse-char \a)
          bB (p-apply (parse-char \b) clojure.string/upper-case)
          c (parse-char \c)
          parse (p-and (p-and a bB) c)]
      (is (= {:success ["aBc" "d"]} (parse "abcd"))))))

(deftest with-apply
  (testing "With apply 2"
    (let [a (parse-char \a)
          aaa (p-and (p-and a a) a)
          AAA (p-apply aaa (fn [_] "XXX"))]
      (is (= {:success ["XXX" "c"]} (AAA "aaac"))))))

(deftest many-test
  (testing "many"
    (let [a (parse-char \a)
          manya (p-many a)
          manya1 (p-many1 a)]
      (is (= {:success ["a" "b"]} (manya "ab")))
      (is (= {:success ["aa" "b"]} (manya "aab")))
      (is (= {:success ["" "baaab"]} (manya "baaab")))
      (is (= {:failure "baaab"} (manya1 "baaab"))))))

(deftest more-many
  (testing "Defining more parsers in one shot"
    (let [any-abc (p-any-char "abc")]
      (is (= {:success ["a" "kkk"]} (any-abc "akkk")))
      (is (= {:success ["b" "kkk"]} (any-abc "bkkk")))
      (is (= {:success ["c" "kkk"]} (any-abc "ckkk")))
      (is (= {:failure "dkkk"} (any-abc "dkkk"))))))

(defn bold-parser []
  (let [lowercase (clojure.string/join (map char (range (int \a) (int \z))))
        uppercase (clojure.string/join (map char (range (int \A) (int \Z))))
        lower (p-any-char lowercase)
        upper (p-any-char uppercase)
        alpha (p-or lower upper)
        word (p-many alpha)
        star (parse-char \*)]
    (p-and (p-and star word) star)))

(deftest parse-bold-test
  (testing "Can parse bold"
    (let [parse (bold-parser)]
      (is (success? (parse "*afAF*"))))))

(defn transform-with-bolds []
  (let [chop-first (fn [s] (subs s 1))
        chop-last (fn [s] (let [c (count s)] (subs s 0 (- c 1))))
        strong (fn [s] (str "<strong>" s "</strong>"))]
    (p-apply (bold-parser)
             (fn [s]
               (->> s chop-first chop-last strong)))))

(deftest transform-bolds-to-strong-test
  (let [parse (transform-with-bolds)]
    (is (success? (parse "*abc*")))
    (is (= {:success ["<strong>abc</strong>" "def"]} (parse "*abc*def")))))


(deftest lists-and-bolds-and-italics
  (let [t "  * Item *bold* of list\n  * item _italic_ of list\n  * other\n"]
    (is (= {:success ["<ul><li>Item <b>bold</b> of list</li><li>item <i>italic</i> of list</li><li>other</li></ul>" ""]} (m/ulist t)))))
