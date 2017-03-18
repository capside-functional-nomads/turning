(ns turning.core-test
  (:require [clojure.test :refer :all]
            [turning.core :refer :all]
            [turning.parser :refer :all]))
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
