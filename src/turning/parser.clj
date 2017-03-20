(ns turning.parser
  #_(:gen-class))

(defn- as-str [whatever]
  (cond (char? whatever) (str whatever)
        (empty? whatever) ""
        (string? whatever) whatever))

(defn- success
  "Creates a success"
  [parsed nonparsed]
  {:success [parsed nonparsed]})

(defn- fail
  "Creates a failure"
  [s]
  {:failure (as-str s)})

(defn- get-nonparsed
  "Extracts next string to be parsed from a success"
  [suc]
  (let [[parsed nonparsed] (:success suc)]
    nonparsed))

(defn- get-parsed
  "Extracts parsed part from success"
  [suc]
  (let [[parsed nonparse] (:success suc)]
    parsed))

(defn parse-char
  "Returns a char parser"
  [c]
  (fn [s]
    (if (= c (first s))
      (success (as-str c) (subs s 1))
      (fail s))))
                                        ; -> any? pass?
(defn p-*
  []
  (fn [s]
    (success (as-str (first s)) (subs s 1))))

(defn success? [result]
  (contains? result :success))
(defn failure? [result]
  (contains? result :failure))

(defn p-or [p1 p2]
  (fn [s]
    (let [r1 (p1 s)]
      (if (success? r1)
        r1
        (p2 s)))))

(defn p-and [p1 p2]
  (fn [s]
    (let [r1 (p1 s)]
      (if (success? r1)
        (let [r2 (p2 (get-nonparsed r1))]
          (if (success? r2)
            (success (str (get-parsed r1) (get-parsed r2)) (get-nonparsed r2))
            (fail s)))
        (fail s)))))

(defn p-apply [p f]
  (fn [s]
    (let [r (p s)]
      (if (success? r)
        (success (f (get-parsed r))
                 (get-nonparsed r))
        r))))

(defn p-many
  "Parses 0 or more times"
  [p]
  (fn [s]
    (loop [r (p s)
           accum ""
           rest s]
      (if (failure? r)
        (success accum rest)
        (let [parsed (get-parsed r)
              nonparsed (get-nonparsed r)]
          (if (empty? nonparsed)
            (success (str accum parsed) nonparsed)
            (do
              #_(prn (str  "parsed: " (str accum parsed) " nonparsed: " nonparsed))
              (recur (p nonparsed)
                     (str accum parsed)
                     nonparsed))))))))

(defn p-many1
  "Parses 1 or more times"
  [p]
  (fn [s]
    (let [r (p s)]
      (if (success? r)
        ((p-many p) s)
        (fail s)))))

; -> one-off
(defn p-any
  "Parses any"
  [& parsers]
  (fn [s]
    (loop [p (first parsers)
           ps (rest parsers)]
      (if (nil? p)
        (fail s)
        (let [r (p s)]
          (if (success? r)
            r
            (recur (first ps) (rest ps))))))))

; -> one-char-of
(defn p-any-char
  [chars]
  (apply p-any (map parse-char chars)))


                                        ; not

(defn parse-char-a [s]
  (let [p (parse-char \a)]
    (p s)))

