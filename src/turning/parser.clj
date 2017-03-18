(ns turning.parser
  #_(:gen-class))

(defn as-str [whatever]
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
      {:success [(as-str c) (as-str (subs s 1))]}
      {:failure (as-str s)})))

(defn success? [result]
  (contains? result :success))

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

(defn parse-char-a [s]
  (let [p (parse-char \a)]
    (p s)))

