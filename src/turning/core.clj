(ns turning.core
  (:gen-class))

(defn long-string [& strings] (clojure.string/join "\n" strings))

(def input-text (long-string
                 "Hello, World!"
                 "This is just a test"))

(defn md->html [input]
  (map (fn [line] (str "<p>" line "</p>"))
       (clojure.string/split input #"\n")))

(defn transform [fn txt]
  (fn txt))

(transform md->html input-text)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (transform md->html input)))

