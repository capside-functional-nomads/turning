(ns turning.markdown
  (:require [turning.parser :as p]))

(def lowercase (clojure.string/join (map char (range (int \a) (int \z)))))
(def uppercase (clojure.string/join (map char (range (int \A) (int \Z)))))
(def accentedletters "àèéíòóúÀÈÉÍÒÓÚ")
(def punctuationletters "!?")
(def numbers "0123456789")
;(def other "ºª|@#\"·$%&/()=?¿'¡`^[+*]´¨{ç}~.:,;-_<>")
; to avoid collision with linktext and linktext exclude ()[]
(def other "ºª|@#\"·$%&/=?¿'¡`^+*´¨{ç}~.:,;-_<>")

(def lower (p/p-any-char lowercase))
(def upper (p/p-any-char uppercase))
(def accented (p/p-any-char accentedletters))
(def punctuation (p/p-any-char punctuationletters))
(def alpha (p/p-or (p/p-or lower upper) accented))
(def numeric (p/p-any-char numbers))
(def alphanum  (p/p-or numeric alpha))
(def othersymbols (p/p-any-char other))
(def alphanumother (p/p-or alphanum othersymbols ))
(def word (p/p-many alphanum))
(def word2 (p/p-many alphanumother))
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
             (p/p-or word2 whitespace) punctuation)
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

; links 
(def linksymbols ":/@&=?.")
(def urltext (p/p-many (p/p-or  
                  (p/p-any-char linksymbols)
                  alphanum)))
(urltext "a")
(urltext "http://")
(urltext "http://ackcent.com")
(urltext "https://user@password:www.domain.com")
(urltext "https://user@password:www.domain.com?param1=a&param2=3")

(def urltextcomment (p/p-and urltext text))
(urltextcomment "https://www.google.com \"Google's page\"")

(def textopen (p/p-char \[ ))
(def textclose (p/p-char  \]  ))
(def linkopen (p/p-char \( ))
(def linkclose (p/p-char  \)  ))
(def linktext (p/p-and (p/p-and textopen text) textclose))
(def linklink (p/p-and (p/p-and linkopen urltextcomment) linkclose))
(def turninglink (p/p-and linktext (p/p-or linktext linklink)))

(textclose "]")
(textopen "[")
(linkopen "(")
(linkclose ")")
(linktext "[ this is text ]")
(linktext "[ this is my 2nd text ]")
(alphanumother "'-")
(word2 "I'm")
(text "I'm a nerd")
(text "")
(linktext "[ this is my 3rd text with strange chars -'  ]")
(turninglink "[hello this is my link text I'm excited!](http://www.example.org)")
(turninglink "[some empty link]()")
;using examples from https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet#links
(turninglink  "[I'm an inline-style link](https://www.google.com)")
(turninglink  "[I'm a relative reference to a repository file](../blob/master/LICENSE)")
(turninglink  "[I'm a reference-style link][Arbitrary case-insensitive reference text]")
(turninglink  "[You can use numbers for reference-style link definitions][1]")
(turninglink  "[I'm an inline-style link with title](https://www.google.com \"Google's Homepage\")")

;create a func that receives a link and outputs the corresponding
;extract text between [ ] and put it as the link value
;extract text between ( ) and put it as the link href
; if link is [1] then put #in the href
; if link () contains text , look for whitespace , then text must go on .. I don't know
(defn htmllink
  "receives a parsed link in markdown and outputs the corresponding html"
  [mdstring]
  (let [s (first  (:success (turninglink mdstring)))
        s1 (clojure.string/split s #"\]")
        linktext (get (clojure.string/split  (get s1 0) #"\["  ) 1 )
        linklink1 (get s1 1) 
        linklink2 (get (clojure.string/split linklink1 #"[\(\[]"  ) 1 )
        linklink3 (get (clojure.string/split linklink2 #"[\)\]]" ) 0)
        linklink4 (get (clojure.string/split linklink3 #" "  ) 0 )
        ]
    (str "<a href=\"" linklink4 "\">" linktext "</a>")
    )
)
(htmllink  "[I'm an inline-style link](https://www.google.com)")
(htmllink  "[I'm a relative reference to a repository file](../blob/master/LICENSE)")
(htmllink  "[You can use numbers for reference-style link definitions][1]")
(htmllink  "[I'm an inline-style link with title](https://www.google.com \"Google's Homepage\")")
;an if branch is needed to correctly parse this case
(htmllink  "[I'm a reference-style link][Arbitrary case-insensitive reference text]")
