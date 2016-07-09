#!/usr/bin/env boot

(set-env! :dependencies '[[org.clojure/clojure "1.9.0-alpha9"]
                          [org.clojure/tools.cli "0.3.3"]
                          [boot/core "2.6.0"]
                          ;;[tesser.core "1.0.1"]
                          [prismatic/schema "1.1.2"]])

(require  '[clojure.java.io :as io]
          '[clojure.core.reducers :as r]
          '[boot.cli :refer [defclifn]]
          ;;'[tesser.simple :as tesser :refer []]
          ;;'[tesser.core :as t :refer[]]
          '[clojure.test :refer [deftest is run-tests]]
          '[schema.core :as s]
          '[clojure.pprint :refer [cl-format]])

;;
;; For CLI scripts I tend to use boot
;; since it can then be integrated in the tooling easily
;; Also for scripts I tend to embed the tests in the same file
;; as they double as REPLable expressions - I start writing them without asserts inside a (comment ... ) form.
;; Although I have been experimenting with this style for bigger projects it does not quite work as well since there is often a need for setup and tear-down
;; I did use midje at some point but ran into edge cases where I was working against the DSL so I use plain clojure.test now. Expectations could be worth a try


(defn words
  "Returns a sequence of words in a string s
  Only letters and numbers are considered ([a-zA-Z0-9])."
  [s]
  ;; Note: the unix `wc` utility counts words differently.
  ;; Example (words "O'hara") or (words "ad-hoc) are considered two words, but `wc` considers it one in both cases. URLs also differ.
  (re-seq #"[a-zA-Z0-9]+" s))


(deftest words-test
  (is (= nil (words "")))
  (is (= '("Very" "nice" "counting" "Really" "very" "nice") (words "Very nice counting. Really very nice!")))
  (is (= '("The" "quick" "brown" "fox" "jumps" "over" "the" "lazy" "dog") (words "The quick brown fox jumps over the lazy dog.")))
  (is (= nil (words " *      *      *      *      *   ")))
  (is (= '("START" "OF" "THE" "PROJECT" "GUTENBERG" "EBOOK" "OPPORTUNITIES" "IN" "ENGINEERING")  (words "***START OF THE PROJECT GUTENBERG EBOOK OPPORTUNITIES IN ENGINEERING***")))
  (is (= '("Very" "nice" "ad" "hoc" "counting") (words "Very nice ad-hoc counting.")))
  (is (= '("Distributed" "Proofreading" "Team" "http" "www" "pgdp" "net") (words "Distributed Proofreading Team (http://www.pgdp.net)")))
  (is (= '("and" "some" "blank" "lines" "with" "spaces" "between" "words") (words "...and some \"blank lines\" with            spaces              between               words."))))


(defn naive-wordcount
  "count words in a string s"
  [s]
  (count (words s)))

(deftest naive-wordcount-test
  (is (= 0 (naive-wordcount "")))
  (is (= 6 (naive-wordcount "Very nice counting. Really very nice!")))
  (is (= 9 (naive-wordcount "The quick brown fox jumps over the lazy dog.")))
  (is (= 0 (naive-wordcount " *      *      *      *      *   ")))
  (is (= 9 (naive-wordcount "***START OF THE PROJECT GUTENBERG EBOOK OPPORTUNITIES IN ENGINEERING***")))
  (is (= 5 (naive-wordcount "Very nice ad-hoc counting.")))
  (is (= 7 (naive-wordcount "Distributed Proofreading Team (http://www.pgdp.net)")))
  )


(defn naive-wordcount-all [s]
  (let [w (words s)]
    {:chars (reduce + (map count w))
     :line  (count (clojure.string/split-lines s))
     :words (count w)}))

(deftest naive-wordcount-all-test
  (is (= {:chars 0, :line 1, :words 0}  (naive-wordcount-all "")))
  (is (= {:chars 30, :line 1, :words 6} (naive-wordcount-all "Very nice counting. Really very nice!")))
  (is (= {:chars 35, :line 1, :words 9} (naive-wordcount-all "The quick brown fox jumps over the lazy dog.")))
  (is (= {:chars 0, :line 1, :words 0} (naive-wordcount-all " *      *      *      *      *   ")))
  (is (= {:chars 57, :line 1, :words 9} (naive-wordcount-all "***START OF THE PROJECT GUTENBERG EBOOK OPPORTUNITIES IN ENGINEERING***")))
  (is (= {:chars 21, :line 1, :words 5} (naive-wordcount-all "Very nice ad-hoc counting.")))
  (is (= {:chars 41, :line 1, :words 7} (naive-wordcount-all "Distributed Proofreading Team (http://www.pgdp.net)"))))



(defn naive-wordcount-frequencies
  "returns an array of word frequencies"
  [s]
  ;; Note: this runs in parallel but does not keep the order of words
  ;; So not sure about the spec here
  ;; It would require to turn it into maps to test the results
  ;; So I use the simple (but efficient) version below
  ;; also commented since requiring tesser gives a warning
  #_(into [] (->>
              (t/frequencies)
              (t/tesser (t/chunk 1024 (or (words s) [])))))
  (into [] (frequencies (words s))))

(deftest naive-wordcount-frequencies-test
  (is (= []  (naive-wordcount-frequencies "")))
  (is (= [["Very" 1] ["nice" 2] ["counting" 1] ["Really" 1] ["very" 1]] (naive-wordcount-frequencies "Very nice counting. Really very nice!")))
  (is (= [["dog" 1] ["fox" 1] ["over" 1] ["The" 1] ["jumps" 1] ["brown" 1] ["quick" 1] ["lazy" 1] ["the" 1]] (naive-wordcount-frequencies "The quick brown fox jumps over the lazy dog.")))
  (is (= [] (naive-wordcount-frequencies " *      *      *      *      *   ")))
  (is (= [["EBOOK" 1] ["OPPORTUNITIES" 1] ["THE" 1] ["GUTENBERG" 1] ["PROJECT" 1] ["ENGINEERING" 1] ["START" 1] ["IN" 1] ["OF" 1]] (naive-wordcount-frequencies "***START OF THE PROJECT GUTENBERG EBOOK OPPORTUNITIES IN ENGINEERING***")))
  (is (= [["Very" 1] ["nice" 1] ["ad" 1] ["hoc" 1] ["counting" 1]] (naive-wordcount-frequencies "Very nice ad-hoc counting.")))
  (is (= [["Distributed" 1] ["Proofreading" 1] ["Team" 1] ["http" 1] ["www" 1] ["pgdp" 1] ["net" 1]] (naive-wordcount-frequencies "Distributed Proofreading Team (http://www.pgdp.net)")))
  )


(defn wordcount
  "count words in a file"
  [filepath & {:keys [parallel frequencies all]
               :or {parallel false
                    frequencies false
                    all false}
               :as opts}]
  (let [file (io/as-file filepath)]

    (cond

      (not (.exists file)) (do (println "The file" (.getPath file) " does not exists")
                               nil)

      parallel
      ;; when --parallel is used, assume a bigger text file (ie. tens of MBs)
      ;; read file as a lazy stream line by line, not holding the head
      ;; and use parallel operations
      (with-open [rdr (clojure.java.io/reader filepath)]
        (let [lseq (line-seq rdr)

              ;; res (->>
              ;;      (map naive-wordcount lseq)
              ;;      (reduce + 0))

              ;; res (->>
              ;;      (r/map naive-wordcount lseq)
              ;;      (r/reduce + 0))

              res (r/fold + (fn [memo word]
                              (+ memo (naive-wordcount word))) lseq)

              ;; res (->>
              ;;      (pmap naive-wordcount lseq)
              ;;      (tesser.simple/reduce + 0))
              ]
          (println res)
          res))

      frequencies
      (let [res (into [] (naive-wordcount-frequencies (slurp file)))]
        (apply println res)
        res)

      all
      (let [{:keys [line words chars] :as res} (naive-wordcount-all (slurp file))]
        (cl-format true "line~p: ~:*~d word~p: ~:*~d char~p: ~:*~d" line words chars)
        res)

      :default
      (let [res (naive-wordcount (slurp file))]
        (println res)
        res))))

(deftest wordcount-test
  (is (= 21046   (wordcount "test/resources/pg24681.txt")))
  (is (= 19088   (wordcount "test/resources/big.txt")))
  (is (= 929462  (wordcount "test/resources/t8.shakespeare.txt")))
  (is (= 5734289 (wordcount "test/resources/loreum.txt"))))


(comment
  ;; better for bigger files
  (time (wordcount "test/resources/pg24681.txt" :parallel true))
  (time (wordcount "test/resources/big.txt"  :parallel true))
  (time (wordcount "test/resources/t8.shakespeare.txt"  :parallel true))
  ;; after this point the non-naive version is faster (on my laptop)
  (time (wordcount "test/resources/loreum.txt"  :parallel true)) ;; 36MB 1.2 sec
  (time (wordcount "test/resources/loreum.txt")) ;; 36MB 3.1 sec
  )

;;
;; This script is a boot CLI task: https://github.com/boot-clj/boot/wiki/Scripts
;; I am aware of tools.cli, however I find boot scripts to be pleasant to write,
;; and simple to use. The only downsides are that the order of different flags are not keps
;; and that since defclifn sits below deftask un-named arguments bound to *args* have to be at the end
;;

(defclifn -main
  [f frequencies  bool      "Enable frequencies"
   p parallel     bool      "Execute in parallel"
   a all          bool      "Output additional metrics"
   i input-file   VAL   str "Path to the file to read"]
  (let [input-file (or input-file (first *args*))]
    (assert (not (nil? input-file)))
    (wordcount input-file :frequencies frequencies :parallel parallel :all all)))

(deftest main-test
  (is (= 576616 (-main "-i" "test/resources/war-and-peace-full-book.txt")))
  (is (= 576616 (-main "-i" "test/resources/war-and-peace-full-book.txt" "--parallel")))
  (is (= {:chars 2533285, :line 65007, :words 576616} (-main "-i" "test/resources/war-and-peace-full-book.txt" "--all")))
  (is (= '(["scholarly" 2] ["refers" 1] ["are" 1] ["publications" 1]) (take 4 (-main "-i" "test/resources/journal.txt" "--frequencies"))))
  )


(comment
  (time (run-tests)) ;; {:test 7, :pass 44, :fail 0, :error 0, :type :summary} in 4.3 sec
  )

(comment

  (reduce ( mapping inc))

  )

;;;;;;;;;;
;; Quiz ;;
;;;;;;;;;;


(defn                ;; macro expanding to (def (fn ...))
  ^{:private true}   ;; annotation to indicate that the function is private to the namespace. Equivalent to defn- This can be circumvented by refering the var directly.
  quiz               ;; name of the variable to which the function is bound
  "Lot of stuff"              ;; Indeed
  {:inline (fn [x] `(str ~x)) ;; indicates a possible optimization. does the same as the one-arity version below, using the macro syntax (syntax quote will resolve str to clojure.core/str and ~ will unquote x) - so this is the same as (str x)
   :inline-arities #{1}       ;; the inline version is only valid for 1-argument calls
   :private true}             ;; redundant with the one above

  (^{:tag String }   ;; type hint indicating the return type for compiler optimization. Could be written ^String
   [[x :as v]]       ;; array destructuring syntax - takes (nth 1) of the only parameter as x and the whole seq to v
   {:pre [(odd? x)]} ;; precondition - test that the first element is odd
   (apply str v)) ;; stringify elements of the array

  (^{:post [(pos? %)] :tag double} ;; post-condition asserting that the result should be positive and type hint for the return type
   [^long a ^double b] ;; 2-arguments with type hint
   (- a b))            ;; - works on all numbers
  {:profile "cpu"}     ;; Additional metadata, can be retrieved with (:profile (meta #'quiz))
  )


 ;;; Additional notes
 ;;; - Type hints can be freely ignored by the compiler and do not change the behavior of a function
 ;;; - Nice quiz !

(comment
  ;; example uses
  (quiz "ab") ;;=> "ab" -> 1-arity called directly: the inline form is used
  (map quiz [[3 4 5 "a" "b" \c ]]) ;;=> ("369") -> quiz is passed as parameter, the first element has to be odd cannot be destructured

  (quiz 5 3)  ;; 5 is odd and bigger than 3 => 2.0
  (quiz 7 8 9)
  )

(alter-var-root (def hello) (fn [_] #'hello))
;; alter-var-root redefines the var passed in parameter - usually the parameter should be used instead of referencing #'hello directly
;; Or a simple `def` should be enough
;; But in this case the var (ie. the var object interned by the previous def) is used
;; Below is the explanation to the best of my abilities:
;; args are evaluated left-to-right so "hello" is defined, and the unbound var is passed as first parameter
;; then "hello" is modified to contain the var to "hello" (itself)
;; Therefore when calling it as a function the resolution never gets to the bottom => StackOverflowError
;; printing is is still fine (it contains a reference to the var hello which could or could not exist at this point)
;; I see it as something analogous to setting a pointer value to a pointer to itself
;;


(comment
  (def a) ;;=> #object[clojure.lang.Var$Unbound 0x521a8296 "Unbound: #'boot.user/a"]
  (type (def abc));;=> clojure.lang.Var -> is of the right type
  (alter-var-root (def abc 3) (fn [_] #'abc)) ;; with a bound var
  (abc) ;;=> java.lang.StackOverflowError:

  (alter-var-root (def b 3) (fn [_] b))
  b ;;=> 3

  (def foo 4)
  (= (resolve (symbol "foo")) (var-get #'foo)) ;;=> false
  (alter-var-root #'foo (fn [_] #'foo))
  (= (resolve (symbol "foo")) (var-get #'foo)) ;;=> true
  )
