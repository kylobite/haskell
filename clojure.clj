
; clojure.clj

0 ; numbers

"String" ; strings

:keyword ; keywords

[127 0x7F 0177 32r3V 2r01111111]
; dec dex oct 32r bin
;=> [127 127 127 127 127]

1.5 ; floats
3e7 ; exponents via `e`

22/7 ; rationals

\u03bc ; characters

(big red apple) ; lists

[1 2 3 :a :b :c] ; vectors

{1 "one", 2 "two", 3 "three"} ; maps

#{0 "0" :zero} ; sets

() [] {} #{} ; !nil

(+ 1 2 3) ; prefix notation
;=> 6

; why prefix is awesomesauce:
;   (+ 1 2 3) vs (1 + 2 + 3)

(fn mk-set [x y] #{x y}) ; how -not- to make an anonymous function
;=> #<user$eval__1$mk_set__2

((fn [x y] #{x y}) 1 2) ; anonymous function
;=> #{1 2}

((fn
    ([x]    #{x})
    ([x y]  #{x y})) 42) ; function w/ param options (1 or 2)
;=> #{42}

((fn arity2+ [x y & z] [x y z]) 1 2) ; function with 2+ params
;=> [1 2 nil]

((fn arity2+ [x y & z] [x y z]) 1 2 3 4)
;=> [1 2 (3 4)]

(def make-a-set
    (fn
        ([x]    #{x})
        ([x y]  #{x y}))) ; define a function
(make-a-set 1)
;=> #{1}
(make-a-set 1 2)
;=> #{1 2}

(defn make-a-set
    "Documentation strings!!!"
    ([x]    #{x})
    ([x y]  #{x y})) ; syntactical sugar for def + fn

(def make-a-list #(list %1 %&)) ; #() can replace fn
(make-a-list 1 2 3) ; list makes a list
;=> (1 (2 3))

(def x "vars") ; def binds data to symbols, aka a var

(do
  7
  (+ 1 2)
  42) ; blocks; returns last line
;=> 42

(let [r         3
      pi        3.1415
      r-squared (* r r)]
    (println "radius is" r)
    (* pi r-squared)) ; locals and printing

(defn print-down-from [x]
    (when (pos? x)          ; when = 'if' w/ side-effects
        (println x)         ; printing is a side-effect
        (recur (dec x))))   ; recur does recursion

(defn sum-down-from [sum x]
    (if (pos? x)
        (recur (+ sum x) (dec x))
        sum)) ; if for when values are returned
(sum-down-from 0 10)
;=> 55

(defn sum-down-from [initial-x]
    (loop [ sum 0
            x   initial-x]
        (if (pos? x)
            (recur (+ sum x) (dec x))
            sum))) ; recur goes to loop, loop acts like let

(defn absolute-value [x]
    (if (pos? x)
        x       ; then, in tail position
        (- x)))   ; else, (-x) in tail position, x is not
; recur only works in a tail position

(cons 1 [2 4]) ; constructs a list
;=> (1 2 4)

(def a "apple") ; a is now clearly "apple"
(quote a) ; quote prevents evaluation
;=> a

(quote (cons 1 [2 4])) ; we can even quote forms like cons
;=> (cons 1 [2 4])

(cons 1 '(2 4)) ; suddenly, list syntax can work here
;=> (1 2 4)

`(1 2 4) ; syntax-quoting or something
;=> (1 2 4)

`what-does-this-even-do? ; i mean really?
;=> user/what-does-this-even-do?

`(1 2 ~4) ; unquoting
;=> (1 2 4)

(let [x 2]
    `(1 ~x 4)) ; moar unquoting
;=> (1 2 4)

(let [x '(2 4)] `(1 ~x)) ; all together now!
;=> (1 (2 4))

(let [x '(2 3)] `(1 ~@x)) ; unquote splicing
;=> (1 2 4)

java.util.Locale/JAPAN ; access Java
;=> #<Locale ja_JP>

(Math/sqrt 9) ; access Java via namespaces
;=> 3

(new java.util.HashMap {"foo" 0 "bar" 1 "baz" 2}) ; java class instances
;=> #<HashMap {foo=0, bar=1, baz=2}>
(java.util.HashMap. {"foo" 0 "bar" 1 "baz" 2}) ; idiomatic form using (.)
;=> #<HashMap {foo=0, bar=1, baz=2}>

(.x (java.awt.Point. 1 2)) ; java instance vars
;=> 1

(.divide (java.math.BigDecimal. "10") 2M) ; java instance methods
;=> 5

(let [origin (java.awt.Point. 0 0)]
    (set! (.x origin) 2)    ; set! modifies value
    (str origin))           ; str converts to string
;=> "java.awt.Point[x=2,y=0"

; new java.util.Date().toString().endsWith("2013"); Java version
(.endsWith (.toString (java.util.Date.)) "2014")  ; Clojure version
(.. (java.util.Date.) toString (endsWith "2014")) ; Readable version
;=> true

; java.util.HashMap h = new java.util.HashMap();
; h.puts("HOME", "/home/me");
; h.puts("SRC", "src");
; h.puts("BIN", "classes")      ; Java Version
(doto (java.util.HashMap.)
    (.puts "HOME"   "/home/me") ; Java repetative referencing
    (.puts "SRC"    "src")
    (.puts "BIN"    "classes")) ; Clojure Version
;=> #<HashMap {HOME=/home/me, SRC=src, BIN=classes}>

(throw (Exception. "I was thrown")) ; Exceptions!
;=> java.lang.Exception: I was thrown

(defn throw-catch [f]
    [(try
        (f)
        (catch ArithmeticException e "You divided by zero")
        (catch Exception e (str "Look what you did: "
            (.getMessage e)))
        (finally (println "loading...")))])
; Try/Catch Exceptions
(throw-catch #(/ 4 2))
; loading...
;=> [2]
;   (throw-catch #(/ 1 0))
;   loading...
;   => ["You divided by zero"]
;   (throw-catch #(throw (Exception. "yes")))
;   loading...
;   => ["Look what you did: yes"]

(ns kylo.report) ; namespacing
; REPL: kylo.bite=>
(defn report-ns [] (str "We are in namespace: " *ns*)) ; *ns* var
(report-ns)
;=> "We are in namespace: kylo.bite"

(ns user) ; back to default
; (report-ns) would crash here because it is not in 'user'

(ns kylo.req
    (:require clojure.set)) ; loading namespaces
(clojure.set/intersection #{0 1} #{1 0})
;=> #{1}

(ns kylo.req-alias
    (:require [clojure.set :as set])) ; alias namespaces
(set/intersection #{0 1} #{1 0})
;=> #{1}





















