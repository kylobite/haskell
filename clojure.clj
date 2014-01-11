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

#{0 "zero" :zero 0x0} ; sets

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

((fn arity2+ [x y & z] [x y z]) 1 2) ; function with 2+ params (needs at least 2)
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
    























