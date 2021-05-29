#lang pl


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Part A - Implementing the SOL Language ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ---------------------------------------------------- Section 1 - SOL BNF

#|

<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <SOL> <SOL> }
        |  { union <SOL> <SOL> } 
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
       
<NumList> :: =  λ | SET ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; ---------------------------------------------------- The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  [Set  SET]
  [Smult Number SOL]
  [Inter SOL SOL]
  [Union SOL SOL]
  [IdS    Symbol]
  [WithS  Symbol SOL SOL])



;; ---------------------------------------------------- Set Operations

;; ---------------------------------------------------- ismember?
(: ismember? : Number SET  -> Boolean)
;; ismember? function checks if the given number is included in the set.
;; The function checks if there is empty set- if true, return false;
;; else- there is not empty set, and go all over the set to check if there is the desired number.
;; The function accepts a number and a set of numbers as input.
;; The returned value is a boolean- true\false.
;; I have not encountered any difficulties to solve that question. 
;; I solved it after half an hour.
(define (ismember? n l)
  (match l
    ['() #f]
    [(cons first rest)
     (cond
       [(= first n) #t]
       [else (ismember? n rest)])]))

;; ---------------------------------------------------- Tests- ismember?
(test (ismember? 1 '(3 4 5)) => #f)
(test (ismember? 1 '()) => #f)
(test (ismember? 1 '(1)) => #t)
(test (ismember? 12 '(1 2)) => #f)
(test (ismember? 4 '(1 2 3 5 6)) => #f)
(test (ismember? 0 '(0)) => #t)
(test (ismember? 1 '(11)) => #f)


;; ---------------------------------------------------- remove-duplicates
(: remove-duplicates : SET  -> SET)
;; remove-duplicates function removes duplicate numbers included in the set.
;; The main function passes an empty list and the non-unique set.
;; The helper function checks if there is an empty set- if true, and the "new list" empty too- return empty.
;; else- if the set is empty, and the new list is not empty, return the new list.
;; else- there is not empty set, and go all over the set to check if there is a duplicate number.
;; if true- remove them, else- append them to the new list.
;; at the end- return the new list..
;; The function accepts a set of numbers as input.
;; The returned value is a set without duplicates numbers.
;; The main difficult to solve that question is the recursive call. 
;; It took me 2 hours to complete this function.
(define (remove-duplicates l)
  (: remove-helper : SET SET -> SET)
  ;; The helper function, description added above- at the main function 
  (define (remove-helper new old)
    (cond
      [(= (length old) 0)
       (cond
         [(= (length new) 0) old]
         [else new])]
      [else (match old
              [(cons first rest)
               (cond
                 [(ismember? first rest) (remove-helper new rest)]
                 [else (remove-helper (append new (list first)) rest)])])]))
  (remove-helper (list) l))


;; ---------------------------------------------------- Tests- remove-duplicates
(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
(test (remove-duplicates '(1)) => '(1))
(test (remove-duplicates '()) => '())
(test (remove-duplicates '(1 2 2 2)) => '(1 2))
(test (remove-duplicates '(2 -2 2 -2 2 -2 2)) => '(-2 2))
(test (remove-duplicates '(-11)) => '(-11))
(test (remove-duplicates '(1 2 3 2 1 2 3 2 1 2 3 2 1 2 3)) => '(1 2 3))
(test (remove-duplicates '(1 11 111 1111 11111)) => '(1 11 111 1111 11111))
(test (remove-duplicates '(0)) => '(0))
(test (remove-duplicates '(00)) => '(00))
(test (remove-duplicates '(1 7 -8 6 4 8 2 7)) => '(1 -8 6 4 8 2 7))
(test (remove-duplicates '(1 4 5 3 3)) => '(1 4 5 3))


;; ---------------------------------------------------- create-sorted-set
(: create-sorted-set : SET -> SET)
;; create-sorted-set function sort a set of numbers.
;; The function uses the "remove-duplicats" function from above, and the "sort" function from the pl lang.
;; The function accepts a set of numbers as input, and makes the appropriate manipulations.
;; The returned value is a sorted set.
;; There is not some difficulties to solve that question. 
;; It took me 20 mins to complete this function.
(define (create-sorted-set l)
  (remove-duplicates(sort l <)))

;; ---------------------------------------------------- Tests- create-sorted-set
(test (create-sorted-set '(3 4 5)) => '(3 4 5))
(test (create-sorted-set '( 3 2 3 5 6)) => '(2 3 5 6))
(test (create-sorted-set '()) => '())
(test (create-sorted-set '( 3 2 3 3 3 3 3 5 6)) => '(2 3 5 6))
(test (create-sorted-set '(1)) => '(1))
(test (create-sorted-set '( 3 2 1)) => '(1 2 3))
(test (create-sorted-set '()) => '())
(test (create-sorted-set '( 3 2 3 2 1 1)) => '(1 2 3))
(test (create-sorted-set '(1 -1 1 1 1 1 -1 11 2 2 2 2 22 3 3 33 -333 333)) => '(-333 -1 1 2 3 11 22 33 333))


;; ---------------------------------------------------- set-union
(: set-union : SET SET -> SET)
;; set-union function returns a set of numbers that include all the members in 2 sets.
;; The function uses the "create-sorted-set" function from above, and the "append" function from the pl lang.
;; The function accepts 2 sets of numbers as input, and makes the appropriate manipulations to union them to 1 set.
;; The returned value is a sorted set that include all the sets members.
;; There is not some difficulties to solve that question. 
;; It took me 30 mins to complete this function.
(define (set-union A B)
  (create-sorted-set(append A B)))



;; ---------------------------------------------------- Tests- set-union
(test (set-union '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-union '(3 4 5) '()) => '(3 4 5))
(test (set-union '(3 4 5) '(1)) => '(1 3 4 5))
(test (set-union '(3 4 5) '(3 4 5 6)) => '(3 4 5 6))
(test (set-union '(3 4 5) '(1)) => '(1 3 4 5))
(test (set-union '(3 4 -5) '(11)) => '(-5 3 4 11))
(test (set-union '() '()) => '())
(test (set-union '(3) '(2)) => '(2 3))
(test (set-union '(3 4 5) '(0 1)) => '(0 1 3 4 5))
(test (set-union '(-3 -4 -5) '(1 0)) => '(-5 -4 -3 0 1))


;; ---------------------------------------------------- set-intersection
(: set-intersection : SET SET -> SET)
;; set-intersection function returns a set of numbers that include all the common members in 2 sets.
;; The function uses the "ismember?" function from above, and the "sort" and "filter" functions from the pl lang.
;; The function accepts 2 sets of numbers as input, and makes the appropriate manipulations to intersect them to 1 set.
;; The returned value is a sorted set that include all the appropriate members.
;; There is not some difficulties to solve that question, all i had to do is understand how to work with "filter" function.
;; It took me 40 mins to complete this function.
(define (set-intersection A B)
  (: mem-filter : Number -> Boolean)
  (define (mem-filter n)
    (ismember? n A))
  (sort (filter mem-filter B) <))

;; ---------------------------------------------------- Tests- set-intersection
(test (set-intersection '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-intersection '(3 -4 5) '(3)) => '(3))
(test (set-intersection '(3 4 -5) '(1)) => '())
(test (set-intersection '(3 4 5) '(3 4)) => '(3 4))
(test (set-intersection '() '()) => '())
(test (set-intersection '(1 2 3 4 5 6 8) '(2 4 6 8 10)) => '(2 4 6 8))
(test (set-intersection '(1 2 3 4 5 6 7 8 9 0) '()) => '())
(test (set-intersection '(1 3 4 5 11) '(1 11)) => '(1 11))
(test (set-intersection '(3 -4 5) '(-4 5)) => '(-4 5))
(test (set-intersection '(3 4 5) '(2)) => '())
(test (set-intersection '(-3 -4 -5) '(-1)) => '())


;; ---------------------------------------------------- set-smult
(: set-smult : Number (Listof Number) -> SET)
;; set-smult function returns a set of numbers that are multiply by scalar number.
;; The function uses the "create-mul-n" function defined below (that uses "mul-n" too), and the "sort" and "map" functions from the pl lang.
;; The "create-mul-n" and "mul-n" functions are a help-functions that i maked. 
;; The function accepts a number (scalar) and set of numbers as input, and makes the appropriate manipulations to multiplt them.
;; The returned value is a sorted set that include all the appropriate members after the multiply action.
;; There is not some difficulties to solve that question, all i had to do is understand how to work with "filter" function.
;; It took me 2 hours to complete this function.
(define (set-smult n l)
  (sort (map (create-mul-n n) l) <))


;; ---------------------------------------------------- helper functions for "set-smult"
(: create-mul-n : Number -> (Number -> Number))
;; create-mul-n function accepts a number "n" as input and returns a function that multiply a number by "n".
;; The function retuens the "mul-n" function.
;; The "create- mul-n" function is a help-function that i maked for "set-smult"
;; There is not some difficulties to solve that question.
;; The time to solve that function is include in "set-smult" time.
(define (create-mul-n n)
  (: mul-n : Number -> Number)
  ;; mul-n function accepts a number "n" as input and returns a number that multiply a number by "n".
  ;; The function uses the multiply (*) function from the pl lang.
  ;; The "mul-n" function is a help-function that i maked for "set-smult" 
  ;; There is not some difficulties to solve that question.
  ;; The time to solve that function is include in "set-smult" time.
  (define (mul-n lmember)
    (* n lmember))
  mul-n)

;; ---------------------------------------------------- Tests- set-smult
(test (set-smult 3 '(3 4 5)) => '(9 12 15))
(test (set-smult 2 '()) => '())
(test (set-smult 0 '(3 4 5)) => '(0 0 0))
(test (set-smult 2 '()) => '())
(test (set-smult 00 '(3 4 5)) => '(0 0 0))
(test (set-smult 2 '(1)) => '(2))
(test (set-smult 1 '(3 4 5)) => '(3 4 5))
(test (set-smult 2 '(22)) => '(44))
(test (set-smult 5 '(3 4 5)) => '(15 20 25))
(test (set-smult -1 '()) => '())
(test (set-smult -1 '(3 4 5)) => '(-5 -4 -3))



;; ---------------------------------------------------- Parser

;; ---------------------------------------------------- parse-sexprS
(: parse-sexprS : Sexpr -> SOL)
;; parse-sexprS function convert s-expressions into SOLs.
;; input: S-expression
;; output: SOL
;; operates: That function convert sexpr type to SOL type, to "help" the other function- parseS.
;; The function checks by "match" function the input, and works by the logic of the SOL.
;; If there is not a good syntax- return Error.
;; My function will work with Sexpr only.
;; There is not some difficulties to solve that question
;; I solved it after a 2 hour.
(define (parse-sexprS sexpr)
  (match sexpr
    [(list (number: ns) ...) (Set ns)]
    [(symbol: name) (IdS name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (WithS name (parse-sexprS named) (parse-sexprS body))]
       [else (error 'parse-sexprS "bad `with' syntax in ~s" sexpr)])]
    [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexprS rhs))]
    [(list 'intersect lhs rhs) (Inter (parse-sexprS lhs) (parse-sexprS rhs))]
    [(list 'union lhs rhs) (Union (parse-sexprS lhs) (parse-sexprS rhs))]
    [else (error 'parse-sexprS "bad syntax in ~s" sexpr)]))


;; ---------------------------------------------------- Tests- parse-sexprS
(test (parse-sexprS '(1 2)) => (Set '(1 2)))
(test (parse-sexprS '()) => (Set '()))
(test (parse-sexprS '(1 -1 0)) => (Set '(1 -1 0)))
(test (parse-sexprS '(1 1 1)) => (Set '(1 1 1)))
(test (parse-sexprS '(1 11 111)) => (Set '(1 11 111)))
(test (parse-sexprS '(-1 -2)) => (Set '(-1 -2)))
(test (parse-sexprS '(union (1 2 3) (1 2 7))) => (Union (Set '(1 2 3)) (Set '(1 2 7))))
(test (parse-sexprS '(union (1 3) (1 2 3))) => (Union (Set '(1 3)) (Set '(1 2 3))))
(test (parse-sexprS '(union (1 2 3) (1 2 3))) => (Union (Set '(1 2 3)) (Set '(1 2 3))))
(test (parse-sexprS '(union (1 2 3) (0 7))) => (Union (Set '(1 2 3)) (Set '(0 7))))
(test (parse-sexprS '(union (-1) (1 -2 7))) => (Union (Set '(-1)) (Set '(1 -2 7))))
(test (parse-sexprS '(union (000) (0))) => (Union (Set '(000)) (Set '(0))))
(test (parse-sexprS '(intersect (00 0) (0))) => (Inter (Set '(00 0)) (Set '(0))))
(test (parse-sexprS '(intersect (1 2 3 4 5 6 7 8 9 0) (0))) => (Inter (Set '(1 2 3 4 5 6 7 8 9 0)) (Set '(0))))
(test (parse-sexprS '(intersect (0 00 000 1 2 3 4) (1 2 3 4))) => (Inter (Set '(0 00 000 1 2 3 4)) (Set '(1 2 3 4))))
(test (parse-sexprS '(intersect (333 33 3 3333333) (333 3333333333333))) => (Inter (Set '(333 33 3 3333333)) (Set '(333 3333333333333))))
(test (parse-sexprS '(intersect (0 0 0 12 13 0 0 14 0 0) (12 13 14 12 13 14 15 15 1 5 15 ))) => (Inter (Set '(0 0 0 12 13 0 0 14 0 0)) (Set '(12 13 14 12 13 14 15 15 1 5 15 ))))
(test (parse-sexprS '(intersect (1 2 3 ) (1 2 3))) => (Inter (Set '(1 2 3)) (Set '(1 2 3))))
(test (parse-sexprS '(intersect (1 2 3) ())) => (Inter (Set '(1 2 3)) (Set '())))
(test (parse-sexprS 'A) => (IdS 'A))
(test (parse-sexprS 'B) => (IdS 'B))
(test (parse-sexprS '(with (S (1 2 3)) (union S S))) => (WithS 'S (Set '(1 2 3)) (Union (IdS 'S) (IdS 'S))))
(test (parse-sexprS '(with (S (intersect (1 2 3) (4 2 7))) (union S S))) => (WithS 'S (Inter (Set '(1 2 3)) (Set '(4 2 7))) (Union (IdS 'S) (IdS 'S))))
(test (parse-sexprS '(with (S (intersect (1 2 3) (union (1 2 3) (4 2 7)))) (union S S))) => (WithS 'S (Inter (Set '(1 2 3)) (Union (Set '(1 2 3)) (Set '(4 2 7)))) (Union (IdS 'S) (IdS 'S))))
(test (parse-sexprS '(with (S (intersect (1 2 3) (1 2 3))) (union S S))) => (WithS 'S (Inter (Set '(1 2 3)) (Set '(1 2 3))) (Union (IdS 'S) (IdS 'S))))
(test (parse-sexprS '(with (A (intersect (1 2 3) (scalar-mult 1 (1 2 3)))) (union A A))) => (WithS 'A (Inter (Set '(1 2 3)) (Smult 1 (Set '(1 2 3)))) (Union (IdS 'A) (IdS 'A))))
(test (parse-sexprS '(with (A (intersect (1 2 3) (scalar-mult 0 (1 2 3)))) (union A A))) => (WithS 'A (Inter (Set '(1 2 3)) (Smult 0 (Set '(1 2 3)))) (Union (IdS 'A) (IdS 'A))))
(test (parse-sexprS '(with (A (intersect (1 2 3) (scalar-mult -1 (1 2 3)))) (union A A))) => (WithS 'A (Inter (Set '(1 2 3)) (Smult -1 (Set '(1 2 3)))) (Union (IdS 'A) (IdS 'A))))
(test (parse-sexprS '(scalar-mult 1 (1 2 3))) => (Smult 1 (Set '(1 2 3))))
(test (parse-sexprS '(scalar-mult 0 (1 2 3))) => (Smult 0 (Set '(1 2 3))))
(test (parse-sexprS '(scalar-mult -1 (1 2 3))) => (Smult -1 (Set '(1 2 3))))
(test (parse-sexprS '(scalar-mult 10 (1 2 3))) => (Smult 10 (Set '(1 2 3))))
(test (parse-sexprS '(scalar-mult 1 ())) => (Smult 1 (Set '())))
(test (parse-sexprS '(scalar-mult 0 (1))) => (Smult 0 (Set '(1))))
(test (parse-sexprS '(scalar-mult 11 (1 2 3))) => (Smult 11 (Set '(1 2 3))))
(test (parse-sexprS '(scalar-mult 100 (1 2 3))) => (Smult 100 (Set '(1 2 3))))
(test (parse-sexprS '(scalar-mult 1 (-1))) => (Smult 1 (Set '(-1))))

(test (parse-sexprS '(with (S union (1) (2)) (union S S)))
      =error> "parse-sexprS: bad `with' syntax in (with (S union (1) (2)) (union S S))")

(test (parse-sexprS '(with (S union (1 2 3) (2 2 22)) (union S S)))
      =error> "parse-sexprS: bad `with' syntax in (with (S union (1 2 3) (2 2 22)) (union S S))")

(test (parse-sexprS '(with (S union (-1) (20)) (inter S S)))
      =error> "parse-sexprS: bad `with' syntax in (with (S union (-1) (20)) (inter S S))")

(test (parse-sexprS '(with (A union (111) (-2 0)) (union S S)))
      =error> "parse-sexprS: bad `with' syntax in (with (A union (111) (-2 0)) (union S S))")


(test (parse-sexprS '1)
      =error> "parse-sexprS: bad syntax in 1")

(test (parse-sexprS '123)
      =error> "parse-sexprS: bad syntax in 123")

(test (parse-sexprS '-1)
      =error> "parse-sexprS: bad syntax in -1")

(test (parse-sexprS 4)
      =error> "parse-sexprS: bad syntax in 4")

(test (parse-sexprS '(a b c))
      =error> "parse-sexprS: bad syntax in (a b c)")

(test (parse-sexprS '(a 1 4 b c))
      =error> "parse-sexprS: bad syntax in (a 1 4 b c)")

(test (parse-sexprS '(want))
      =error> "parse-sexprS: bad syntax in (want)")


;; ---------------------------------------------------- parseS
(: parseS : String -> SOL)
;; parses a string containing a SOL expression to a SOL AST
;; input: String that represents SOL
;; output: SOL
;; operates: That function convert String type that represents a SOL to SOL type.
;; My function will work with correct String only.
;; The function get the String, make it to be a sexpr by "string->sexpr" function, and check the sexpr:
;; by match function the check the pattern and by cond function to check the validation.
;; if the pattern not good- Error,
;; if the validation not good- another Error.
;; There is not some difficulties to solve that question
;; The solved time is included in ths "parse-SexprS" time. 2 hours for all..
(define (parseS str)
  (parse-sexprS (string->sexpr str)))


;; ---------------------------------------------------- Tests- parseS
(test (parseS "{1 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 3 4 1 4 4 2 3 4 1 2 3)))
(test (parseS "{1 3 4 4 2 3 4 1 2 3}") => (Set '(1 3 4 4 2 3 4 1 2 3)))
(test (parseS "{1 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 3 4 1 4 4 2 3 4 1 2 3)))
(test (parseS "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{intersect {union {1 2 3} {4 2 3}} {1 2 7}}") => (Inter (Union (Set '(1 2 3)) (Set '(4 2 3))) (Set '(1 2 7))))
(test (parseS "{intersect {1 2 3} {union {1 2 3} {4 2 3}}}") => (Inter (Set '(1 2 3)) (Union (Set '(1 2 3)) (Set '(4 2 3)))))
(test (parseS "{intersect {1 2 3} {}}") => (Inter (Set '(1 2 3)) (Set '())))
(test (parseS "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{union {0} {4 2 3}}") => (Union (Set '(0)) (Set '(4 2 3))))
(test (parseS "{union {1 3 0 3} {union {1}{2}}}") => (Union (Set '(1 3 0 3)) (Union (Set '(1)) (Set '(2)))))
(test (parseS "{scalar-mult 1 {1 2 3}}") => (Smult 1 (Set '(1 2 3))))
(test (parseS "{scalar-mult 0 {1 3 5 7}}") => (Smult 0 (Set '(1 3 5 7))))
(test (parseS "{scalar-mult 10 {1 1 1}}") => (Smult 10 (Set '(1 1 1))))
(test (parseS "{scalar-mult 11 {1}}") => (Smult 11 (Set '(1))))
(test (parseS "{scalar-mult 5 {1 2 3 1 2 3 1 2 3}}") => (Smult 5 (Set '(1 2 3 1 2 3 1 2 3))))
(test (parseS "{scalar-mult 2 {-3 3 -1}}") => (Smult 2 (Set '(-3 3 -1))))

(test (parseS "{with S {intersect {1 2 3} {4 2 3}}
                 {union S S}}")
      =error> "parse-sexprS: bad `with' syntax in")

(test (parseS "{with S {union {2 3} {4 2 3}}
                 {union S S}}")
      =error> "parse-sexprS: bad `with' syntax in")

(test (parseS "{with S {intersect {} {4 2 3}}
                 {intersect S S}}")
      =error> "parse-sexprS: bad `with' syntax in")

(test (parseS "{with S {intersect {1 2} {0}}
                 {intersect S S}}")
      =error> "parse-sexprS: bad `with' syntax in")

(test (parseS "{with S {intersect {1 2 3} {4 2 3}}
                 {union S S}}")
      =error> "parse-sexprS: bad `with' syntax in")

(test (parseS "{with S {intersect {1 2 3} {0 0 0 4 2 3}}
                 {intersect S S}}")
      =error> "parse-sexprS: bad `with' syntax in")



;; ---------------------------------------------------- Substitution

#|

 Formal specs for `subst':
   (`Set' is a <NumList>, E, E1, E2 are <SOL>s, `x' is some <id>,`y' is a *different* <id>)

      Set[v/x]              = Set
      {smult n E}[v/x]      = {smult n E[v/x]}
      {inter E1 E2}[v/x]    = {inter E1[v/x] E2[v/x]}
      {union E1 E2}[v/x]    = {union E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#


;; ---------------------------------------------------- substS
(: substS : SOL Symbol SOL -> SOL)
;; That function makes the convertion from an Id to the Id's value (only if its identified)
;; input: SOL, Symbol and SOL
;; output: SOL
;; operates: That function convert the identified Id to his value.
;; Substitutes the Symbol, by the second SOL -> inside to the first SOL.
;; My function will work with correct input only.
;; The function get the input and makes check by "cases" function-
;; to check the pattern and the validation of the variant.
;; The return value has no free instances of the Symbol. 
;; The difficulty in this question-
;; "WithS" variant was a little hard..
;; It tooks me 2 hours to solve this question.
(define (substS expr from to)
  (cases expr
    [(Set n) expr]
    [(Smult n s) (Smult n (substS s from to))]
    [(Inter l r) (Inter (substS l from to) (substS r from to))]
    [(Union l r) (Union (substS l from to) (substS r from to))]
    [(IdS name) (if (eq? name from) to expr)]
    [(WithS bound-id named-expr bound-body) (WithS bound-id
                                                   (substS named-expr from to)
                                                   (if (eq? from bound-id) bound-body
                                                       (substS bound-body from to)))]))

;; ---------------------------------------------------- Tests- substS
(test (substS (Set '(-3 3 1)) 'a (Set '(-3 3 1))) => (Set '(-3 3 1)))
(test (substS (Set '(1 2 3)) 'b (Set '(-3 3 1))) => (Set '(1 2 3)))
(test (substS (Set '(0 -1 1)) 'S (Set '(-3 3 1))) => (Set '(0 -1 1)))
(test (substS (Set '(-1 -2 -3)) 'a (Set '(-3 3 1))) => (Set '(-1 -2 -3)))
(test (substS (Set '(3 3 33)) 'a (Set '(-3 3 1))) => (Set '(3 3 33)))
(test (substS (Smult 5 (Set '(3 3 33))) 'a (Set '(1))) => (Smult 5 (Set '(3 3 33))))
(test (substS (Smult 0 (Set '(3 15))) 'a (Set '(3 1))) => (Smult 0 (Set '(3 15))))
(test (substS (Smult -0 (Set '(3))) 'b (Set '(-27 3 1))) => (Smult -0 (Set '(3))))
(test (substS (Smult 1 (Set '())) 's (Set '(-2 3 1))) =>  (Smult 1 (Set '())))
(test (substS (Smult -1 (Set '(15 18 9))) 'R (Set '(32 2 2 1))) => (Smult -1 (Set '(15 18 9))))
(test (substS (Smult 100 (Set '(101 5 4))) 'W (Set '(100 1 64 1))) =>  (Smult 100 (Set '(101 5 4))))
(test (substS (Smult 17 (Set '(1 1 2 3 4 5 7))) 'a (Set '(78 98 1))) => (Smult 17 (Set '(1 1 2 3 4 5 7))))
(test (substS (Inter (Set '(13 17)) (Set '(3 1 13 17 4 6))) 'b (Set '(-27 3 1))) => (Inter (Set '(13 17)) (Set '(3 1 13 17 4 6))))
(test (substS (Inter (Set '(197)) (Set '(3 4 6 1 9 7 197))) 'x (Set '(-27 3 1))) => (Inter (Set '(197)) (Set '(3 4 6 1 9 7 197))))
(test (substS (Inter (Set '(31)) (Set '(6))) 'd (Set '(-27 3 1))) => (Inter (Set '(31)) (Set '(6))))
(test (substS (Inter (Set '(100)) (Set '(10 100 1000))) 'D (Set '(-27 3 1))) => (Inter (Set '(100)) (Set '(10 100 1000))))
(test (substS (Inter (Set '(3)) (Set '(3))) 'Qw (Set '(-2))) => (Inter (Set '(3)) (Set '(3))))
(test (substS (Inter (Set '(1)) (Set '())) 'QQQ (Set '(3 1))) => (Inter (Set '(1)) (Set '())))
(test (substS (Union (Set '(3)) (Set '(3))) 'Qw (Set '(-2))) => (Union (Set '(3)) (Set '(3))))
(test (substS (Union (Set '(31)) (Set '(6))) 'd (Set '(-27 3 1))) => (Union (Set '(31)) (Set '(6))))
(test (substS (Union (Set '(100)) (Set '(10 100 1000))) 'D (Set '(-27 3 1))) => (Union (Set '(100)) (Set '(10 100 1000))))
(test (substS (Union (Set '(3)) (Set '(3))) 'Qw (Set '(-2))) => (Union (Set '(3)) (Set '(3))))
(test (substS (Union (Set '(1)) (Set '())) 'QQQ (Set '(3 1))) => (Union (Set '(1)) (Set '())))
(test (substS (IdS 'x) 'x (Set '(-2))) => (Set '(-2)))
(test (substS (IdS 'e) 't (Set '(-2))) => (IdS 'e))
(test (substS (IdS 'x) 'y (Set '(-2))) => (IdS 'x))
(test (substS (IdS 'X) 'X (Set '(-10 8 7))) => (Set '(-10 8 7)))
(test (substS (IdS 'z) 'x (Set '(-2))) => (IdS 'z))
(test (substS (IdS 'X) 'X (Set '(-10 -88 -4 -19 -3))) => (Set '(-10 -88 -4 -19 -3)))
(test (substS (WithS 'y (Set '(1 2)) (Smult 2 (IdS 'x))) 'x (Set '(2 2))) => (WithS 'y (Set '(1 2)) (Smult 2 (Set '(2 2)))))
(test (substS (WithS 'y (Set '(1 2)) (Union (Set '(1 2)) (IdS 'x))) 'x (Set '(11))) => (WithS 'y (Set '(1 2)) (Union (Set '(1 2)) (Set '(11)))))
(test (substS (WithS 'x (Set '(1 2)) (IdS 'y)) 'y (Set '(0))) => (WithS 'x (Set '(1 2)) (Set '(0))))
(test (substS (WithS 'z (Set '(1 2)) (IdS 'x)) 'x (Set '())) => (WithS 'z (Set '(1 2)) (Set '())))
(test (substS (WithS 'x (Set '()) (IdS 'y)) 'y (Set '())) => (WithS 'x (Set '()) (Set '())))
(test (substS (WithS 'x (Set '()) (IdS 'y)) 'x (Set '())) => (WithS 'x (Set '()) (IdS 'y)))



;; ---------------------------------------------------- Evaluation 
#|

Evaluation rules:
    ;; Please complete the missing parts in the formal specifications below

    eval({ N1 N2 ... Nl })  =  (sort (create-set (N1 N2 ... Nl)))
                               where create-set removes all duplications from
                              the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E}) =  (K*N1 K*N2 ... K*Nl) if (N1 N2 ... Nl) = eval(E,) is a sorted set AND
                                = error! otherwise (if S is not a sorted set)
    eval({intersect E1 E2}) = (sort (create-set (set-intersection (eval(E1,) , eval(E2,))))
                                    if both E1 and E2 evaluate to sorted sets
                                = error! otherwise
    eval({union E1 E2}) = (sort (create-set (eval(E1,) , eval(E2,))))
                                  if both E1 and E2 evaluate to sorted sets
                             = error! otherwise
    eval({with {x E1} E2}) = eval(E2,extend(x,eval(E1,),))

|#


;;---------  the eval procedure ------------------------------
;; ---------------------------------------------------- eval
(: eval : SOL -> SET)
;; evaluates SOL expressions by reducing them to set values
;; That function makes the convertion from an SOL to the set values
;; input: SOL
;; output: SET
;; operates: That function evaluated the SOL to his value.
;; My function will work with correct input only.
;; The function get the input and makes check by "cases" function-
;; to check the pattern and the validation of the variant.
;; There is not some difficulties in this question.
;; It tooks me half an hour to solve that question..
(define (eval expr)
  (cases expr
    [(Set S) (create-sorted-set S)]  ;; sort and remove-duplicates
    [(Smult n set) (set-smult n (eval set))]
    [(Inter l r) (set-intersection (eval l) (eval r))]
    [(Union l r) (set-union (eval l) (eval r))]
    [(WithS name named body) (eval (substS body name (Set (eval named))))]
    [(IdS name) (error 'eval "free identifier: ~s" name)]))

;; ---------------------------------------------------- Tests- eval
(test (eval (Set '(-1 2 3))) => '(-1 2 3))
(test (eval (Set '())) => '())
(test (eval (Set '(1 2 3))) => '(1 2 3))
(test (eval (Set '(-1 -2 -3))) => '(-3 -2 -1))
(test (eval (Set '(0 -0 0))) => '(0))
(test (eval (Smult 5 (Set '(-1 2 3)))) => '(-5 10 15))
(test (eval (Smult 0 (Set '(-1 2 3)))) => '(0 0 0))
(test (eval (Smult -1 (Set '(-1 2 3)))) => '(-3 -2 1))
(test (eval (Smult 10 (Set '(-1 2 3)))) => '(-10 20 30))
(test (eval (Smult 1 (Set '(-1 2 3)))) => '(-1 2 3))
(test (eval (Inter (Set '(-1 2 3)) (Set '(-1 2 3)))) => '(-1 2 3))
(test (eval (Inter (Set '(-1 2 3)) (Set '()))) => '())
(test (eval (Inter (Set '(-1 2 3)) (Set '(0)))) => '())
(test (eval (Inter (Set '(-1 2 3)) (Set '(-1)))) => '(-1))
(test (eval (Inter (Set '(3 33 333)) (Set '(33 3)))) => '(3 33))
(test (eval (Inter (Set '(-1 -2 -3)) (Set '(-1 2 -3)))) => '(-3 -1))
(test (eval (Union (Set '(3 33 333)) (Set '(33 3)))) => '(3 33 333))
(test (eval (Union (Set '(3 33 333)) (Set '(0 00 000)))) => '(0 3 33 333))
(test (eval (Union (Set '(7)) (Set '(12 3)))) => '(3 7 12))
(test (eval (Union (Set '(0)) (Set '(15 16)))) => '(0 15 16))
(test (eval (Union (Set '(12)) (Set '(33 3)))) => '(3 12 33))
(test (eval (Union (Set '(4 8 0)) (Set '(33 3)))) => '(0 3 4 8 33))
(test (eval (WithS 'x (Union (Set '(3 1)) (Set '(3 2))) (Union (IdS 'x) (IdS 'x)))) => '(1 2 3))
(test (eval (WithS 'x (Inter (Set '(3 1)) (Set '(3 2))) (Union (IdS 'x) (IdS 'x)))) => '(3))
(test (eval (WithS 'y (Smult 5 (Set '(3 2))) (Union (IdS 'y) (IdS 'y)))) => '(10 15))
(test (eval (WithS 'x (Union (Set '(3 1)) (Set '(3 2))) (WithS 'y (Smult 5 (Set '(3 2))) (Union (IdS 'y) (IdS 'x))))) => '(1 2 3 10 15))
(test (eval (WithS 'x (Union (Set '()) (Set '(3 2))) (Union (IdS 'x) (IdS 'x)))) => '(2 3))
(test (eval (WithS 'x (Union (Set '()) (Set '())) (Union (IdS 'x) (IdS 'x)))) => '())
(test (eval (IdS 'a)) =error> "eval: free identifier: a")
(test (eval (IdS 'b)) =error> "eval: free identifier: b")
(test (eval (IdS 'S)) =error> "eval: free identifier: S")
(test (eval (IdS 'ar)) =error> "eval: free identifier: ar")


;; ---------------------------------------------------- run
(: run : String -> SET)
;; evaluate a SOL program contained in a string
;; This function get as input a String, and returns SET.
;; This function call to "eval" function on the "parseS" string.
;; this is the "run" because its run all the program.
(define (run str)
  (eval (parseS str)))

;; ---------------------------------------------------- Tests- run    
(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{intersect {1 2 3} {4 2 3}}") => '(2 3))
(test (run "{union {0} {4 3}}") => '(0 3 4))
(test (run "{1 2 3 4 2 3 100 -8 5000}") => '(-8 1 2 3 4 100 5000))
(test (run "{1 0 1 0 0 1 0 1 0 1}") => '(0 1))
(test (run "{1 2 1 2 1 2 3}") => '(1 2 3))
(test (run "{scalar-mult 1 {4 2 3}}") => '(2 3 4))
(test (run "{scalar-mult 0 {4 2 3}}") => '(0 0 0))
(test (run "{scalar-mult -1 {4 2 3}}") => '(-4 -3 -2))
(test (run "{scalar-mult -0 {4 2 3}}") => '(0 0 0))

(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {with {x {4 5 7 6 9 8 8 8}}
                    {union x S}}}")
      => '(2 3 4 5 6 7 8 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {with {x {0}}
                    {union x S}}}")
      => '(0 2 3))

(test (run "{with {S {intersect {1 2 3 4} {4 2 3}}}
                 {with {x {4 5 7 6 9 8 8 8}}
                    {intersect x S}}}")
      => '(4))

(test (run "{with {S {union {1 2 3} {4 2 3}}}
                 {with {x {4 5 7 6 9 8 8 8}}
                    {union x S}}}")
      => '(1 2 3 4 5 6 7 8 9))

(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
              {union {scalar-mult 3 B}
                 {4 5 7 9 8 8 8}}}")
      =error> "eval: free identifier:")



;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Part B - Detecting free instances in a WAE program ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#|

     # The WAE functions has been taken from the class presentation, i explained the functions but didn't make tests.
       The functions "isSymbolMember" & "remove-duplicates-symbols" is like the functions from Part A above,
       with some changes for good implementation of the required function (freeInstanceList..) #


BNF for the WAE language: 

<WAE> ::= <num> 
  | { + <WAE> <WAE> } 
  | { - <WAE> <WAE> } 
  | { * <WAE> <WAE> } 
  | { / <WAE> <WAE> } 
  | { with { <id> <WAE> } <WAE> } 
  | <id>
  
|#

;; ---------------------------------------------------- The abstract syntax tree WAE
(define-type WAE
  [Num  Number]
  [Add  WAE WAE]
  [Sub  WAE WAE]
  [Mul  WAE WAE]
  [Div  WAE WAE]
  [Id   Symbol]
  [With Symbol WAE WAE]
  )

;; ---------------------------------------------------- parse-sexpr WAE
(: parse-sexpr : Sexpr -> WAE)
;; parse-sexpr function convert s-expressions into WAEs.
;; input: S-expression
;; output: WAE
;; operates: That function convert sexpr type to WAE type, to "help" the other function- parse.
;; The function checks by "match" function the input, and works by the logic of the WAE.
;; If there is not a good syntax- return Error.
;; The function will work with Sexpr only.
(define (parse-sexpr sexpr) 
  (match sexpr 
    [(number: n) (Num n)] 
    [(symbol: name) (Id name)] 
    [(cons 'with more) 
     (match sexpr 
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))] 
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])] 
    [(list '+ lhs rhs) (Add  (parse-sexpr lhs) (parse-sexpr rhs))] 
    [(list '- lhs rhs) (Sub  (parse-sexpr lhs) (parse-sexpr rhs))] 
    [(list '* lhs rhs) (Mul  (parse-sexpr lhs) (parse-sexpr rhs))] 
    [(list '/ lhs rhs) (Div  (parse-sexpr lhs) (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))



;; ---------------------------------------------------- parse WAE
(: parse : String -> WAE)
;; parses a string containing a WAE expression to a WAE AST
;; input: String that represents WAE
;; output: WAE
;; operates: That function convert String type that represents a WAE to WAE type.
;; The function will work with correct String only.
;; The function get the String, make it to be a sexpr by "string->sexpr" function, and check the sexpr:
;; by match function the check the pattern and by cond function to check the validation.
;; if the pattern not good- Error.
(define (parse str)
  (parse-sexpr (string->sexpr str)))



;; ---------------------------------------------------- subst WAE
(: subst : WAE Symbol WAE -> WAE)
;; That function makes the convertion from an Id to the Id's value (only if its identified)
;; input: WAE, Symbol and WAE
;; output: WAE
;; operates: That function convert the identified Id to his value.
;; Substitutes the Symbol, by the second WAE -> inside to the first WAE.
;; The function will work with correct input only.
;; The function get the input and makes check by "cases" function-
;; to check the pattern and the validation of the variant.
;; The return value has no free instances of the Symbol. 
(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst named-expr from to)
           (if (eq? bound-id from) bound-body 
               (subst bound-body from to)))]))


;; ---------------------------------------------------- isSymbolMember? 
(: isSymbolMember? : Symbol (Listof Symbol)  -> Boolean)
;; isSymbolMember? function checks if the given symbol is included in the list.
;; The function checks if there is empty list- if true, return false;
;; else- there is not empty list, and go all over the list to check if there is the desired symbol.
;; The function accepts a symbol and a list of symbols as input.
;; The returned value is a boolean- true\false.
;; I have not encountered any difficulties to solve that question. 
;; I took "ismember?" function and maked some changes for symbols instead of numbers
(define (isSymbolMember? s l)
  (match l
    ['() #f]
    [(cons first rest)
     (cond
       [(equal? first s) #t]
       [else (isSymbolMember? s rest)])]))

;; ---------------------------------------------------- Tests- isSymbolMember?
(test (isSymbolMember? 'a '(a b c)) => #t)
(test (isSymbolMember? 'r '(r rr rrr)) => #t)
(test (isSymbolMember? 's '(s)) => #t)
(test (isSymbolMember? 'y '()) => #f)
(test (isSymbolMember? 'x '(y z)) => #f)
(test (isSymbolMember? 'xxx '(xx)) => #f)
(test (isSymbolMember? 'x '(xx xxx)) => #f)


;; ---------------------------------------------------- remove-duplicates-symbols
(: remove-duplicates-symbols : (Listof Symbol) -> (Listof Symbol))
;; remove-duplicates function removes duplicate symbols included in the set.
;; The main function passes an empty list and the non-unique set.
;; The helper function checks if there is an empty list- if true, and the "new list" empty too- return empty.
;; else- if the list is empty, and the new list is not empty, return the new list.
;; else- there is not empty list, and go all over the list to check if there is a duplicate symbols.
;; if true- remove them, else- append them to the new list.
;; at the end- return the new list..
;; The function accepts a list of symbols as input.
;; The returned value is a list without duplicates symbols.
;; The main difficult to solve that question is the recursion call. 
;; I took "remove-duplicate" function and maked some changes for symbols instead of numbers
(define (remove-duplicates-symbols l)
  (: remove-helper-symbol : (Listof Symbol) (Listof Symbol) -> (Listof Symbol))
  ;; The helper function, description added above- at the main function 
  (define (remove-helper-symbol new old)
    (cond
      [(= (length old) 0)
       (cond
         [(= (length new) 0) old]
         [else new])]
      [else (match old
              [(cons first rest)
               (cond
                 [(isSymbolMember? first rest) (remove-helper-symbol new rest)]
                 [else (remove-helper-symbol (append new  (list first)) rest)])])]))
  (remove-helper-symbol (list) l))

;; ---------------------------------------------------- Tests- remove-duplicates-symbols

(test (remove-duplicates-symbols '(a a a a a a)) => '(a))
(test (remove-duplicates-symbols '(a b c a b c)) => '(a b c))
(test (remove-duplicates-symbols '()) => '())
(test (remove-duplicates-symbols '(x xy)) => '(x xy))
(test (remove-duplicates-symbols '(x y z)) => '(x y z))
(test (remove-duplicates-symbols '(a a a aa aa aa aaa aaa aaa)) => '(a aa aaa))
(test (remove-duplicates-symbols '(z zz z)) => '(zz z))



;; ---------------------------------------------------- freeInstanceList 
(: freeInstanceList : WAE -> (Listof Symbol))
;; freeInstanceList function find and return free (unbouns) symbols and make a list of them.
;; The main function passes an empty list and the required WAE.
;; The helper function checks (by "cases" function) the pattern of the WAE, and by a recursive call-
;; make the right operations and manipulations.
;; The function uses "remove-duplicate-syumbols" and "isSymbolMember" that i wrote below.
;; In addition, the function usese "append" function to fill the list of the free symbols.
;; The main difficult to solve that question is the recursive call. 
;; It took me 3 hours to complete this function. (include the helper function the other two..).
(define (freeInstanceList wae)
  (: helperList : (Listof Symbol) WAE -> (Listof Symbol))
   ;; The helper function, description added above- at the main function 
  (define (helperList declared wae)
    (cases wae
      [(Num n) null]
      [(Add l r) (remove-duplicates-symbols (append (helperList declared l) (helperList declared r)))]
      [(Sub l r) (remove-duplicates-symbols (append (helperList declared l) (helperList declared r)))]
      [(Mul l r) (remove-duplicates-symbols (append (helperList declared l) (helperList declared r)))]
      [(Div l r) (remove-duplicates-symbols (append (helperList declared l) (helperList declared r)))] 
      [(Id s) (cond
                [(isSymbolMember? s declared) null]
                [else (list s)])]
      [(With s wae1 wae2) (remove-duplicates-symbols (append (helperList (append declared (list s)) wae1) (helperList (append declared (list s)) wae2)))]))
  (helperList (list) wae))


;; ---------------------------------------------------- Tests- freeInstanceList
(test (freeInstanceList (parse "w")) => '(w))
(test (freeInstanceList (parse "{with {xxx 2} {with {yyy 3} {+ {- xx y } z}}}")) => '(xx y z))
(test (freeInstanceList (With 'x (Num 2) (Add (Id 'x) (Num 3)))) => '())
(test (freeInstanceList (parse "{+ z {+ x z}}")) => '(x z))
(test (freeInstanceList (parse "abcdef")) => '(abcdef))
(test (freeInstanceList (With 'xxx (Num 2) (With 'yyy (Num 3) (Mul (Add (Sub (Id 'rr) (Id 'y)) (Id 'z)) (Id 'sss))))) => '(rr y z sss))
(test (freeInstanceList (With 'x (Num 2) (Add (Id 'x) (Num 3)))) => '())
(test (freeInstanceList (parse "{- {+ z {+ x z}} r}")) => '(x z r))
(test (freeInstanceList (With 'x (Num 2) (Mul (Id 'x) (Num 3)))) => '())
(test (freeInstanceList (With 'x (Num 2) (Div (Add (Id 'y) (Num 3)) (Id 'z)))) => '(y z))
(test (freeInstanceList (With 'x (Num 2) (Sub (Add (Id 'y) (Num 3)) (Id 's)))) => '(y s))


