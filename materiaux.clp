(deftemplate Bottle
    -100 100 number
    (   (N (-10 1) (0 0))
	(Z (-1 0) (0 1) (90 0))
        (P  (80 0) (95 1)) )
)

(deftemplate WH
    -10 10 num
    (   (N  (-1 1) (0 0))
	(Z (-1 0) (0 1) (1 0))
	(P  (0 0) (2 1)) )
)
(deftemplate Mix
    -50 50 mix
    (   (BN (-10 1) (0 0))
        (N (-5 0) (-2 1) (0 0))
	(Z (-2 0) (0 1) (2 0))
        (P (0 0) (5 1) (10 0))
	(BP(5 0) (10 1)) )
)

(defrule Get_User_Input 
  ?i <- (initial-fact) 
  => 
  (printout t "Enter a quantity of bottles/PET available n (n<100) :") 
  (bind ?response (read)) 
  (assert (crispBottle ?response)) 
  (printout t "Enter a quantity of WH available: (n<10) " ) 
  (bind ?response (read)) 
  (assert (crispWH ?response)) 
  (retract ?i)) 

;;fuzzify
(defrule fuzzify 
  (crispBottle ?b) 
  (crispWH ?w) 
  => 
(assert (Bottle (?b 0) (?b 1) (?b 0) ))
(assert (WH (?w 0) (?w 1) (?w 0) ))
)
;;defuzzify
 
(defrule defuzzify1 
  (declare (salience -1)) 
  ?f <- (Mix ?) 
  => 
  (bind ?t (maximum-defuzzify ?f))
  (assert (action ?t) ) 
  (printout t "mix--> " ?t crlf)
)
;;action rules
(defrule action_1
  (declare (salience -1)) 
  ?a<-(action ?f)
  (test (>= ?f 10))
  => 
  (retract ?a)
  (assert (raw Bottle))
  (assert (raw WH))
  (printout t "mix possible "crlf)
)
(defrule action_2
  (declare (salience -1)) 
  ?a<-(action ?f)
  (test (< ?f 10))
  (test (>= ?f 5))
  => 
  (retract ?a)
  (assert (raw Bottle))
  (printout t "mix partial "crlf)
)
(defrule action_3
  (declare (salience -1)) 
  ?a<-(action ?f)
  (test (< ?f 5))
  (test (> ?f 1))
  => 
  (retract ?a)
  (assert (raw WH))
  (printout t "mix partial "crlf)
)
(defrule action_4
  (declare (salience -1)) 
  ?a<-(action ?f)
  (test (<= ?f 1))
  => 
  (retract ?a)
  (printout t "mix not possible "crlf)
)


(defrule BP 
  (Bottle P) 
  (WH P) 
  => 
  (assert (Mix BP))
  (printout t "BP" crlf)
) 
(defrule PZ 
  (Bottle P) 
  (WH Z) 
  => 
  (assert (Mix P))
  (printout t "P" crlf)
) 

(defrule ZP 
  (Bottle Z) 
  (WH P) 
  => 
  (assert (Mix P))
  (printout t "P" crlf)
) 
(defrule ZZ 
  (Bottle Z) 
  (WH Z) 
  => 
  (assert (Mix Z))
  (printout t "Z" crlf)
) 
(defrule NZ 
  (Bottle N) 
  (WH Z) 
  => 
   (assert (Mix N))
  (printout t "N" crlf)
) 
(defrule ZN 
  (Bottle Z) 
  (WH N) 
  => 
  (assert (Mix N))
  (printout t "N" crlf)
) 
(defrule BN 
  (Bottle N) 
  (WH N) 
  => 
  (printout t "BN" crlf)
) 




(deffacts initial-state
  (raw Credit_Card)
  (raw Jerrycan)
  (raw Bumper)
  (polymer PET)
  (polymer PP)
  (organic WH)
  (made_of Bottle PET)
  (made_of Credit_Card PET)
  (made_of Jerrycan PP)
  (made_of Bumper PP)
  (need Lamp)
  (need Chair)
  (need Brick_wall)
  (make_using Lamp PET)
  (make_using_mix Chair PP WH)
  (make using_mix Brick_wall PP WH)
  (make_tech Lamp Thermo)
  (make_tech Chair Inject)
  (make_tech Brick_wall Inject)
)

(defrule cleaning
(declare (salience -2))
?f<-(raw ?Material)
(made_of ?Material ?Base)
=>
(retract ?f)
(assert(cleaned ?Base))
)

(defrule grinding
(declare (salience -2))
?f<-(cleaned ?Base)
=>
(retract ?f)
(assert (flakes ?Base)) 
)

(defrule org_grinding
(declare (salience -3))
?f<-(raw ?Base)
(organic ?Base)
=>
(retract ?f)
(assert (flakes ?Base))
)

(defrule injection_moulding
(declare (salience -4))
?f<-(flakes ?Poly)
?g<-(flakes ?Org)
?h<-(inject ?Obj)
(organic ?Org)
(polymer ?Poly)
(make_using_mix ?Obj ?Poly ?Org)
=>
(retract ?f)
(retract ?g)
(retract ?h)
(assert (upcycling_mix ?Org ?Poly))
)

(defrule thermoforming
(declare (salience -4))
?f<-(flakes ?Poly)
?g<-(thermo ?Obj)
(make_using ?Obj ?Poly)
(polymer ?Poly)
=>
(retract ?f)
(retract ?g)
(assert (upcycling ?Poly))
)

(defrule make_mix_obj
(declare (salience -4))
?f<-(upcycling_mix ?Org ?Poly)
?g<-(need ?Obj)
(make_using_mix ?Obj ?Poly ?Org)
=>
(assert(resultat ?Obj))
(printout t ?Obj " created with upcycling " ?Poly " and " ?Org crlf)
)

(defrule make_obj
(declare (salience -4))
?f<-(upcycling ?Poly)
?g<-(need ?Obj)
(make_using ?Obj ?Poly)
=>
(retract ?f)
(retract ?g)
(assert(resultat ?Obj))
(printout t ?Obj " created with upcycling " ?Poly crlf)
)

(defrule build_obj_simple
(declare (salience -2))
(need ?Obj)
(make_tech ?Obj Thermo)
=>
(assert (thermo ?Obj))
)

(defrule build_obj_complex
(declare (salience -2))
(need ?Obj)
(make_tech ?Obj Inject)
=>
(assert (inject ?Obj))
)

