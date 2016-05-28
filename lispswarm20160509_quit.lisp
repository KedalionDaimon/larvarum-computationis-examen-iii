; This is the "fast" version working with "only" 1000 atoms.

; MY ISSUE IS THE DOUBLE-CREATION OF UNKNOWN ATOMS AT THE RE-EVAL STAGE:
; (re-eval '(aa . bb) '((x . y) (b . c) (a . c) (b . d) (a . d) (l . r) (s . t) (l . m) (n . o) (o . p) (p . q) (q . r) (r . s)))
; --> (0 (AA . BB) (AA . BB) (X . Y) (B . C) (A . C) (B . D) (A . D) (L . R) (S . T) (L . M) (N . O) (O . P) (P . Q))
; whereas Scheme does:
; --> (1 (aa . bb) (x . y) (b . c) (a . c) (b . d) (a . d) (l . r) (s . t) (l . m) (n . o) (o . p) (p . q) (q . r))
; USING INCOGNITUM, NOW CORRECTED TO:
; --> (1 (AA . BB) (X . Y) (B . C) (A . C) (B . D) (A . D) (L . R) (S . T) (L . M) (N . O) (O . P) (P . Q) (Q . R))

; I AM USING A LOT OF CONSP INSTEAD OF MY "PAIR".
; USE THIS THING IN CLISP, _SKIP_ THE RECURSIVELEARN!
; MOST LIKELY - THE PROBLEM IS THAT I HAVE "NIL" FOR NOT FOUND ARGUMENTS! IT SHOULD NOT BE NIL! IT USED TO BE '()!

; I ACHIEVED PLANNING WITH ANALOGIES!

; The version "B" contains a more far-reaching snowflake mechanism
; (see below).

; LarCom E

; This system employs directional logical triangulation.
; The function proto-eval-triangle re-considers all possible cases of
; a directional conclusion. The predecessor system, victri, was
; operating exclusively on vic atoms. The system expresses the
; influence of a clause by putting it at the front of the list
; of all atoms (i.e. left-most) if it is influential. The system is
; not value-based but purely position-based.

; Herein, vic-connections are defined as dotted pairs - (a . b) - while
; ana-connections are expressed by means of lists - (a b).

; The precise manner of operation can be influenced below under
; USER-SET PARAMETERS.

; The present system has two enhancements to its reasoning mechanism.
; Firstly, it has a "re-slider" - a mechanism for re-considering input
; several times. - This is the "sledge". To disable the sledge, the
; iterations of re-sliding can be set to 0. Secondly, it considers the
; "considerations of the considerations" of a given vic-connection of
; input - and not only the vic-connection's sole considerations.
; (That mechanism may have become "too fanciful", but is still kept
; in existence.) It is implemented in the re-eval function below. If
; it is no longer desired, the "original" re-eval function may be
; re-implemented. - As reasoning, however, grows exponentially, it is
; being limited by the setting of "globallimit" - this global variable
; limits how many conclusions one single couple of atoms can enter
; into - if set to a negative value, it is effectively disabled.

; It could be re-considered whether planning really cannot incorporate
; at all ana-atoms, or whether simply one of the two alternatives has
; to be selected. The present choice - plans consist only out of vic-
; atoms - is more consistent, as only "elementary atoms" are in a
; vicinity, while the inclusion of ana-atoms would mean to include in
; planning the decision between alternatives. However, as one
; alternative _should_ exist, this would be imaginable.

; PREPARATIONS FOR THE USER-SET PARAMETERS:
; A text shall be given to the system for "pre-learning" prior to user
; interaction, so it already has some "fundamental ideas" and needs no
; teaching like a baby.

; For this purpose, the text shall be subdivided into partially
; overlapping sections.

; Take the first atoms of a list:
(defun proto-frontpart (howfar inlist resultlist)
  (if (or (zerop howfar) (null inlist)) (reverse resultlist)
    (proto-frontpart (- howfar 1) (cdr inlist) (cons (car inlist) resultlist))))

(defun frontpart (howfar inlist) (proto-frontpart howfar inlist '()))
; sample call:
; (frontpart 3 '(a b c d e f)) --> (a b c)
; (frontpart 10 '(a b c d e f)) --> (a b c d e f)

; skip the first atoms of a list:
(defun skip (howmany inlist)
  (if (or (zerop howmany) (null inlist)) inlist
    (skip (- howmany 1) (cdr inlist))))
; (skip 2 '(a b c d e)) --> (c d e)
; (skip 0 '(a b c d e)) --> (a b c d e)

; The chainload function cuts a long input list of text to be learned
; into partially overlapping smaller portions.

(defun proto-chainload (xskip ylength inlist resultlist)
  (if (null inlist) (reverse resultlist)
    (proto-chainload xskip ylength (skip xskip inlist) (cons (frontpart ylength inlist) resultlist))))

(defun chainload (xskip ylength inlist) (proto-chainload xskip ylength inlist '()))

; ==== USER-SET PARAMETERS ====

; "knowledgesize" determines the capacity of the system to contain atom
; connections - a higher count makes the system more intelligent.
(defvar knowledgesize 1000)

; "inmemory" determines on what size of input history the system shall
; reason - in particular, it should be longer than the longest expected
; input to the system. - If the input is longer, its front part is cut
; off and only the back part is used for reasoning.
(defvar inmemory 30)

; "re-slide-count" determines how often the system re-considers given
; input - set it to 0 in order to consider input only once.
(defvar re-slide-count 1)

; How many conclusions can be made with one pair that is in the focus
; of reasoning - set it to a negative number to enable unlimited
; reasoning, but beware that this quickly overwrites all other known
; atoms. It forces termination on countdown to "zero?".
(defvar globallimit 3)

; "rootlist" represents the initial list of "pre-set" learned material
; - the more, the better. This list can be also loaded from a prepared
; file - this is only a very short example. That list should be huge.
; This is used to implant previous knowledge into the system ere "using" it.
; (defvar rootlist '(a b c d e f g h i j))

; "initlist" defines a list of "chunks" of the rootlist, each "chunk"
; being separately learned by the system. This setting means:
; chunks of length three, each chunk progresses by 1 atom from the
; previous chunk (3 3 would have meant no repetitions in the chunks).
; The chunks can be of larger size - the larger, the better -
; however, large chunks use a lot of knowledge space as they are
; always fully hierarchised. "knowledgesize" must have been chosen
; appropriately large.
; (defvar initlist (chainload 1 3 rootlist))

(defvar snow-depth 1) ; how many re-considerations should be possible.
; use a snow-depth of 1 to have only 1 re-call (as before). "More
; fanciful" is a higher snow-depth.

; This defines how many words of past input may be maximum remembered.
; If all of the input words can be found inside the history, then this
; "insect" is suitable for handling said input.
(defvar historylength 30)

; ==== END OF USER-SET PARAMETERS ====

; That function is a cshift/eoshift-equivalent and has not been used -
; but should I ever start using vectors here, this may change:
; (defun proto-find-rotate (element position listofallatoms resultlist)
;   (if (null? listofallatoms) (cons position (cons element (reverse (cdr resultlist))))
;     (if (equal? element (car listofallatoms))
;       (cons position (append (cons element (reverse resultlist)) (cdr listofallatoms)))
;       (proto-find-rotate element (+ 1 position) (cdr listofallatoms) (cons (car listofallatoms) resultlist)))))
; sample calls:
; (proto-find-rotate 'a 0 '(x y z a b c) '()) --> (3 a x y z b c)
; (proto-find-rotate 'q 0 '(x y z a b c) '()) --> (6 q x y z a b)

; (defun find-rotate (element listofallatoms) (proto-find-rotate element 0 listofallatoms '()))

; In order to look at possible connections within the input, it should
; be split into possible pairs. This is done by the following function
; based on this observation:
; (map cons '(a b c) '(x y z)) --> ((a . x) (b . y) (c . z))
(defun mapcons (fulllist) (mapcar #'cons (reverse (cdr (reverse fulllist))) (cdr fulllist)))
; a variation of it could be, if you wish to tolerate the existence of
; (a)-style "connection", would be this:
; (defun mapcons (fulllist) (map cons fulllist (cdr fulllist)))
; that would be interesting for "no-response-plans", i.e. where "A" is
; found and the response is "()".
; sample call of current version:
; (mapcons '(a b c d)) --> ((a . b) (b . c) (c . d))

; If a vic-connection of the type (a . a), i.e. between the same atom,
; is observed, it cannot participate in further reasoning. - The only
; question is, whether it can be confirmed as already observed or not.
; A question is whether I should loot at analogies of the type (a a).
; Currently, they are not considered as I have not foreseen a way for
; their creation - that an atom is analogous to itself really needs no
; explicit mentioning.
(defun extractpair (sameatoms listofallatoms atomschecked)
  (if (null listofallatoms)
    (cons NIL (cons sameatoms (reverse atomschecked)))
    (if (equal sameatoms (car listofallatoms))
      (cons T (cons sameatoms (append (reverse atomschecked) (cdr listofallatoms) '((nihil . nihil)))))
      (extractpair sameatoms (cdr listofallatoms) (cons (car listofallatoms) atomschecked)))))
; sample calls:
; (extractpair '(a . a) '((b . c) (c . d) (d . e) (e . f)) '()) --> (NIL (a . a) (b . c) (c . d) (d . e) (e . f))
; (extractpair '(a . a) '((b . c) (c . d) (d . e) (a . a)) '()) --> (T (a . a) (b . c) (c . d) (d . e) (NIHIL . NIHIL))

; The below triangulation function tests different conditions that
; may have been triggered by triangulation. They are one of:
; - reaching a confirmative conclusion - #t;
; - reaching a contradicting conclusion - #f; - I may consider to
; merely deliver "(#f)" in such a case, rather than the entire main
; knowledge list, if I do not re-use that list with the contradiction;
; - an inconclusive state - '() - when nothing can be said;
; none of the above, which warrants a re-try as long as there are
; untested atoms in the list of all atoms.

; In the below triangulation function, meeting the "opposite" of a
; connection means, generally, a contradiction to it. That is true
; in particular for the first and the third pairs. The first pair must
; either be confirmed by direct observation or conclude its triangle
; before its "opposite" can be seen. A=B (being the same as (B=A),
; A->B and B->A (these latter are considered distinct) are each a
; possible "opposite" of the other two. Only the second pair is
; so far more tolerant to being countered - it is only "dropped" but
; does not stop further reasoning, and that is only the case if the
; proposed third atom has not yet been seen.

; CONSIDER REPLACING THE "IF's" WITH ONE HUGE "COND".

; Avoiding "double" analogies - (b a) and (a b) - should already be the
; case as the "recognition" of analogies also checks for their reverse.

; A few effects used herein:
; (equal? (car '(a . b)) (cdar '((x . a)))) --> #t
; A->B X->A : X->B : (cons (caar '((x . a))) (cdr '(a . b)))
; (equal? (cdr '(a . b)) (caar '((b . x)))) --> #t
; A->B B->X : A->X : (cons (car '(a . b)) (cdar '((b . x))))
; (cons (cdr '(x . y)) (car '(x . y))) --> (y . x)

; A little auxiliary function to distinguish a "pair" and a "list" of
; two elements - the one is a vic-connection, the other is an ana-
; connection, and they are treated differently:

(defun ecdr (x) (if (or (symbolp x) (numberp x) NIL) NIL (cdr x)))
(defun ecar (x) (if (or (symbolp x) (numberp x) NIL) NIL (car x)))
(defun ecaar (x) (if (or (symbolp x) (numberp x) NIL) NIL (ecar (ecar x))))
(defun ecadr (x) (if (or (symbolp x) (numberp x) NIL) NIL (ecar (ecdr x))))
(defun ecdar (x) (if (or (symbolp x) (numberp x) NIL) NIL (ecdr (ecar x))))
(defun ecddr (x) (if (or (symbolp x) (numberp x) NIL) NIL (ecdr (ecdr x))))
(defun ecaaar (x) (if (or (symbolp x) (numberp x) NIL) NIL (ecar (ecaar x))))
(defun ecaadr (x) (if (or (symbolp x) (numberp x) NIL) NIL (ecar (ecadr x))))
(defun ecadar (x) (if (or (symbolp x) (numberp x) NIL) NIL (ecar (ecdar x))))
(defun ecaddr (x) (if (or (symbolp x) (numberp x) NIL) NIL (ecar (ecddr x))))
(defun ecdaar (x) (if (or (symbolp x) (numberp x) NIL) NIL (ecdr (ecaar x))))
(defun ecdadr (x) (if (or (symbolp x) (numberp x) NIL) NIL (ecdr (ecadr x))))
(defun ecddar (x) (if (or (symbolp x) (numberp x) NIL) NIL (ecdr (ecdar x))))
(defun ecdddr (x) (if (or (symbolp x) (numberp x) NIL) NIL (ecdr (ecddr x))))

; I got fed up and pair got a recursive definition:

(defun proto-pair (x)
  (if (null x) NIL
    (if (atom x) T
      (proto-pair (cdr x)))))

(defun pair (x)
  (if (atom x) NIL
    (proto-pair x)))

; sample calls:
; (consp '(a . b)) --> T
; (consp '(a b)) --> T
; however:
; (pair '(a . b)) --> T
; (pair '(a b)) --> NIL


; I also created a more robust list test:

(defun listq (x) (if (and (listp x) (not (pair x))) T NIL))
; sample calls:
; (listp '(a . b)) --> T
; (listp '(a b)) --> T
; (listq '(a . b)) --> NIL
; (listq '(a b)) --> T

(defun proto-eval-triangle (firstpair secondpair thirdpair firstpairseen secondpairseen thirdpairseen inconclusives listofallatoms atomschecked)
; You may uncomment (progn ... ) in order to trace reasoning.
  (progn ; (terpri) (print "firstpair: ") (print firstpair) (print " secondpair: ") (print secondpair) (print " thirdpair: ") (print thirdpair)
         ; (print " firstpairseen: ") (print firstpairseen) (print " secondpairseen: ") (print secondpairseen)
         ; (print " thirdpairseen: ") (print thirdpairseen) (print " inconclusives: ") (print inconclusives)
         ; (print " list of all atoms: ") (print listofallatoms)
         ; (print " atoms checked: ") (print atomschecked)

  (cond

  ; POSITIVE:
  ; all three sides seen, triangulation possible:
  ((if (and (equal T firstpairseen) (equal T secondpairseen) (equal T thirdpairseen)) T NIL)
  (progn ; (terpri) (print "CLAUSE 1 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (terpri)
    (cons T (append (cons firstpair (cons secondpair (cons thirdpair (reverse atomschecked)))) listofallatoms '((nihil . nihil))))))

  ; INCONCLUSIVE:
  ; nothing whatsoever has been seen - thirdpairseen is impossible if secondpairseen is NIL:
  ((if (and (null listofallatoms) (equal NIL firstpairseen) (equal NIL secondpairseen)) T NIL)
  (progn ; (terpri) (print "CLAUSE 2 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (terpri)
    (cons 'INCOGNITUM (cons firstpair (reverse atomschecked))))) ; ======= THAT USED TO BE '()

  ; RECOGNISED:
  ; only the first pair has been seen:
  ((if (and (null listofallatoms) (equal T firstpairseen) (equal NIL secondpairseen) (equal NIL thirdpairseen)) T NIL)
  (progn ; (terpri) (print "CLAUSE 3 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (terpri)
    (cons T (cons firstpair (append (reverse atomschecked) '((nihil . nihil)))))))

  ; CONCLUDED:
  ; the first pair has been seen and the second pair has been seen - the third pair is the "conclusion":
  ((if (and (null listofallatoms) (equal T firstpairseen) (equal T secondpairseen) (equal NIL thirdpairseen)) T NIL)
  (progn ; (terpri) (print "CLAUSE 4 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (terpri)
    (cons T (cons firstpair (cons secondpair (cons thirdpair (reverse atomschecked)))))))

  ; CONCLUDED:
  ; the first pair has not been seen, but everything else has been seen - the first pair is the "conclusion":
  ((if (and (null listofallatoms) (equal NIL firstpairseen) (equal T secondpairseen) (equal T thirdpairseen)) T NIL)
  (progn ; (terpri) (print "CLAUSE 5 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (terpri)
    (cons T (cons firstpair (cons secondpair (cons thirdpair (reverse atomschecked)))))))
 ; same as above one

  ; INCONCLUSIVE:
  ; the first pair has not been seen, but is a hypothesis - but the third pair cannot yet be concluded;
  ; still, the first and the second pair can be put on the front part:
  ((if (and (null listofallatoms) (equal NIL firstpairseen) (equal T secondpairseen) (equal NIL thirdpairseen)) T NIL)
  (progn ; (terpri) (print "CLAUSE 6 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (terpri)
    (cons 'INCONGITUM (cons firstpair (cons secondpair (reverse atomschecked)))))) ; ======= THAT USED TO BE '()

  ; NEGATIVE - THIRD PAIR CONTRADICTED, FIRST PAIR SEEN (pair): --- THIS IS EXPRESSED A BIT UNUSUALLY COMPARED TO BELOW
  ((if (and (equal T firstpairseen) (equal T secondpairseen) (equal NIL thirdpairseen) (pair thirdpair)
    (or (and (listq (car listofallatoms))
        (or (equal (car listofallatoms) (list (car thirdpair) (cdr thirdpair)))
            (equal (car listofallatoms) (list (cdr thirdpair) (car thirdpair)))))
    (and (pair (car listofallatoms))
         (equal (car listofallatoms) (cons (cdr thirdpair) (car thirdpair)))))) T NIL)
  (progn ; (terpri) (print "CLAUSE 7 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
      (cons NIL (append (reverse atomschecked) listofallatoms (list thirdpair secondpair firstpair)))))

  ; NEGATIVE - THIRD PAIR CONTRADICTED, FIRST PAIR SEEN (list):
  ((if (and (equal T firstpairseen) (equal T secondpairseen) (equal NIL thirdpairseen) (listq thirdpair)
           (pair (car listofallatoms))
           (or (equal (car listofallatoms) (cons (car thirdpair) (ecadr thirdpair)))
               (equal (car listofallatoms) (cons (ecadr thirdpair) (car thirdpair))))) T NIL)
  (progn ; (terpri) (print "CLAUSE 8 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
      (cons NIL (append (reverse atomschecked) listofallatoms (list thirdpair secondpair firstpair)))))

  ; NEGATIVE - THIRD PAIR CONTRADICTED, FIRST PAIR NOT SEEN (pair): --- THIS IS EXPRESSED A BIT UNUSUALLY COMPARED TO BELOW
  ((if (and (equal NIL firstpairseen) (equal T secondpairseen) (equal NIL thirdpairseen) (pair thirdpair)
    (or (and (listq (car listofallatoms))
        (or (equal (car listofallatoms) (list (car thirdpair) (cdr thirdpair)))
            (equal (car listofallatoms) (list (cdr thirdpair) (car thirdpair)))))
    (and (pair (car listofallatoms))
         (equal (car listofallatoms) (cons (cdr thirdpair) (car thirdpair)))))) T NIL)
  (progn ; (terpri) (print "CLAUSE 9 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
      (cons NIL (append (reverse atomschecked) listofallatoms (list thirdpair secondpair)))))

  ; NEGATIVE - THIRD PAIR CONTRADICTED, FIRST PAIR NOT SEEN (list):
  ((if (and (equal NIL firstpairseen) (equal T secondpairseen) (equal NIL thirdpairseen) (listq thirdpair)
           (pair (car listofallatoms))
           (or (equal (car listofallatoms) (cons (car thirdpair) (ecadr thirdpair)))
               (equal (car listofallatoms) (cons (ecadr thirdpair) (car thirdpair))))) T NIL)
  (progn ; (terpri) (print "CLAUSE 10 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
      (cons NIL (append (reverse atomschecked) listofallatoms (list thirdpair secondpair)))))

  ; NEGATIVE - FIRST PAIR CONTRADICTED BEFORE IT WAS SEEN, SECOND ATOM NOT SEEN, EITHER (pair):
  ((if (and (equal NIL firstpairseen) (equal NIL secondpairseen) (pair firstpair)
           (or (and (pair (car listofallatoms)) (equal (car listofallatoms) (cons (cdr firstpair) (car firstpair))))
               (and (listq (car listofallatoms))
                    (or (equal (car listofallatoms) (list (car firstpair) (cdr firstpair)))
                        (equal (car listofallatoms) (list (cdr firstpair) (car firstpair))))))) T NIL)
  (progn ; (terpri) (print "CLAUSE 11 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (cons NIL (append (reverse atomschecked) listofallatoms (list firstpair)))))

  ; NEGATIVE - FIRST PAIR CONTRADICTED BEFORE IT WAS SEEN, SECOND ATOM NOT SEEN, EITHER (list):
  ((if (and (equal NIL firstpairseen) (equal NIL secondpairseen) (listq firstpair)
           (pair (car listofallatoms))
               (or (equal (car listofallatoms) (cons (car firstpair) (ecadr firstpair)))
                   (equal (car listofallatoms) (cons (ecadr firstpair) (car firstpair))))) T NIL)
  (progn ; (terpri) (print "CLAUSE 12 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (cons NIL (append (reverse atomschecked) listofallatoms (list firstpair)))))

  ; NEGATIVE - FIRST PAIR CONTRADICTED BEFORE IT WAS SEEN, SECOND ATOM SEEN, THIRD ATOM NOT SEEN (pair):
  ((if (and (equal NIL firstpairseen) (equal T secondpairseen) (equal NIL thirdpairseen) (pair firstpair)
           (or (and (pair (car listofallatoms)) (equal (car listofallatoms) (cons (cdr firstpair) (car firstpair))))
               (and (listq (car listofallatoms))
                    (or (equal (car listofallatoms) (list (car firstpair) (cdr firstpair)))
                        (equal (car listofallatoms) (list (cdr firstpair) (car firstpair))))))) T NIL)
  (progn ; (terpri) (print "CLAUSE 13 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (cons NIL (append (reverse atomschecked) listofallatoms (list secondpair firstpair)))))

  ; NEGATIVE - FIRST PAIR CONTRADICTED BEFORE IT WAS SEEN, SECOND ATOM SEEN, THIRD ATOM NOT SEEN (list):
  ((if (and (equal NIL firstpairseen) (equal T secondpairseen) (equal NIL thirdpairseen) (listq firstpair)
           (pair (car listofallatoms))
               (or (equal (car listofallatoms) (cons (car firstpair) (ecadr firstpair)))
                   (equal (car listofallatoms) (cons (ecadr firstpair) (car firstpair))))) T NIL)
  (progn ; (terpri) (print "CLAUSE 14 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (cons NIL (append (reverse atomschecked) listofallatoms (list secondpair firstpair)))))

  ; NEGATIVE - FIRST PAIR CONTRADICTED BEFORE IT WAS SEEN, SECOND ATOM SEEN, THIRD ATOM SEEN:
  ((if (and (equal NIL firstpairseen) (equal T secondpairseen) (equal T thirdpairseen) (pair firstpair)
           (or (and (pair (car listofallatoms)) (equal (car listofallatoms) (cons (cdr firstpair) (car firstpair))))
               (and (listq (car listofallatoms))
                    (or (equal (car listofallatoms) (list (car firstpair) (cdr firstpair)))
                        (equal (car listofallatoms) (list (cdr firstpair) (car firstpair))))))) T NIL)
  (progn ; (terpri) (print "CLAUSE 15 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (cons NIL (append (reverse atomschecked) listofallatoms (list thirdpair secondpair firstpair)))))

  ; NEGATIVE - FIRST PAIR CONTRADICTED BEFORE IT WAS SEEN, SECOND ATOM SEEN, THIRD ATOM SEEN:
  ((if (and (equal NIL firstpairseen) (equal T secondpairseen) (equal T thirdpairseen) (listq firstpair)
           (pair (car listofallatoms))
               (or (equal (car listofallatoms) (cons (car firstpair) (ecadr firstpair)))
                   (equal (car listofallatoms) (cons (ecadr firstpair) (car firstpair))))) T NIL)
  (progn ; (terpri) (print "CLAUSE 16 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (cons NIL (append (reverse atomschecked) listofallatoms (list thirdpair secondpair firstpair)))))

  ; RECALL - THIRD PAIR SEEN (pair):
  ; whether the first pair is seen or not is determined in the re-call, i.e. whether all three sides were seen.
  ((if (and (equal T secondpairseen) (equal NIL thirdpairseen) (pair thirdpair)
           (pair (car listofallatoms)) (equal (car listofallatoms) thirdpair)) T NIL) ; then re-call with thirdpairseen set to "true"
  (progn ; (terpri) (print "CLAUSE 17 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair secondpair thirdpair firstpairseen secondpairseen T inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - THIRD PAIR SEEN (list):
  ; whether the first pair is seen or not is determined in the re-call, i.e. whether all three sides were seen.
  ((if (and (equal T secondpairseen) (equal NIL thirdpairseen) (listq thirdpair)
           (listq (car listofallatoms))
           (or (equal (car listofallatoms) thirdpair)
               (equal (car listofallatoms) (reverse thirdpair)))) T NIL) ; then re-call with thirdpairseen set to "true"
  (progn ; (terpri) (print "CLAUSE 18 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair secondpair thirdpair firstpairseen secondpairseen T inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - FIRST PAIR SEEN (pair):
  ((if (and (equal NIL firstpairseen) (pair firstpair)
           (pair (car listofallatoms)) (equal (car listofallatoms) firstpair)) T NIL) ; then re-call with firstpairseen set to "true"
  (progn ; (terpri) (print "CLAUSE 19 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair secondpair thirdpair T secondpairseen thirdpairseen inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - FIRST PAIR SEEN (list):
  ((if (and (equal NIL firstpairseen) (listq firstpair)
           (listq (car listofallatoms))
           (or (equal (car listofallatoms) firstpair)
               (equal (car listofallatoms) (reverse firstpair)))) T NIL) ; then re-call with firstpairseen set to "true"
  (progn ; (terpri) (print "CLAUSE 20 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair secondpair thirdpair T secondpairseen thirdpairseen inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - SECOND PAIR SEEN (pair and pair, first possibility vic-vic-vic):
  ((if (and (equal NIL secondpairseen) (pair firstpair) (pair (car listofallatoms))
           (equal (car firstpair) (ecdar listofallatoms))
           (not (equal (cdr firstpair) (ecaar listofallatoms)))
           (not (equal (car firstpair) (cdr firstpair)))
           (not (equal (ecaar listofallatoms) (ecdar listofallatoms)))) T NIL)
  (progn ; (terpri) (print "CLAUSE 21 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair (car listofallatoms) (cons (ecaar listofallatoms) (cdr firstpair))
      firstpairseen T thirdpairseen inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - SECOND PAIR SEEN (pair and pair, second possibility vic-vic-vic):
  ((if (and (equal NIL secondpairseen) (pair firstpair) (pair (car listofallatoms))
           (equal (cdr firstpair) (ecaar listofallatoms))
           (not (equal (car firstpair) (ecdar listofallatoms)))
           (not (equal (car firstpair) (cdr firstpair)))
           (not (equal (ecaar listofallatoms) (ecdar listofallatoms)))) T NIL)
  (progn ; (terpri) (print "CLAUSE 22 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair (car listofallatoms) (cons (car firstpair) (ecdar listofallatoms))
      firstpairseen T thirdpairseen inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - SECOND PAIR SEEN (pair and pair, third possibility vic-vic-ana):
  ((if (and (equal NIL secondpairseen) (pair firstpair) (pair (car listofallatoms))
           (equal (car firstpair) (ecaar listofallatoms))
           (not (equal (cdr firstpair) (ecdar listofallatoms)))
           (not (equal (car firstpair) (cdr firstpair)))
           (not (equal (ecaar listofallatoms) (ecdar listofallatoms)))) T NIL)
  (progn ; (terpri) (print "CLAUSE 23 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair (car listofallatoms) (list (cdr firstpair) (ecdar listofallatoms))
      firstpairseen T thirdpairseen inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - SECOND PAIR SEEN (pair and pair, fourth possibility vic-vic-ana):
  ((if (and (equal NIL secondpairseen) (pair firstpair) (pair (car listofallatoms))
           (equal (cdr firstpair) (ecdar listofallatoms))
           (not (equal (car firstpair) (ecaar listofallatoms)))
           (not (equal (car firstpair) (cdr firstpair)))
           (not (equal (ecaar listofallatoms) (ecdar listofallatoms)))) T NIL)
  (progn ; (terpri) (print "CLAUSE 24 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair (car listofallatoms) (list (car firstpair) (ecaar listofallatoms))
      firstpairseen T thirdpairseen inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - SECOND PAIR SEEN (list and list, first possibility ana-ana-ana):
  ((if (and (equal NIL secondpairseen) (listq firstpair) (listq (car listofallatoms))
           (equal (car firstpair) (ecaar listofallatoms))
           (not (equal (ecadr firstpair) (ecadar listofallatoms)))
           (not (equal (car firstpair) (ecadr firstpair)))
           (not (equal (ecaar listofallatoms) (ecadar listofallatoms)))) T NIL)
  (progn ; (terpri) (print "CLAUSE 25 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair (car listofallatoms) (list (ecadr firstpair) (ecadar listofallatoms))
      firstpairseen T thirdpairseen inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - SECOND PAIR SEEN (list and list, second possibility ana-ana-ana):
  ((if (and (equal NIL secondpairseen) (listq firstpair) (listq (car listofallatoms))
           (equal (ecadr firstpair) (ecadar listofallatoms))
           (not (equal (car firstpair) (ecaar listofallatoms)))
           (not (equal (car firstpair) (ecadr firstpair)))
           (not (equal (ecaar listofallatoms) (ecadar listofallatoms)))) T NIL)
  (progn ; (terpri) (print "CLAUSE 26 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair (car listofallatoms) (list (car firstpair) (ecaar listofallatoms))
      firstpairseen T thirdpairseen inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - SECOND PAIR SEEN (list and list, third possibility ana-ana-ana):
  ((if (and (equal NIL secondpairseen) (listq firstpair) (listq (car listofallatoms))
           (equal (car firstpair) (ecadar listofallatoms))
           (not (equal (ecadr firstpair) (ecaar listofallatoms)))
           (not (equal (car firstpair) (ecadr firstpair)))
           (not (equal (ecaar listofallatoms) (ecadar listofallatoms)))) T NIL)
  (progn ; (terpri) (print "CLAUSE 27 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair (car listofallatoms) (list (ecadr firstpair) (ecaar listofallatoms))
      firstpairseen T thirdpairseen inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - SECOND PAIR SEEN (list and list, fourth possibility ana-ana-ana):
  ((if (and (equal NIL secondpairseen) (listq firstpair) (listq (car listofallatoms))
           (equal (ecadr firstpair) (ecaar listofallatoms))
           (not (equal (car firstpair) (ecadar listofallatoms)))
           (not (equal (car firstpair) (ecadr firstpair)))
           (not (equal (ecaar listofallatoms) (ecadar listofallatoms)))) T NIL)
  (progn ; (terpri) (print "CLAUSE 28 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair (car listofallatoms) (list (car firstpair) (ecadar listofallatoms))
      firstpairseen T thirdpairseen inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - SECOND PAIR SEEN (pair and pair, first possibility vic-ana-vic):
  ((if (and (equal NIL secondpairseen) (pair firstpair) (listq (car listofallatoms))
           (equal (car firstpair) (ecaar listofallatoms))
           (not (equal (cdr firstpair) (ecadar listofallatoms)))
           (not (equal (car firstpair) (cdr firstpair)))
           (not (equal (ecaar listofallatoms) (ecadar listofallatoms)))) T NIL)
  (progn ; (terpri) (print "CLAUSE 29 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair (car listofallatoms) (cons (ecadar listofallatoms) (cdr firstpair))
      firstpairseen T thirdpairseen inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - SECOND PAIR SEEN (pair and pair, second possibility vic-ana-vic):
  ((if (and (equal NIL secondpairseen) (pair firstpair) (listq (car listofallatoms))
           (equal (cdr firstpair) (ecadar listofallatoms))
           (not (equal (car firstpair) (ecaar listofallatoms)))
           (not (equal (car firstpair) (cdr firstpair)))
           (not (equal (ecaar listofallatoms) (ecadar listofallatoms)))) T NIL)
  (progn ; (terpri) (print "CLAUSE 30 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair (car listofallatoms) (cons (car firstpair) (ecaar listofallatoms))
      firstpairseen T thirdpairseen inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - SECOND PAIR SEEN (pair and pair, third possibility vic-ana-vic):
  ((if (and (equal NIL secondpairseen) (pair firstpair) (listq (car listofallatoms))
           (equal (car firstpair) (ecadar listofallatoms))
           (not (equal (cdr firstpair) (ecaar listofallatoms)))
           (not (equal (car firstpair) (cdr firstpair)))
           (not (equal (ecaar listofallatoms) (ecadar listofallatoms)))) T NIL)
  (progn ; (terpri) (print "CLAUSE 31 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair (car listofallatoms) (cons (ecaar listofallatoms) (cdr firstpair))
      firstpairseen T thirdpairseen inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - SECOND PAIR SEEN (pair and pair, fourth possibility vic-ana-vic):
  ((if (and (equal NIL secondpairseen) (pair firstpair) (listq (car listofallatoms))
           (equal (cdr firstpair) (ecaar listofallatoms))
           (not (equal (car firstpair) (ecadar listofallatoms)))
           (not (equal (car firstpair) (cdr firstpair)))
           (not (equal (ecaar listofallatoms) (ecadar listofallatoms)))) T NIL)
  (progn ; (terpri) (print "CLAUSE 32 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair (car listofallatoms) (cons (car firstpair) (ecadar listofallatoms))
      firstpairseen T thirdpairseen inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - SECOND PAIR SEEN (pair and pair, first possibility ana-vic-vic):
  ((if (and (equal NIL secondpairseen) (listq firstpair) (pair (car listofallatoms))
           (equal (car firstpair) (ecaar listofallatoms))
           (not (equal (ecadr firstpair) (ecdar listofallatoms)))
           (not (equal (car firstpair) (ecadr firstpair)))
           (not (equal (ecaar listofallatoms) (ecdar listofallatoms)))) T NIL)
  (progn ; (terpri) (print "CLAUSE 33 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair (car listofallatoms) (cons (ecadr firstpair) (ecdar listofallatoms))
      firstpairseen T thirdpairseen inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - SECOND PAIR SEEN (pair and pair, second possibility ana-vic-vic):
  ((if (and (equal NIL secondpairseen) (listq firstpair) (pair (car listofallatoms))
           (equal (ecadr firstpair) (ecdar listofallatoms))
           (not (equal (car firstpair) (ecaar listofallatoms)))
           (not (equal (car firstpair) (ecadr firstpair)))
           (not (equal (ecaar listofallatoms) (ecdar listofallatoms)))) T NIL)
  (progn ; (terpri) (print "CLAUSE 34 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair (car listofallatoms) (cons (ecaar listofallatoms) (car firstpair))
      firstpairseen T thirdpairseen inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - SECOND PAIR SEEN (pair and pair, third possibility ana-vic-vic):
  ((if (and (equal NIL secondpairseen) (listq firstpair) (pair (car listofallatoms))
           (equal (car firstpair) (ecdar listofallatoms))
           (not (equal (ecadr firstpair) (ecaar listofallatoms)))
           (not (equal (car firstpair) (ecadr firstpair)))
           (not (equal (ecaar listofallatoms) (ecdar listofallatoms)))) T NIL)
  (progn ; (terpri) (print "CLAUSE 35 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair (car listofallatoms) (cons (ecaar listofallatoms) (ecadr firstpair))
      firstpairseen T thirdpairseen inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - SECOND PAIR SEEN (pair and pair, fourth possibility ana-vic-vic):
  ((if (and (equal NIL secondpairseen) (listq firstpair) (pair (car listofallatoms))
           (equal (ecadr firstpair) (ecaar listofallatoms))
           (not (equal (car firstpair) (ecdar listofallatoms)))
           (not (equal (car firstpair) (ecadr firstpair)))
           (not (equal (ecaar listofallatoms) (ecdar listofallatoms)))) T NIL)
  (progn ; (terpri) (print "CLAUSE 36 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair (car listofallatoms) (cons (car firstpair) (ecdar listofallatoms))
      firstpairseen T thirdpairseen inconclusives (cdr listofallatoms) atomschecked)))

  ; RECALL - SECOND PAIR CONTRADICTED ERE USED FOR A THIRD ATOM (pair):
  ; this is not a termination reason, as the hypothesis of the first atom was not per se being contradicted:
  ((if (and (equal T secondpairseen) (equal NIL thirdpairseen) (pair secondpair)
           (or (and (pair (car listofallatoms)) (equal (car listofallatoms) (cons (cdr secondpair) (car secondpair))))
               (and (listq (car listofallatoms))
                    (or (equal (car listofallatoms) (list (car secondpair) (cdr secondpair)))
                        (equal (car listofallatoms) (list (cdr secondpair) (car secondpair))))))) T NIL)
  (progn ; (terpri) (print "CLAUSE 37 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair 'INCOGNITUM 'INCOGNITUM firstpairseen NIL NIL inconclusives (cdr listofallatoms)
      (cons secondpair (cons (car listofallatoms) atomschecked)))))
 ; the second pair will end up next to and behind its contradiction

  ; RECALL - SECOND PAIR CONTRADICTED ERE USED FOR A THIRD ATOM (list):
  ; this is not a termination reason, as the hypothesis of the first atom was not per se being contradicted:
  ((if (and (equal T secondpairseen) (equal NIL thirdpairseen) (listq secondpair)
           (pair (car listofallatoms))
           (or (equal (car listofallatoms) (cons (car secondpair) (ecadr secondpair)))
               (equal (car listofallatoms) (cons (ecadr secondpair) (car secondpair))))) T NIL)
  (progn ; (terpri) (print "CLAUSE 38 ") (print " FIRST: ") (print firstpair) (print "  SECOND: ") (print secondpair) (print "  THIRD: ") (print thirdpair) (print "  CAR_L: ") (print (car listofallatoms)) (terpri)
    (proto-eval-triangle firstpair 'INCOGNITUM 'INCOGNITUM firstpairseen NIL NIL inconclusives (cdr listofallatoms)
      (cons secondpair (cons (car listofallatoms) atomschecked)))))
 ; the second pair will end up next to and behind its contradiction

  ; RECALL - NOTHING OF INTEREST HAPPENED:
  (T
  (proto-eval-triangle firstpair secondpair thirdpair firstpairseen secondpairseen thirdpairseen inconclusives
    (cdr listofallatoms) (cons (car listofallatoms) atomschecked))))))

(defun eval-triangle (firstpair listofallatoms)
  (if (equal (car firstpair) (cdr firstpair))
    (extractpair firstpair listofallatoms NIL)
    (proto-eval-triangle firstpair 'INCOGNITUM 'INCOGNITUM NIL NIL NIL NIL listofallatoms NIL)))

; The sample calls were mainly inspired by the victri-predecessor:
; sample calls (non-exhaustive):

; positive triangulation 1:
; (eval-triangle '(a . b) '((x . y) (b . c) (a . c) (y . z) (q . r) (a . b)))
; --> (T (A . B) (B . C) (A . C) (X . Y) (Y . Z) (Q . R) (NIHIL . NIHIL))

; negative triangulation 1:
; (eval-triangle '(a . b) '((x . y) (b . c) (a . b) (c . a) (y . z) (q . r)))
; --> (NIL (X . Y) (C . A) (Y . Z) (Q . R) (A . C) (B . C) (A . B))

; positive triangulation 2:
; (eval-triangle '(a . b) '((x . y) (z . a) (a . b) (z . b) (y . z) (q . r)))
; --> (T (A . B) (Z . A) (Z . B) (X . Y) (Y . Z) (Q . R) (NIHIL . NIHIL))

; negative triangulation 2:
; (eval-triangle '(a . b) '((x . y) (z . a) (a . b) (b . z) (y . z) (q . r)))
; --> (NIL (X . Y) (B . Z) (Y . Z) (Q . R) (Z . B) (Z . A) (A . B))

; positive triangulation 3:
; (eval-triangle '(a . b) '((x . y) (b c) (a . c) (y . z) (q . r) (a . b)))
; --> (T (A . B) (B C) (A . C) (X . Y) (Y . Z) (Q . R) (NIHIL . NIHIL))

; negative triangulation 3:
; (eval-triangle '(a . b) '((x . y) (b c) (a c) (y . z) (q . r) (a . b)))
; --> (NIL (X . Y) (A C) (Y . Z) (Q . R) (A . B) (A . C) (B C))

; positive triangulation 4:
; (eval-triangle '(a b) '((x . y) (b . c) (a . c) (y . z) (q . r) (a b)))
; --> (T (A B) (B . C) (A . C) (X . Y) (Y . Z) (Q . R) (NIHIL . NIHIL))

; negative triangulation 3:
; (eval-triangle '(a b) '((x . y) (b . c) (a c) (y . z) (q . r) (a b)))
; --> (NIL (X . Y) (A C) (Y . Z) (Q . R) (A B) (A . C) (B . C))

; positive triangulation 5:
; (eval-triangle '(a b) '((x . y) (b c) (a c) (y . z) (q . r) (a b)))
; --> (T (A B) (B C) (A C) (X . Y) (Y . Z) (Q . R) (NIHIL . NIHIL))

; negative triangulation 5:
; (eval-triangle '(a b) '((x . y) (b . c) (c . a) (y . z) (q . r) (a b)))
; --> (NIL (X . Y) (C . A) (Y . Z) (Q . R) (A B) (A . C) (B . C))

; conclusion 1:
; (eval-triangle '(a . b) '((x . y) (b . c) (a . b) (e . f) (y . z) (q . r)))
; --> (T (A . B) (B . C) (A . C) (X . Y) (E . F) (Y . Z) (Q . R))

; conclusion 2:
; (eval-triangle '(a . b) '((x . y) (z . a) (a . b) (e . f) (y . z) (q . r)))
; --> (T (A . B) (Z . A) (Z . B) (X . Y) (E . F) (Y . Z) (Q . R))

; conclusion 3:
; (eval-triangle '(a . b) '((x . y) (b . c) (a . c) (e . f) (y . z) (q . r)))
; --> (T (A . B) (B . C) (A . C) (X . Y) (E . F) (Y . Z) (Q . R))

; conclusion 4:
; (eval-triangle '(a . b) '((x . y) (z . a) (z . b) (e . f) (y . z) (q . r)))
; --> (T (A . B) (Z . A) (Z . B) (X . Y) (E . F) (Y . Z) (Q . R))

; conclusion 5:
; (eval-triangle '(a . b) '((x . y) (b c) (a . b) (e . f) (y . z) (q . r)))
; --> (T (A . B) (B C) (A . C) (X . Y) (E . F) (Y . Z) (Q . R))

; conclusion 6:
; (eval-triangle '(a b) '((x . y) (b . c) (a b) (e . f) (y . z) (q . r)))
; --> (T (A B) (B . C) (A . C) (X . Y) (E . F) (Y . Z) (Q . R))

; conclusion 7:
; (eval-triangle '(a . b) '((x . y) (a . c) (a . b) (e . f) (y . z) (q . r)))
; --> (T (A . B) (A . C) (B C) (X . Y) (E . F) (Y . Z) (Q . R))

; conclusion 8:
; (eval-triangle '(a b) '((x . y) (b c) (a b) (e . f) (y . z) (q . r)))
; --> (T (A B) (B C) (A C) (X . Y) (E . F) (Y . Z) (Q . R))

; counter-conclusion 1:
; (eval-triangle '(a . b) '((x . y) (b . c) (b . a) (e . f) (y . z) (q . r)))
; --> (NIL (X . Y) (B . A) (E . F) (Y . Z) (Q . R) (B . C) (A . B))

; counter-conclusion 2:
; (eval-triangle '(a . b) '((x . y) (z . a) (b . a) (e . f) (y . z) (q . r)))
; --> (NIL (X . Y) (B . A) (E . F) (Y . Z) (Q . R) (Z . A) (A . B))

; counter-conclusion 3:
; (eval-triangle '(a . b) '((x . y) (b . c) (c . a) (e . f) (y . z) (q . r)))
; --> (NIL (X . Y) (C . A) (E . F) (Y . Z) (Q . R) (A . C) (B . C))

; counter-conclusion 4:
; (eval-triangle '(a . b) '((x . y) (z . a) (b . z) (e . f) (y . z) (q . r)))
; --> (NIL (X . Y) (B . Z) (E . F) (Y . Z) (Q . R) (Z . B) (Z . A))

; counter-conclusion 5:
; (eval-triangle '(a . b) '((x . y) (b c) (a b) (e . f) (y . z) (q . r)))
; --> (NIL (X . Y) (A B) (E . F) (Y . Z) (Q . R) (B C) (A . B))

; counter-conclusion 6:
; (eval-triangle '(a b) '((x . y) (b . c) (c . a) (e . f) (y . z) (q . r)))
; --> (NIL (X . Y) (C . A) (E . F) (Y . Z) (Q . R) (A . C) (B . C))

; counter-conclusion 7:
; (eval-triangle '(a . b) '((x . y) (a c) (a b) (e . f) (y . z) (q . r)))
; --> (NIL (X . Y) (A B) (E . F) (Y . Z) (Q . R) (A C) (A . B))

; counter-conclusion 8:
; (eval-triangle '(a b) '((x . y) (b . c) (a . b) (e . f) (y . z) (q . r)))
; --> (NIL (X . Y) (A . B) (E . F) (Y . Z) (Q . R) (B . C) (A B))

; inconclusive - A-B is proposed, but cannot be proven in any way:
; (eval-triangle '(a . b) '((x . y) (g . h) (f . g) (e . f) (y . z) (q . r)))
; --> (INCOGNITUM (A . B) (X . Y) (G . H) (F . G) (E . F) (Y . Z) (Q . R))

; inconclusive - A=B is proposed, but cannot be proven in any way:
; (eval-triangle '(a b) '((x . y) (g . h) (f . g) (e . f) (y . z) (q . r)))
; --> (INCOGNITUM (A B) (X . Y) (G . H) (F . G) (E . F) (Y . Z) (Q . R))

; inconclusive - vicinal combination possible, but proposal unknown:
; (eval-triangle '(a . b) '((x . y) (g . h) (b . c) (e . f) (y . z) (q . r)))
; --> (INCONGITUM (A . B) (B . C) (X . Y) (G . H) (E . F) (Y . Z) (Q . R))

; inconclusive - analogous combination possible, but proposal unknown:
; (eval-triangle '(a b) '((x . y) (g . h) (b . c) (e . f) (y . z) (q . r)))
; --> (INCONGITUM (A B) (B . C) (X . Y) (G . H) (E . F) (Y . Z) (Q . R))

; direct confirmation 1:
; (eval-triangle '(a . b) '((x . y) (e . f) (a . b) (b . a) (y . z) (q . r)))
; --> (T (A . B) (X . Y) (E . F) (B . A) (Y . Z) (Q . R) (NIHIL . NIHIL))

; direct confirmation 2:
; (eval-triangle '(a b) '((x . y) (e . f) (a b) (b . a) (y . z) (q . r)))
; --> (T (A B) (X . Y) (E . F) (B . A) (Y . Z) (Q . R) (NIHIL . NIHIL))

; direct confirmation 3:
; (eval-triangle '(a b) '((x . y) (e . f) (b a) (b . a) (y . z) (q . r)))  
; --> (T (A B) (X . Y) (E . F) (B . A) (Y . Z) (Q . R) (NIHIL . NIHIL))

; the atom doubling in the end is not good:

; direct contradiction 1:
; (eval-triangle '(a . b) '((x . y) (e . f) (b . a) (q . e) (y . z) (a . b)))
; --> (NIL (X . Y) (E . F) (B . A) (Q . E) (Y . Z) (A . B) (A . B))

; direct contradiction 2:
; (eval-triangle '(a . b) '((x . y) (b . c) (b . a) (q . e) (y . z) (a . b)))
; --> (NIL (X . Y) (B . A) (Q . E) (Y . Z) (A . B) (B . C) (A . B))

; direct contradiction 3:
; (eval-triangle '(a . b) '((x . y) (b . c) (a . c) (b . a) (y . z) (a . b)))
; --> (NIL (X . Y) (B . A) (Y . Z) (A . B) (A . C) (B . C) (A . B))

; direct contradiction 4:
; (eval-triangle '(a . b) '((x . y) (e . f) (b a) (q . e) (y . z) (a . b)))
; --> (NIL (X . Y) (E . F) (B A) (Q . E) (Y . Z) (A . B) (A . B))

; direct contradiction 5:
; (eval-triangle '(a b) '((x . y) (e . f) (a . b) (q . e) (y . z) (a b)))
; --> (NIL (X . Y) (E . F) (A . B) (Q . E) (Y . Z) (A B) (A B))

; say that a list must have at least 3 elements to be considered for
; triangulation (minimum for recognising three known triangle sides):
(defun notlongenough (somelist)
(if (or
  (null somelist)
  (null (ecdr somelist))
  (null (ecddr somelist)))
  T NIL))
; sample calls:
; (notlongenough '(a b)) --> T
; (notlongenough '(a b c)) --> NIL
; later on, I will use cddddr, i.e. one "cdr" more, because I know that the list
; will have a #t, #f or () attached in front.

; OPERATION SNOWFLAKE:
; As an improvement to an earlier version, not only a term is evaluated,
; but also each of the participants in its triangulations is evaluated.
; I.e., each of the two other sides now tries to reach further
; conclusions. - That means that inheritance of conclusions is possible
; even if not directly driven by input. (That might be too far-reaching
; and too "fanciful", which is why the old version above is kept for
; reference.) - I am calling this figure a "snowflake" as further
; triangles may thus emerge from the sides of triangles.

; Relying only on the first pair for confirming a term is possible, but
; other choices are possible, too. Relying on the first pair bases
; reasoning on the basis of "most recent experience". But if one were
; to base reasoning on the "most consistent recent experience", one
; should look at further combination possibilities of a given term -
; namely, HOW MANY positive conclusions can be made without
; hitting a contradiction. This is done by re-evaluating a term on the
; list of all atoms "apart from" positive previous conclusions.

(defun proto1-re-eval (firstpair listofallatoms congruentpairs counter)
  (if (notlongenough listofallatoms)
    congruentpairs ; this line is - it delivers which congruent pairs to re-evaluate.
    (let ((evaltri (eval-triangle firstpair listofallatoms)))
      (if (equal NIL (car evaltri)) ; we hit a wrong combination - output the transformed list so far
        congruentpairs
        (if (null (car evaltri)) ; we hit an inconclusive combination
          congruentpairs
          (proto1-re-eval firstpair (cddddr evaltri) (cons (cadddr evaltri) (cons (caddr evaltri) congruentpairs)) (+ 1 counter)))))))

; Deriving conclusions has the positive effect of extending consistent
; knowledge - but it also has the negative effect of displacing old
; knowledge into forgetting. - This second effect becomes problematic
; if very many conclusions are reached, and therefore, the below
; function terminates reasoning if the "global limit" of conclusions
; has been reached. (If that number is negative, there is no limit.)
; The global limit of conclusions not only limits the conclusions of a
; given couple of atoms, it also limits how many "consequential"
; evaluations may be undertaken, namely globallimit^N, where currently
; N=2. (The initial pair triggering N conclusions, and each of these
; triggering further N conclusions.)

; NOW _THIS_ IS FUN: I HAVE DIFFERENT CONSEQUENCES FOR #f & NIL!
; I HANDLED THE "#f" as "NIL" and the "'()" as "'INCOGNITUM".

; THUNK: GLOBAL LIMIT ENABLED IN THE SECOND LINE. Otherwise exactly the same as above.

(defun proto2-re-eval (firstpair listofallatoms congruentpairs counter)
  (if (or (zerop globallimit) (notlongenough listofallatoms))
    (cons counter (cons firstpair (append (reverse congruentpairs) listofallatoms)))
    (let ((evaltri (eval-triangle firstpair listofallatoms)))
      (if (equal NIL (car evaltri)) ; we hit a wrong combination - output the transformed list so far
        (cons (- counter 1) (cons firstpair (append (reverse congruentpairs) (reverse (cdr (reverse (cdr evaltri)))))))
        (if (equal 'INCOGNITUM (car evaltri)) ; we hit an inconclusive combination
          (cons counter (cons firstpair (append (reverse congruentpairs) (cddr evaltri))))
          (proto2-re-eval firstpair (cddddr evaltri) (cons (cadddr evaltri) (cons (caddr evaltri) congruentpairs)) (+ 1 counter)))))))

; the congruent pairs are delivered in a back-to-front way:
(defun proto3-re-eval (firstpair congruentpairs counter listofallatoms)
  (if (null congruentpairs)
    (reverse (cdr (reverse (proto2-re-eval firstpair listofallatoms '() counter)))) ; (cons counter listofallatoms)
    (let ((reeval (reverse (cdr (reverse (proto2-re-eval (car congruentpairs) listofallatoms '() counter))))))
      (proto3-re-eval firstpair (cdr congruentpairs) (+ counter (car reeval)) (cdr reeval))))) ; making this not reverse cdr reverse reeval keeps the length.

; Basically, the idea is that not a single pair is re-evaluated, but
; all pairs that single pair generates. For this, first the congruent
; pairs are established in in proto1-re-eval. Then proto3-re-eval is
; feeding each of these pairs to the original list of all atoms (i.e.
; not the evaluated one in proto1-re-eval) by means of proto2-re-eval.
; When done, it spits out the last evaluation.

; Old version, just once following the conclusions - can be enabled any
; time instead of the other functions:
; (defun re-eval (firstpair listofallatoms)
;   (let ((re-ev (proto1-re-eval firstpair listofallatoms '() 1))) ; "1" means minimum 0 in the result.
;     (proto3-re-eval firstpair re-ev 1 listofallatoms)))

; Erasedup is an UGLY KLUDGE that should be eliminated in order to get
; the below functions to work (not needed for try 4):
(defun proto-erasedup (somelist resultlist)
  (if (null somelist) (reverse resultlist)
    (if (or (and (listq (car somelist))
                 (not (member (car somelist) resultlist))
                 (not (member (reverse (car somelist)) resultlist)))
            (and (pair (car somelist))
                 (not (member (car somelist) resultlist))))
      (proto-erasedup (cdr somelist) (cons (car somelist) resultlist))
        (proto-erasedup (cdr somelist) resultlist))))

(defun erasedup (somelist) (proto-erasedup somelist '()))

(defun build-conclusions-list (listofconclusions listofallatoms resultlist)
  (if (null listofconclusions) ; the previous variant "(cdr resultlist)"
    (if (not (null resultlist)) (cdr resultlist) '()) ; caused an error on an empty list
    (build-conclusions-list (cdr listofconclusions) listofallatoms
      (append resultlist (proto1-re-eval (car listofconclusions) listofallatoms '() 1)))))

(defun proto-get-conclusions (snowdepth listofallatoms resultconclusions)
  (if (zerop snowdepth) resultconclusions
    (let ((build-conc (build-conclusions-list resultconclusions listofallatoms '())))
      (if (null build-conc) ; then you are unable to re-reason and you may terminate faster
        resultconclusions ; otherwise, continue searching conclusions:
        (proto-get-conclusions (- snowdepth 1) listofallatoms (append resultconclusions build-conc))))))

(defun get-conclusions (firstpair listofallatoms)
  (proto-get-conclusions snow-depth listofallatoms (list firstpair)))

(defun re-eval (firstpair listofallatoms)
  (let ((re-ev (get-conclusions firstpair listofallatoms))) ; "1" means minimum 0 in the result.
;     (begin (display "re-ev: ") (display re-ev) (display #\newline)
        ; THIS CAN BE REPLACED:
    (proto3-re-eval firstpair (cdr (erasedup re-ev)) 1 listofallatoms)))
;       EXTREME DEPTH CAN BE GAINED IF NOT USING ERASEDUP, BUT THE
;       CONCLUSIONS COUNT RISES AWFULLY - FROM 126 TO 274953538178:
;       (proto3-re-eval firstpair (cdr re-ev) 1 listofallatoms)))
; )

; Rather than a global limit, the above function could contain a local
; limit - I do not _need_ to supply the _entire_ re-eval to
; proto3-re-eval, I could limit it to its first M conclusions.
; M does not have to be the same as N above, it could be e.g. N^2.
; So far, everything is symmetrically regulated by the globallimit.

; sample calls:

; (re-eval '(a . b) '((x . y) (q . r) (b . c) (c . a) (s . t) (b . d) (d . a) (l . m) (n . o) (o . p) (p . q) (q . r)))
; --> (0 (A . B) (X . Y) (Q . R) (C . A) (S . T) (B . D) (D . A) (L . M) (N . O) (O . P) (P . Q) (Q . R))

; (re-eval '(a . b) '((x . y) (q . r) (s . t) (l . m) (n . o) (o . p) (p . q) (q . r) (r . s)))
; --> (1 (A . B) (X . Y) (Q . R) (S . T) (L . M) (N . O) (O . P) (P . Q) (Q . R))

; (re-eval '(a . b) '((x . y) (b . c) (a . c) (q . r) (s . t) (l . m) (n . o) (o . p) (p . q) (q . r) (r . s)))
; --> (2 (A . B) (B . C) (A . C) (X . Y) (Q . R) (S . T) (L . M) (N . O) (O . P) (P . Q) (Q . R))

; (re-eval '(a . b) '((x . y) (b . c) (a . c) (b . d) (a . d) (l . r) (s . t) (l . m) (n . o) (o . p) (p . q) (q . r) (r . s)))
; --> (3 (A . B) (B . C) (A . C) (B . D) (A . D) (X . Y) (L . R) (S . T) (L . M) (N . O) (O . P) (P . Q) (Q . R))

; (re-eval '(a . b) '((x . y) (b . c) (a . c) (b . d) (a . d) (b . e) (e . a) (l . r) (s . t) (l . m) (n . o) (o . p) (p . q) (q . r) (r . s)))
; --> (2 (A . B) (B . C) (A . C) (B . D) (A . D) (X . Y) (E . A) (L . R) (S . T) (L . M) (N . O) (O . P) (P . Q) (Q . R) (R . S))

; (re-eval '(a . b) '((x . y) (b . c) (a . c) (b . d) (a . d) (b . e) (e . a) (b . f) (f . a) (q . r) (s . t) (l . m) (n . o) (o . p) (p . q) (q . r) (r . s)))
; --> (2 (A . B) (B . C) (A . C) (B . D) (A . D) (X . Y) (E . A) (B . F) (F . A) (Q . R) (S . T) (L . M) (N . O) (O . P) (P . Q) (Q . R) (R . S))

; (re-eval '(a . b) '((x . y) (b . c) (a . c) (b . e) (e . a) (b . f) (f . a) (q . r) (s . t) (l . m) (n . o) (o . p) (p . q) (q . r) (r . s)))
; --> (1 (A . B) (B . C) (A . C) (X . Y) (E . A) (B . F) (F . A) (Q . R) (S . T) (L . M) (N . O) (O . P) (P . Q) (Q . R) (R . S))

; (re-eval '(a . b) '((x . y) (b . e) (e . a) (b . f) (f . a) (q . r) (s . t) (l . m) (n . o) (o . p) (p . q) (q . r) (r . s)))
; --> (0 (A . B) (X . Y) (E . A) (B . F) (F . A) (Q . R) (S . T) (L . M) (N . O) (O . P) (P . Q) (Q . R) (R . S))

; END OF OPERATION SNOWFLAKE.

; Now - remove all duplicates and say how often you saw any unicate.
; Triangulation should be undertaken on each input unit only once.
; However, triangulation merely shows "confirmed observations" -
; actually, observations may be confirmed also from how often they are seen in input.
(defun proto1-erase-duplicates (unicate listofpairs counter resultlist)
  (if (null listofpairs) (cons (cons counter unicate) (reverse resultlist))
    (if (equal unicate (car listofpairs))
      (proto1-erase-duplicates unicate (cdr listofpairs) (+ 1 counter) resultlist)
      (proto1-erase-duplicates unicate (cdr listofpairs) counter (cons (car listofpairs) resultlist)))))

(defun proto2-erase-duplicates (listofunicates listofpairs resultlist)
  (if (null listofunicates) (reverse resultlist)
    (let ((proto1 (proto1-erase-duplicates (car listofunicates) listofpairs 0 '())))
    (proto2-erase-duplicates (cdr proto1) (cdr proto1)
      (cons (car proto1) resultlist)))))

(defun erase-duplicates (listofpairs) (proto2-erase-duplicates listofpairs listofpairs '()))

; sample call:
; (erase-duplicates '((a . b) (a . b) (b . c) (c . d) (a . b) (c . d)))
; --> ((3 A . B) (1 B . C) (2 C . D))
; I could now MULTIPLY that with triangulation results...
; or I could merely ADD it, and ADDING IT would be more interesting if
; the results could be NEGATIVE. But NEGATIVE results would only
; signify ABSOLUTE KNOWLEDGE rather than ATTENTION.

(defun proto-compare-tri (pairs listofallatoms bestresult)
  (if (null pairs) bestresult
    (let ((intermediate (re-eval (cdar pairs) listofallatoms)))
      (if (>= (+ (caar pairs) (car intermediate)) (car bestresult))
        (proto-compare-tri (cdr pairs) listofallatoms
          (cons (+ (caar pairs) (car intermediate)) (cdr intermediate)))
        (proto-compare-tri (cdr pairs) listofallatoms bestresult)))))

(defun compare-tri (inputlist listofallatoms)
  (proto-compare-tri (erase-duplicates (mapcons inputlist)) listofallatoms '(-1 ())))

; sample calls:

; (compare-tri '(a b c a b d a b e)
; '((f . g) (a . b) (b . x) (g . h) (b . y) (a . y) (b . z) (z . a)
; (b . h) (a . h) (c . i) (c . j) (k . b) (k . c) (l . b) (l . c)
; (m . b) (m . c) (b . i) (b . j)
; ; (c . a) is unknown
; (d . b)
; (e . n) (n . b)))

; previously in Scheme it used to be:
; --> (5 (a . b) (b . x) (a . x) (b . y) (a . y) (f . g) (g . h)
; (z . a) (b . h) (a . h) (c . i) (c . j) (k . b) (k . c) (l . b)
; (l . c) (m . b) (m . c) (b . i) (b . j) (d . b) (e . n) (n . b))

; now it is:
; --> (13 (B . E) (A . B) (F . G) (B . X) (G . H) (B . Y) (A . Y)
; (B . Z) (Z . A) (B . H) (A . H) (K . B) (C . I) (L . B) (C . J)
; (M . B) (K . C) (B . I) (L . C) (B . J) (M . C) (D . B) (E . N))

; (compare-tri '(a b c a b d a b e)
; '((f . g) (a . b) (b . x) (g . h) (b . y) (a . y) (b . z) (z . a)
; (b . h) (a . h) (c . b) (c . i) (c . j)
; (a . c) ; it tend to conclude
; (a . d) ; each of these three's antitheses if they are not
; (d . b) ; specified, because they are later than "(a . b)"
; (e . n) (n . b)))

; previously in Scheme it used to be:
; --> (5 (a . b) (b . x) (a . x) (b . y) (a . y) (f . g) (g . h)
; (z . a) (b . h) (a . h) (c . b) (c . i) (c . j) (a . c) (a . d)
; (d . b) (e . n) (n . b))

; now it is:
; --> (8 (B . E) (A . B) (F . G) (B . X) (G . H) (B . Y) (A . Y)
; (B . Z) (Z . A) (B . H) (A . H) (C . B) (C . I) (D . B) (C . J)
; (A . C) (A . D) (N . B))

; Now, the foundation of hierarchisation has to be defined. I.e. that
; the system re-considers input, each time combining another pair.

; IN THE PRESENT DESIGN, HIERARCHISATION ALWAYS CONTINUES "TO THE TOP"
; AND DELIVERS "ONE" CONNECTION AS THE ANSWER. - THIS HAPPENS ON EACH
; RE-HIERARCHISATION, TOO. - That might be re-considered as it is
; wasteful of atoms, but on the positive side, it creates a lot of
; variations of one and the same input that can be used in further
; reasoning and thus aids the creation of "areas of knowledge".

(defun proto-hierarchise (chosen carinputlist inputlist resultlist)
  (if (null inputlist) (reverse resultlist)
    (if (equalp chosen (cons carinputlist (car inputlist)))
      (proto-hierarchise chosen (car inputlist) (cdr inputlist) (cons chosen (cdr resultlist)))
      (proto-hierarchise chosen (car inputlist) (cdr inputlist) (cons (car inputlist) resultlist)))))

(defun hierarchise (chosen inputlist)
  (if (null inputlist) '()
    (proto-hierarchise chosen (car inputlist) (cdr inputlist) (list (car inputlist)))))

; (hierarchise '(a . b) '(a b c d a b c d a b))
; --> ((A . B) C D (A . B) C D (A . B))
; (hierarchise '((a . b) . c) (hierarchise '(a . b) '(a b c d a b c d a b)))
; --> (((A . B) . C) D ((A . B) . C) D (A . B))
; (hierarchise '(b . c) '(a b c a b d a b e o))
; --> (A (B . C) A B D A B E O)

; The above function is used in the tri-hier function defined below.

; If input is re-hierarchised after being only partially shifted, i.e.
; if input history is partially "re-considered", that creates multiple
; hierarchisation peaks and contributes to "areas of knowledge" as well
; as giving the system "multiple points of view" on the same "matter".

; ======== THIS MODIFICATION TO SPLITFUSE CAN WORK WITH ANALOGIES ========

; The next function is where a plan is prepared for output. Its
; hierarchy is flattened and the list of sub-atoms constitutes the
; answer. - This function is later used in planning.

(defun proto-splitfuse (pairtree resultlist)
  (if (not (consp pairtree)) (reverse (cons pairtree resultlist))
    (if (not (consp (car pairtree)))
      (proto-splitfuse (cdr pairtree) (cons (car pairtree) resultlist))
      (proto-splitfuse (cons (caar pairtree) (cons (cdar pairtree) (cdr pairtree))) resultlist))))
; sample calls:
; (proto-splitfuse '(((a (b . c) d . e) . f) g . h) '()) --> (A B C D E F G H)
; (proto-splitfuse '(a . b) '()) --> (A B)
; (proto-splitfuse 'a '()) --> (A)

(defun elimnil (somelist resultlist)
  (if (null somelist) resultlist
    (if (equal NIL (car somelist))
      (elimnil (cddr somelist) resultlist)
      (elimnil (cdr somelist) (cons (car somelist) resultlist)))))

(defun splitfuse (pairtree) (elimnil (reverse (proto-splitfuse pairtree '())) '()))
; sample calls - first pure vic, second with anas:
; (splitfuse '(((a (b . c) d . e) . f) g . h)) --> (A B C D E F G H)
; (splitfuse '(((a (b c) d e) . f) g . h)) --> (A B D F G H)


; The next function tries to find a plan as the continuation of the
; present. If the hierarchy has no right neighbour, it is decomposed.
; Then again it is checked whether the right-most part has a right
; neighbour in the list of all atoms, until such right neighbour is
; found - it is this right neighbour that consititutes the plan:

(defun proto-findplan (initialpair listofallatoms original-listofallatoms)
  (if (and (null listofallatoms) (consp initialpair))
    (proto-findplan (cdr initialpair) original-listofallatoms original-listofallatoms)
    (if (null listofallatoms) NIL
      (if (and (consp (car listofallatoms))
          (or (and (consp initialpair) (equal (cdr initialpair) (caar listofallatoms)))
              (equal initialpair (caar listofallatoms))))
        (splitfuse (cdar listofallatoms))
        (proto-findplan initialpair (cdr listofallatoms) original-listofallatoms)))))

(defun findplan (initialpair listofallatoms) (proto-findplan initialpair listofallatoms listofallatoms))
; sample calls:
; (findplan '(x . a) '((r . s) (s . t) (t . u) (u . a) (a . v) (a . w) (u . v))) --> (V)
; (findplan '(y x . a) '((r . s) (s . t) (t . u) (u . a) (a v . w) (a . w) (u . v))) --> (V W)

; debugging version, delivering not a plan, but the problematic
; listofallatoms:
; (defun tri-hier (inputlist listofallatoms)
; (progn (princ "inputlist: ") (princ inputlist) (terpri) (terpri)
;        (princ "listofallatoms: ") (princ listofallatoms) (terpri)
;        (princ "length of listofallatoms: ") (princ (length listofallatoms)) (terpri) (terpri)
;   (if (equal? 1 (length inputlist)) (findplan inputlist listofallatoms) ; (cons inputlist listofallatoms)
;     (let ((tri-knowledge (compare-tri inputlist listofallatoms)))
;       (tri-hier (hierarchise (cadr tri-knowledge) inputlist) (cdr tri-knowledge))))))

; Now it is time to "triangulate and hierarchise", i.e. combining
; the previous functions into a challenge-response package. The answer
; of that function contains both the string of the plan and a changed
; version of the list of all atoms (changes according to reasoning).

(defun tri-hier (inputlist listofallatoms)
  (if (equal 1 (length inputlist)) (cons (findplan (car inputlist) listofallatoms) listofallatoms) ; inputlist
    (let ((tri-knowledge (compare-tri inputlist listofallatoms)))
      (tri-hier (hierarchise (cadr tri-knowledge) inputlist) (cdr tri-knowledge)))))

; sample call:
; (tri-hier '(a b c a b e) '((e n o p . q) (f . g)
; (a . b) (b . x) (g . h) (b . y) (a . y) (b . z) (z . a)
; (b . h) (a . h) (c . i) (c . j) (k . b) (k . c) (l . b)
; (l . c) (m . b) (m . c) (b . i) (b . j) (d . b) (n . b)))
; previously in Scheme:
; --> ((n o p q) ((a . b) c (a . b) . e) ((a . b) . e) (c (a . b) . e)
; (c . i) (e n o p . q) (a . b) (b . x) (a . x) (b . y) (a . y) (f . g)
; (g . h) (z . a) (b . h) (a . h) (c . j) (k . b) (k . c) (l . b)
; (l . c) (m . b) (m . c) (b . i))
; now in Lisp:
; ((N O P Q) ((A B . C) A B . E) (A B . C) (A B . E)
; (A . B) (A . Y) (B . C) (Z . A) (B . E) (E N O P . Q)
; (F . G) (B . X) (B . Y) (B . Z) (G . H) (B . H) (K . B) (K . C) (L . B)
; (L . C) (C . I) (B . I) (M . B) (M . C))


; ---- EXPERIMENTAL STAGE: MAKE IT "LEARN A TEXT", THEN TALK ABOUT IT!

; defun a global list of all atoms, "empty" for now
(defun proto-protolist (x resultlist)
  (if (zerop x) resultlist ; no need to reverse, it is nonsense anyway
    (proto-protolist (- x 1) (cons '(nihil . nihil) resultlist))))

; THUNK: The below function defuns the list of all atoms according to
; the globally set variable knowledgesize.
; (defun protolist (proto-protolist 65000 '())) ; a first experiment with 65k atoms.

(defvar protolist (proto-protolist knowledgesize '()))

; This is where you require the "filling" of the knowledge base before
; even "elementary" teaching;
; this also determines the length of the list of all atoms.

; For interactive input, always the last N atoms will be reasoned on:
(defun proto-takelastn (counter somelist resultlist)
  (if (or (zerop counter) (null somelist)) resultlist
    (proto-takelastn (- counter 1) (cdr somelist) (cons (car somelist) resultlist))))

; OPERATION SLEDGE:

; It is possible that repeated hierarchisations uncover formerly
; unknown combination possibilities - this is why the input should be
; re-considered to a certain history length, and moreover, each chunk
; of history should be considered several times. This is implemented
; below. takelastn determines how much of the history is to be re-
; considered while re-slider defines the re-consideration of history.

(defun takelastn (counter somelist) (proto-takelastn counter (reverse somelist) '()))
; sample call:
; (takelastn 3 '(a b c d e f)) --> (d e f)

(defun re-slider (howoften inputlist listofallatoms)
  (if (zerop howoften) (tri-hier inputlist listofallatoms)
    (re-slider (- howoften 1) inputlist (cdr (tri-hier inputlist listofallatoms)))))

; END OF OPERATION SLEDGE.

(defvar mainlist '())
(defvar maininput '())
(defvar tempmain '())
(defvar newinput '())

; This function is used to "learn" an input list - without generating
; a reply, i.e. only the list of all atoms is changed. This is used
; for pre-setting knowledge into the system.

(defun learn (inputlist listofallatoms)
  (if (equal 1 (length inputlist)) listofallatoms ; (cons (findplan (car inputlist) listofallatoms) listofallatoms)
    (let ((tri-knowledge (compare-tri inputlist listofallatoms)))
      (learn (hierarchise (cadr tri-knowledge) inputlist) (cdr tri-knowledge)))))

(defun recursivelearn (listofinputlists listofallatoms)
  (if (null listofinputlists) (setq mainlist listofallatoms)
    (recursivelearn (cdr listofinputlists) (learn (car listofinputlists) listofallatoms))))

; This is where the actual pre-learning takes place:
; (recursivelearn initlist protolist)
; (setq rootlist '()) ; free memory of the rootlist after pre-learning.

; make sure the history of past input is within limits:
(defun proto-takefirstn (counter x resultlist)
  (if (or (zerop counter) (null x)) (reverse resultlist)
    (proto-takefirstn (- counter 1) (cdr x) (cons (car x) resultlist))))

(defun takefirstn (howmany x) (proto-takefirstn howmany x '()))
; sample call:
; (takefirstn 3 '(a b c d e f)) --> (a b c)
; (takefirstn 3 '(a b)) --> (a b)

; this function tests whether each element of the first list
; can be found in the second list. I shall use this to
; determine whether the machine is able to be used for a specific
; input or whether such input would be unknown to it.
(defun allinsecond (firstlist secondlist)
  (if (null firstlist) T
    (if (null (member (car firstlist) secondlist)) NIL
      (allinsecond (cdr firstlist) secondlist))))
; sample calls:
; (allinsecond '(a b a c a) '(a b c)) --> T
; (allinsecond '(a b x c a) '(a b c)) --> NIL

; THIS IS A THUNK.
; It updates the past input history for future calls.
(defun mergehistory (newinput knownhistory)
  (takefirstn historylength (append newinput knownhistory)))

(defun shallitrun (readinsist newinput knownhistory)
  (if (equal readinsist '(DORUN)) T
    (allinsecond newinput knownhistory)))

; setup instincts - "must have" in reality, but
; neither the Scheme version nor the Fortran version have them.
(defun word-instinct (word)
    (cond ((equal word 'I) 'YOU)
          ((equal word 'ME) 'YOU)
          ((equal word 'YOU) 'ME)
          ((equal word 'AM) 'ARE)
          ((equal word 'ARE) 'AM-ARE)
          ((equal word 'MINE) 'YOURS)
          ((equal word 'YOURS) 'MINE)
          ((equal word 'MY) 'YOUR)
          ((equal word 'YOUR) 'MY)
          ((equal word 'MYSELF) 'YOURSELF)
          ((equal word 'YOURSELF) 'MYSELF)
          ((equal word 'WAS) 'WAS-WERE)
          (T word)))

(defun proto-apply-instincts (sentence checked-sentence)
    (cond ((null sentence)
            (reverse checked-sentence))
          (T
            (proto-apply-instincts
                (cdr sentence)
                (cons (word-instinct (car sentence)) checked-sentence)))))

(defun apply-instincts (sentence)
    (proto-apply-instincts sentence '()))
; sample call: (apply-instincts '(I WAS HERE TODAY))
; --> (YOU WAS-WERE HERE TODAY)

; The below functions handle batch interaction of the system.
(defvar mainlist '())
(defvar maininput '())
(defvar inputhistory '())
(defvar insistence '())

; The list of input is placed into swarminput.txt,
; the list of all atoms should be in swarmdata.txt,
; and the answer is to be received in swarmoutput.txt.
; swarminsistence tries to find out whether reasoning
; should take place "anyway" (otherwise it will only
; take place if the input can be found in the history)
; for this, insistence must be set to "(DORUN)".
; It is the OS' responsibility to clean up the insistence,
; clean up the input, and handle and clean up the output.
(defun head ()
  (progn
; (print "reading swarmdata.txt")
    (with-open-file (stream "swarmdata.txt")
    (setq mainlist (read stream)))
; (print "reading swarminput.txt")
    (with-open-file (stream "swarminput.txt")
    (setq maininput (apply-instincts (read stream))))
; (print "reading swarminsistence.txt")
    (with-open-file (stream "swarminsistence.txt")
    (setq insistence (read stream)))
; (print "reading swarmhistory.txt")
   ; (setq inputhistory '())
   (with-open-file (stream "swarmhistory.txt")
   (setq inputhistory (apply-instincts (read stream))))
; (print "going for trihier")
    (if (null (shallitrun insistence maininput inputhistory)) (quit) ; "then do nothing", else:
      (let ((trihier (re-slider re-slide-count maininput mainlist)))
        (progn
; (print "reading swarmdata.txt")
          (with-open-file (stream "swarmdata.txt" :direction :output :if-exists :supersede)
          (format stream (write-to-string (cdr trihier))))
; (print "reading swarmoutput.txt")
          (with-open-file (stream "swarmoutput.txt" :direction :output :if-exists :supersede)
          (format stream (write-to-string (car trihier))))
; (print "reading swarmhistory.txt")
          (with-open-file (stream "swarmhistory.txt" :direction :output :if-exists :supersede)
          (format stream (write-to-string (mergehistory maininput inputhistory))))
          (quit))))))

; fire up the whole exchange:
; (head) ; Lisp with parentheses, Scheme without parentheses.

(head)

; (quit)

; TO SUM IT UP:
; THE SYSTEM WILL FIRE IF THE INPUT LIST'S ELEMENTAS ARE ALL KNOWN,
; OTHERWISE IT WILL NOT FIRE. THIS CAN BE OVERRIDDEN BY PLACING
; "(DORUN)" IN swarminsistence.txt - THEN THE SYSTEM ALSO RUNS IF THE
; INPUT LIST'S ELEMENTS ARE NOT ALL KNOWN. (OBVIOUSLY, IT THEN CREATES
; NEW ELEMENTS AND COULD RUN ON THAT SAME INPUT AGAIN, EVEN WITHOUT
; (DORUN), BECAUSE NOW IT BECOMES KNOWN INPUT).
