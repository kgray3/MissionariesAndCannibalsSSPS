;---------------------------------------------------------------------------------
; GENERAL INFORMATION
;
; FILE: mc_ssps.l
; DATE: Fall 2020
; LINE: State Space Solver for the Missionaries and Cannibals Problem
;---------------------------------------------------------------------------------
;---------------------------------------------------------------------------------
; PROGRAM DESCRIPTION
;
; This program is a state space problem solver for a classic missionaries and
; cannibls problem. An explicit state space tree is grown in concert with breadth
; first search for a solution.
;---------------------------------------------------------------------------------
;---------------------------------------------------------------------------------
; REPRESENTATIONAL NOTES
;
; Banks are represented as a 3-slot class consisting of
; missionaries, cannibals, and boat.
;
; States are represented as a 2-slot class consisting of
; left-bank and right-bank.
;
; Operators are represented as a 3-slot class consisting of
; a name, a precondition, and a description.
;
; Nodes are represented as a 4-slot class consisting of
; a name, a state, a parent node, and a move (state space operator)
;---------------------------------------------------------------------------------
;---------------------------------------------------------------------------------
; MODELING A BANK
( defclass bank ()
    (
        ( missionaries :accessor bank-missionaries :initarg :missionaries )
        ( cannibals :accessor bank-cannibals :initarg :cannibals )
        ( boat :accessor bank-boat :initarg :boat )
    )
)
( defmethod display ( ( b bank ) )
   ( format t "MISSIONARIES=~A CANNIBALS=~A BOAT=~A~%" ( bank-missionaries b ) ( bank-cannibals b ) ( bank-boat b ) ) 
)
;------------------------------------------------------------------
; MODELLING A STATE
( defclass state ()
    (
        ( left-bank :accessor state-left-bank :initarg :left-bank )
        ( right-bank :accessor state-right-bank :initarg :right-bank )
    )
)
( defmethod display ( ( s state ) )
    ( display ( state-left-bank s ) )
    ( display ( state-right-bank s ) )
    nil
)

( defmethod copy-state ( ( s state ) )
    ( make-instance 'state :left-bank ( state-left-bank s ) :right-bank ( state-right-bank s ) )
)
;------------------------------------------------------------------
; MODELLING A NODE
( defclass node ()
    (
        ( name :accessor node-name :initarg :name )
        ( state :accessor node-state :initarg :state )
        ( parent :accessor node-parent :initarg :parent )
        ( operator :accessor node-operator :initarg :operator )
    )
)
( defmethod display ( ( n node ) )
    ( format t "~A " ( node-name n ) )
    ( if ( not ( rootp n ) )
        ( let ()
            ( format t "~A " ( node-name ( node-parent n ) ) )
            ( display ( node-operator n ) )
        )
    )
    ( terpri )
    ( display ( node-state n ) )
    nil
)
;---------------------------------------------------------------------------------
; MODELING A STATE SPACE OPERATOR
( defclass operator ()
    (
        ( name :accessor operator-name :initarg :name )
        ( precondition :accessor operator-precondition :initarg :precondition )
        ( description :accessor operator-description :initarg :description )
    )
)

;---------------------------------------------------------------------------------
; THE MAIN PROGRAM - argument values of e u x eu ex ux eux will cause tracing
( defmethod mc ( ( trace-instruction symbol ) )
    ( setf *trace-instruction* trace-instruction )
    ( establish-operators )
    ( setup )
    ( solve )
)

( defmethod establish-operators ()

        ( setf *MOVE-CM-LR*
            ( make-instance 'operator
            :name 'MOVE-CM-LR
            :precondition "There are at least one C and M on left-bank."
            :description "Move MC to right-bank." )
        )
        ( setf *MOVE-CC-LR*
            ( make-instance 'operator
            :name 'MOVE-CC-LR
            :precondition "There are at least two C on left-bank."
            :description "Move CC to right-bank." )
        ) 
        ( setf *MOVE-MM-LR*
            ( make-instance 'operator
            :name 'MOVE-MM-LR
            :precondition "There are at least two M on left-bank."
            :description "Move MM to right-bank." )
        )
        ( setf *MOVE-C-LR*
            ( make-instance 'operator
            :name 'MOVE-C-LR
            :precondition "There is at least one C on left-bank."
            :description "Move C to right-bank." )
        )
        ( setf *MOVE-M-LR*
            ( make-instance 'operator
            :name 'MOVE-M-LR
            :precondition "There is at least one M on left-bank."
            :description "Move M to right-bank." )
        )
        ( setf *MOVE-CM-RL*
            ( make-instance 'operator
            :name 'MOVE-CM-RL
            :precondition "There are at least one C and M on right-bank."
            :description "Move MC to left-bank." )
        )
        ( setf *MOVE-CC-RL*
            ( make-instance 'operator
            :name 'MOVE-CC-RL
            :precondition "There are at least two C on right-bank."
            :description "Move CC to left-bank." )
        )        
        ( setf *MOVE-MM-RL*
            ( make-instance 'operator
            :name 'MOVE-MM-RL
            :precondition "There are at least two M on right-bank."
            :description "Move MM to left-bank." )
        )
        ( setf *MOVE-C-RL*
            ( make-instance 'operator
            :name 'MOVE-CM-RL
            :precondition "There is at least one C on right-bank."
            :description "Move C to left-bank." )
        )
        ( setf *MOVE-M-RL*
            ( make-instance 'operator
            :name 'MOVE-M-RL
            :precondition "There is at least one M on right-bank."
            :description "Move M to left-bank." )
        )
    
)

( defmethod display ( ( o operator ) )
   ( format t "~A " ( operator-name o ) )
)
;---------------------------------------------------------------------------------
; SOLVE PERFORMS BREADTH FIRST SEARCH
( defmethod solve ( &aux kids e-node )
    ( if ( member *trace-instruction* '(u eu ux eux) ) ( display-the-unexplored-list ) )
    ( if ( member *trace-instruction* '(x ex ux eux) ) ( display-the-explored-list ) )
    ( cond
        ( ( null *unexplored* )
            ( format t ">>> THERE IS NO SOLUTION.~%" )
            ( return-from solve NIL )
        )
    )
    ( setf e-node ( pop *unexplored* ) )
    ( if ( member *trace-instruction* '(e ex eu eux) ) ( display-the-e-node e-node ) )
    ( cond
        ( ( goalp ( node-state e-node ) )
            ( format t "~%>>> GOT A SOLUTION!" )
            ( display-solution e-node )
        )
        ( ( feast-state-p ( node-state e-node ) )
            ( solve )
        )
        ( ( exploredp ( node-state e-node ) )
            ( solve )
        )
        ( t
            ( push e-node *explored* )
            ( setf kids ( children-of e-node ) )
            ( setf *unexplored* ( append *unexplored* kids ) )
            ( solve )
        )
    )
    nil
)

( defmethod display-solution ( ( e-node node ) )
    ( cond
        (( rootp e-node )
            ( terpri )
        )
        ( t
            ( display-solution ( node-parent e-node ) )
            ( format t "~A~%" ( operator-description ( node-operator e-node ) ) )
        )

    )
    nil
)

( defmethod rootp ( ( e-node node ) )
    ( eq ( node-name e-node ) 'root )
)

( defmethod display-the-unexplored-list ()
    ( format t "~%>>> UNEXPLORED LIST~%" )
    ( display-unexplored-states )
)

( defmethod display-unexplored-states ()
    ( mapcar #'display *unexplored* )
    nil
)

( defmethod display-the-explored-list ()
    ( format t "~%>>> EXPLORED LIST~%" )
    ( display-explored-states )
)

( defmethod display-explored-states ()
    ( mapcar #'display *explored* )
    nil
)

( defmethod display-the-e-node ( (e-node node) )
    ( format t "~%>>> E-NODE~%" )
    ( display e-node )
    nil
)

( defmethod goalp ( ( node-state state ) )
    ( setf right-bank ( state-right-bank node-state ) )
    ( and ( = ( count 'M  ( bank-missionaries right-bank ) ) 3 ) 
        ( = ( count 'C ( bank-cannibals right-bank ) ) 3 ) )
)

( defmethod feast-state-p ( ( node-state state ) ) 
    ( setf right-bank ( state-right-bank node-state ) )
    ( setf left-bank ( state-left-bank node-state ) )
    (cond 
        (( > ( count 'C ( bank-cannibals right-bank ) ) ( count 'M ( bank-missionaries right-bank ) ) )
            t
        )
        (( > ( count 'C ( bank-cannibals left-bank ) ) ( count 'M ( bank-missionaries left-bank ) ) )
            t
        )
        (t
            nil
        )
    )
)

( defmethod exploredp ( ( s state ) )
    ( member s *explored* :key #'node-state :test #'equal-state-p )
 ;best to use member with two key word args -- :key and :test
)

( defmethod equal-state-p ( ( s1 state ) ( s2 state ) )
    (and
        ( equal ( state-left-bank s1 ) ( state-left-bank s2 ) )
        ( equal ( state-right-bank s1 ) ( state-right-bank s2 ) )
    )
)
;------------------------------------------------------------------
; THE SETUP
( defmethod setup ( &aux root lb rb istate )
;; establish root node
    ( setf lb ( make-instance 'bank :missionaries '(m m m) :cannibals '(c c c) :boat 'b ) )
    ( setf rb ( make-instance 'bank :missionaries '() :cannibals '() :boat nil ) )
    ( setf istate ( make-instance 'state :left-bank lb :right-bank rb ) )
    ( setf root ( make-instance 'node :state istate :name 'root ) )
;; initialize list of unexplored nodes
    ( setf *unexplored* ( list root ) )
;; initialize list of explored nodes
    ( setf *explored* () )
; get ready to create good names
    ( setf *ng* ( make-instance 'name-generator :prefix "N" ) )
)
;------------------------------------------------------------------
; GENERATING CHILDREN
( defmethod children-of ( (e-node node) &aux kids )
    ( if ( applicablep *MOVE-CM-LR* e-node )
        ( push ( child-of e-node *MOVE-CM-LR* ) kids )
    )
    ( if ( applicablep *MOVE-CC-LR* e-node )
        ( push ( child-of e-node *MOVE-CC-LR* ) kids )
    )
    ( if ( applicablep *MOVE-MM-LR* e-node )
        ( push ( child-of e-node *MOVE-MM-LR* ) kids )
    )
    ( if ( applicablep *MOVE-C-LR* e-node )
        ( push ( child-of e-node *MOVE-C-LR* ) kids )
    )
    ( if ( applicablep *MOVE-M-LR* e-node )
        ( push ( child-of e-node *MOVE-M-LR* ) kids )
    )
    ( if ( applicablep *MOVE-CM-RL* e-node )
        ( push ( child-of e-node *MOVE-CM-RL* ) kids )
    )
    ( if ( applicablep *MOVE-CC-RL* e-node )
        ( push ( child-of e-node *MOVE-CC-RL* ) kids )
    )
    ( if ( applicablep *MOVE-MM-RL* e-node )
        ( push ( child-of e-node *MOVE-MM-RL* ) kids )
    )
    ( if ( applicablep *MOVE-C-RL* e-node )
        ( push ( child-of e-node *MOVE-C-RL* ) kids )
    )
    ( if ( applicablep *MOVE-M-RL* e-node )
        ( push ( child-of e-node *MOVE-M-RL* ) kids )
    )
    kids
)

( defmethod applicablep ( ( op operator ) ( n node ) )
    ( setf right-bank ( state-right-bank ( node-state n ) ) )
    ( setf left-bank ( state-left-bank ( node-state n ) ) )
    ( cond
        (( equal ( bank-boat left-bank ) 'B)
            (cond
                (( eq ( operator-name op ) 'MOVE-CM-LR )
                    (and 
                        ( > ( count 'C ( bank-cannibals left-bank ) ) 0 )
                        ( > ( count 'M ( bank-missionaries left-bank ) ) 0 )
                    )
                )
                (( eq ( operator-name op ) 'MOVE-CC-LR )
                    ( > ( count 'C ( bank-cannibals left-bank ) ) 1 )
                )
                (( eq ( operator-name op ) 'MOVE-MM-LR )
                    ( > ( count 'M ( bank-missionaries left-bank ) ) 1 )
                )
                (( eq ( operator-name op ) 'MOVE-C-LR )
                    ( > ( count 'C ( bank-cannibals left-bank ) ) 0 )
                )
                (( eq ( operator-name op ) 'MOVE-M-LR )
                    ( > ( count 'M ( bank-missionaries left-bank ) ) 0 )
                )
                (t
                    nil
                )
            )
        )
        (t
            (cond
                (( eq ( operator-name op ) 'MOVE-CM-RL )
                    (and 
                        ( > ( count 'C ( bank-cannibals right-bank ) ) 0 )
                        ( > ( count 'M ( bank-missionaries right-bank ) ) 0 )
                    )
                )
                (( eq ( operator-name op ) 'MOVE-CC-RL )
                    ( > ( count 'C ( bank-cannibals right-bank ) ) 1 )
                )
                (( eq ( operator-name op ) 'MOVE-MM-RL )
                    ( > ( count 'M ( bank-missionaries right-bank ) ) 1 )
                )
                (( eq ( operator-name op ) 'MOVE-C-RL )
                    ( > ( count 'C ( bank-cannibals right-bank ) ) 0 )
                )
                (( eq ( operator-name op ) 'MOVE-M-RL )
                    ( > ( count 'M ( bank-missionaries right-bank ) ) 0 )
                )
                (t
                    nil
                )
            )
        )
    )
    
)

( defmethod child-of ( ( n node ) ( o operator ) &aux c )
    ( setf new-node ( make-instance 'node ) )
    ( setf ( node-name new-node ) ( next *ng* ) )
    ( setf ( node-parent new-node ) n )
    ( setf ( node-operator new-node ) o )
    ( setf c ( copy-state ( node-state n ) ) )
    ( apply-operator o c )
    ( setf ( node-state new-node ) c )
    new-node
)

( defmethod apply-operator ( ( o operator ) ( s state ) )
    ( cond 
        (( equal ( operator-name o ) 'MOVE-CM-LR )
            ( setf ( bank-missionaries ( state-left-bank s ) ) ( remove 'M ( bank-missionaries ( state-left-bank s ) ) :count 1 ) )
            ( setf ( bank-cannibals ( state-left-bank s ) ) ( remove 'C ( bank-cannibals ( state-left-bank s ) ) :count 1 ) )
            ( setf ( bank-missionaries ( state-right-bank s ) ) ( append (bank-missionaries ( state-right-bank s )) '(M) ) )
            ( setf ( bank-cannibals ( state-right-bank s ) ) ( append ( bank-cannibals ( state-right-bank s ) ) '(C) ) )
           ( setf ( bank-boat ( state-left-bank s ) ) nil )
           ( setf ( bank-boat ( state-right-bank s ) ) 'B)
        )
        (( equal ( operator-name o ) 'MOVE-CC-LR )
            ( setf ( bank-cannibals ( state-left-bank s ) ) ( remove 'C ( bank-cannibals ( state-left-bank s ) ) :count 2 ) )
            ( setf ( bank-cannibals ( state-right-bank s ) ) ( append ( bank-cannibals ( state-right-bank s ) ) '(C C) ) )
            ( setf ( bank-boat ( state-left-bank s ) ) nil )
            ( setf ( bank-boat ( state-right-bank s ) ) 'B)
        )
        (( equal ( operator-name o ) 'MOVE-MM-LR )
            ( setf ( bank-missionaries ( state-left-bank s ) ) ( remove 'M ( bank-missionaries ( state-left-bank s ) ) :count 2 ) )
            ( setf ( bank-missionaries ( state-right-bank s ) ) ( append ( bank-missionaries ( state-right-bank s ) ) '(M M)))
            ( setf ( bank-boat ( state-left-bank s ) ) nil )
            ( setf ( bank-boat ( state-right-bank s ) ) 'B)
        )
        (( equal ( operator-name o ) 'MOVE-C-LR )
            ( setf ( bank-cannibals ( state-left-bank s ) ) ( remove 'C ( bank-cannibals ( state-left-bank s ) ) :count 1 ) )
            ( setf ( bank-cannibals ( state-right-bank s )) ( append ( bank-cannibals ( state-right-bank s ) ) '(C) ) )
            ( setf ( bank-boat ( state-left-bank s ) ) nil )
            ( setf ( bank-boat ( state-right-bank s ) ) 'B)
        )
        (( equal ( operator-name o ) 'MOVE-M-LR )
            ( setf ( bank-missionaries ( state-left-bank s ) ) ( remove 'M ( bank-missionaries ( state-left-bank s ) ) :count 1 ) )
            ( setf ( bank-missionaries ( state-right-bank s ) ) ( append ( bank-missionaries ( state-right-bank s ) ) '(M) ) )
            ( setf ( bank-boat ( state-left-bank s ) ) nil )
            ( setf ( bank-boat ( state-right-bank s ) ) 'B)
        )
        (( equal ( operator-name o ) 'MOVE-CM-RL )
            ( setf ( bank-missionaries ( state-right-bank s ) ) ( remove 'M ( bank-missionaries ( state-right-bank s ) ) :count 1 ) )
            ( setf ( bank-cannibals ( state-right-bank s ) ) ( remove 'C ( bank-cannibals ( state-right-bank s ) ) :count 1 ) )
            ( setf ( bank-missionaries ( state-left-bank s ) ) ( append ( bank-missionaries ( state-left-bank s ) ) '(M) ) )
            ( setf ( bank-cannibals ( state-left-bank s ) ) ( append ( bank-cannibals ( state-left-bank s ) ) '(C) ) )
            ( setf ( bank-boat ( state-left-bank s ) ) 'B )
            ( setf ( bank-boat ( state-right-bank s ) ) nil)
        )
        (( equal ( operator-name o ) 'MOVE-CC-RL )
            ( setf ( bank-cannibals ( state-right-bank s ) ) ( remove 'C ( bank-cannibals ( state-right-bank s ) ) :count 2 ) )
            ( setf ( bank-cannibals ( state-left-bank s ) ) ( append ( bank-cannibals ( state-left-bank s ) ) '(C C) ) )
            ( setf ( bank-boat ( state-left-bank s ) ) 'B )
            ( setf ( bank-boat ( state-right-bank s ) ) nil)
        )
        (( equal ( operator-name o ) 'MOVE-MM-RL )
            ( setf ( bank-missionaries ( state-right-bank s ) ) ( remove 'M ( bank-missionaries ( state-right-bank s ) ) :count 2 ) )
            ( setf ( bank-missionaries ( state-left-bank s ) ) ( append ( bank-missionaries ( state-left-bank s ) ) '(M M)))
            ( setf ( bank-boat ( state-left-bank s ) ) 'B )
            ( setf ( bank-boat ( state-right-bank s ) ) nil)
        )
        (( equal ( operator-name o ) 'MOVE-C-RL )
            ( setf ( bank-cannibals ( state-right-bank s ) ) ( remove 'C ( bank-cannibals ( state-right-bank s ) ) :count 1 ) )
            ( setf ( bank-cannibals ( state-left-bank s ) ) ( append ( bank-cannibals ( state-left-bank s ) ) '(C) ) )
            ( setf ( bank-boat ( state-left-bank s ) ) 'B )
            ( setf ( bank-boat ( state-right-bank s ) ) nil)
        )
        (( equal ( operator-name o ) 'MOVE-M-RL )
            ( setf ( bank-missionaries ( state-right-bank s ) ) ( remove 'M ( bank-missionaries ( state-right-bank s ) ) :count 1 ) )
            ( setf ( bank-missionaries ( state-left-bank s ) ) ( append ( bank-missionaries ( state-left-bank s ) ) '(M) ) )
            ( setf ( bank-boat ( state-left-bank s ) ) 'B )
            ( setf ( bank-boat ( state-right-bank s ) ) nil)
        )
    )
)
;------------------------------------------------------------------
; MODELLING A NAME-GENERATOR
( defclass name-generator ()
    ( ( prefix :accessor name-generator-prefix :initarg :prefix :initform "name" )
        ( nr :accessor name-generator-nr :initform 0 )
    )
)
( defmethod next ( ( ng name-generator ) )
    ( setf ( name-generator-nr ng ) ( + 1 ( name-generator-nr ng ) ) )
    ( concatenate 'string
        ( name-generator-prefix ng )
        ( write-to-string ( name-generator-nr ng ) )
    )
)