;--------------------------------------------------------------
;Sep 23, 2014 : Initial commitment
;		Fixed a bug where current string & panel number
;			can be set higher than its limit
;		Refactoring program
;Sep 24, 2014 : Incr function added
;Sep 25, 2014 : Refactoring program
;Sep 26, 2014 : FindAll function added
;		Initial menu created
;Oct 30, 2014 : General cleanup
;		Fixed a bug where Increment would not work
;			properly if negative number is given
;Oct 31, 2014 : Improvement on accessing functions
;Nov 04, 2014 : Initial GUI added for menu
;		Fixed a bug where Increment could produce
;			label with panel number 0
;--------------------------------------------------------------

(defun *error* ( errormsg )
  (if (not (member errormsg '("Function cancelled" "quit / exit abort" "Cancel") ) )
    (princ (strcat "\nError: " errormsg) )
    (if ret (Label))
  )
  (princ)
)

(defun saveVars(/ pnlCur strCur pnlLim strLim)
  ;; Save the input from the dialog box
  (setq pnlCur (atoi (get_tile "pnlCur")))
  (setq strCur (atoi (get_tile "strCur")))
  (setq pnlLim (atoi (get_tile "pnlLim")))
  (setq strLim (atoi (get_tile "strLim")))
  (setq *pnlCur pnlCur)
  (setq *strCur strCur)
  (setq *pnlLim pnlLim)
  (setq *strLim strLim)
)

;;NOTE: incrementEntityBy terminates AFTER changing the entity,
;;      whereas incrementBy only assigns values to *pnlCur and *strCur.
(defun incrementBy (jump)
  (setq *pnlCur (+ *pnlCur jump) )

  ;; Step up / down
  (while (> *pnlCur *pnlLim)
    (setq *pnlCur (- *pnlCur *pnlLim) )
    (setq *strCur (1+ *strCur) )
  )
  (while (<= *pnlCur 0)
    (setq *pnlCur (+ *pnlCur *pnlLim) )
    (setq *strCur (1- *strCur) )
  )

  ;; Terminates if the label is outside of the limit
  (if (> *strCur *strLim)
    (progn
      (alert "You have reached the end of the last string.")
      (setq term 1)
      (setq *strCur nil)
      (setq *pnlCur nil)
    )
  )
)

(defun incrementEntityBy (jump ent / index entInfo txt pnlNum strNum)
  (setq entInfo (entget ent) )
  (setq index 1)
  (setq txt (cdr (assoc 1 entInfo) ) ); (pnlNum).(strNum)

  ;; Assign pnlNum and strNum using txt
  (while (or (/= (substr txt index 1) ".") (> index 10))
    (setq index (1+ index) )
  )
  (setq pnlNum (atoi (substr txt 1 (1- index) ) ) )
  (setq strNum (atoi (substr txt (1+ index) ) ) )
  
  ;; Step up / down
  (setq pnlNum (+ pnlNum jump) )
  (while (> pnlNum *pnlLim)
    (setq pnlNum (- pnlNum *pnlLim) )
    (setq strNum (1+ strNum) )
  )
  (while (<= pnlNum 0)
    (setq pnlNum (+ pnlNum *pnlLim) )
    (setq strNum (1- strNum) )
  )
  
  (if (> strNum *strLim)
    ; Terminate if the label is outside of the limit
    ; Otherwise, increment the label
    (progn
      (alert "You have reached the end of the last string.")
      (setq term 1)
    )
    (progn
      (setq txt (strcat (itoa pnlNum) "." (itoa strNum) ) )
      (setq entInfo (subst (cons 1 txt) (assoc 1 entInfo) entInfo) )
      (entmod entInfo)
    )
  )
  (princ)
)

(defun ask-int (var msg lim / tmp)
  ;; Asks for an integer value to parse into the variable,
  ;; defaulting to the previous value
  (while
    (>(setq tmp
	(cond
	  ( (getint (strcat "\n" msg " <" (if (eval var) (itoa (eval var) ) "1")
			    ">: ")	 ) )
	  ( (eval (cond ( (eval var) ) (1) ) ) )
	)
      )
      (eval lim)
    ); if current input is bigger than the limit
    (princ (strcat " must be less than " (itoa (eval lim) ) ) )
  )
  (set var tmp)
)

(defun Replace(/ term txt entName entType entInfo)
  (setq ret T)
;  ;; initialize
;  (ask-int '*pnlLim "Enter number of panels per string" 9001)
;  (ask-int '*strLim "Enter number of strings existing" 9001)
;  (ask-int '*pnlCur "Enter current panel number" *pnlLim)
;  (ask-int '*strCur "Enter current string number" *strLim)

  (setq term 0)
  (while (= term 0)
    
    ;; ask user for the label to modify
    (while (and (/= "TEXT" entType) (/= "MTEXT" entType) )
      (setq entName (car (entsel "\nSelect the label: ") ) )
      (cond
		(entName (setq entType (cdr (assoc 0 (entget entName) ) ) ) )
		(T (princ "No objects selected") )
		;; read selected entity's type if present; otherwise repeat
      )
    )
    (setq entType 0)

    ;; replace the chosen label
    (setq entInfo (entget entName) )
    (setq txt (strcat (itoa *pnlCur) "." (itoa *strCur) ) )
    (setq entInfo (subst (cons 1 txt) (assoc 1 entInfo) entInfo) )
    (entmod entInfo)
    (princ txt)
    (incrementBy 1)
  )
  (princ "\nEnd of the program.")
  (princ)
)

(defun Increment (/ term tmpj ss ssl index)
  (setq ret T)
  (ask-int 'jump "Enter how much you would increment by" 9001)
  (setq term 0)
  (while (not ss)
    (setq ss (ssget '( (0 . "TEXT,MTEXT") ) ) )
  )
  (setq ssl (sslength ss) )
  (setq index 0)
  (while (and (> ssl index) (= term 0) )
    (incrementEntityBy jump (ssname ss index) )
    (setq index (1+ index) )
  )
)

(defun FindAll (/ entInfo entName entType blkName ss index count)
  (setq ret T)
  (while (/= entType "INSERT")
    (setq entName (car (entsel "\nChoose a block reference: ")))
    (cond
      (entName (setq entType (cdr (assoc 0 (entget entName)))))
      (T (princ "No block reference selected"))
      ;; read selected entity's type if present; otherwise repeat
    )
  )
  (princ (setq blkName (cdr (assoc 8 (entget entName)))))
  (while (not ss)
    (setq ss (ssget '((0 . "INSERT"))))
  )
  
  ;; Count the number of same block reference
  (setq index 0)
  (setq count 0)
  (while (< index (sslength ss))
    (if (= (cdr (assoc 8 (entget (ssname ss index)))) blkName)
      (setq count (1+ count))
    )
    (setq index (1+ index))
  )
  (princ (strcat "\nThere are " (itoa count) " \"" blkname
		 "\" within the selection."))
  (princ)
)

(defun Label(/ ddiag dcl_id ret)
  (setq ret nil)
  ;; Try to load the DCL file from disk into memory
  (if(not(setq dcl_id (load_dialog "Menu.dcl")))
    (progn
      (alert "The DCL file could not be loaded.")
      (exit)
    )
    (progn
      ;; Try to load the definition inside the DCL file
      (if (not (new_dialog "Menu" dcl_id))
        (progn
          (alert "The definition could not be found inside the DCL file")
          (exit)
        )

	;; If the definition was loaded
        (progn
	  ;; Generate initial value for variables
  	  (set_tile "pnlCur" (cond (*pnlCur (itoa *pnlCur)) (T "1")))
	  (set_tile "strCur" (cond (*strCur (itoa *strCur)) (T "1")))
	  (set_tile "pnlLim" (cond (*pnlLim (itoa *pnlLim)) (T "1")))
	  (set_tile "strLim" (cond (*strLim (itoa *strLim)) (T "1")))
	
          ;; If an action event occurs, do this function
          (action_tile "cancel" "(done_dialog 1)")
          (action_tile "accept" "(saveVars)(done_dialog 2)")
	  (action_tile "replace" "(saveVars)(done_dialog 3)")
	  (action_tile "increment" "(saveVars)(done_dialog 4)")
	  (action_tile "findAll" "(saveVars)(done_dialog 5)")

	  ;; Execution of action tiles
          (setq ddiag(start_dialog))

          (if (= ddiag 1)
            (princ)
          )
          (if (= ddiag 2)
            (princ)
          )
	  (if (= ddiag 3)
	    (Replace)
	  )
	  (if (= ddiag 4)
	    (Increment)
	  )
	  (if (= ddiag 5)
	    (FindAll)
	  )
        )
      )
    )
  )
)

;(defun Label (/ mod done)
;  (prompt
;    (strcat "\nWhich action would you like to do?"
;      "\n[R]eplace existing label,"
;      " [I]ncrement labels,"
;      " [F]ind identical blocks"
;      " [S]etup string schedule: "
;    ); strcat
;  ); prompt
;  (while (and (not done) (setq mod (grread T 12 0)))
;    (cond
;      ((or (equal opt '(2 82)) (equal mod '(2 114))); typed R or r
;        (setq done T); [stops (while) loop to end this routine with calling of chosen routine]
;        (Replace)
;      ); R condition
;      ((or (equal opt '(2 73)) (equal mod '(2 105))); typed I or i
;        (setq done T)
;        (Increment)
;      ); I condition
;      ((or (equal opt '(2 70)) (equal mod '(2 102))); typed F or f
;        (setq done T)
;        (FindAll)
;      ); F condition
;      ((or (equal opt '(2 83)) (equal mod '(2 115))); typed S or s
;        (setq done T)
;        (Setup)
;      ); S condition
;    ); cond
;  ); while
;); defun

