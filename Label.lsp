;This program performs a macro to replace existing text
;with the incremental numbering.
;--------------------------------------------------------------
;Sep 23, 2014 : Initial commitment
;		Fixed a bug where current string & panel number
;			can be set higher than its limit
;		Refactoring program
;Sep 24, 2014 : Incr function added
;Sep 25, 2014 : Refactoring program
;--------------------------------------------------------------

(defun incrementBy (jump)
  (setq *pnlCur (+ *pnlCur jump))
  (while (> *pnlCur *pnlLim)
    (setq *pnlCur (- *pnlCur *pnlLim))
    (setq *strCur (1+ *strCur))
  )
  (if (> *strCur *strLim)
    (progn
      (alert "You have reached the end of the last string.")
      (setq term 1)
      (setq *strCur nil)
      (setq *pnlCur nil)
    ); progn
  ); if
)

(defun incrementEntityBy (jump ent / index entInfo txt pnlNum strNum)
  (setq entInfo (entget ent))
  (setq index 1)
  (setq txt (cdr (assoc 1 entInfo))); (pnlNum).(strNum)

  ; separate pnlNum and strNum from txt
  (while (/= (substr txt index 1) ".")
    (setq index (1+ index))
  ); while
  (setq pnlNum (atoi (substr txt 1 (1- index))))
  (setq strNum (atoi (substr txt (1+ index))))
  ; increment the label
  (setq pnlNum (+ pnlNum jump))
  (while (> pnlNum *pnlLim)
    (setq pnlNum (- pnlNum *pnlLim))
    (setq strNum (1+ strNum))
  ); while
  ; terminate if the label is outside of the limit
  (if (> strNum *strLim)
    (progn
      (alert "You have reached the end of the last string.")
      (setq term 1)
    ); progn
    (progn
      (setq txt (strcat (itoa pnlNum) "." (itoa strNum)))
      (setq entInfo (subst (cons 1 txt) (assoc 1 entInfo) entInfo))
      (entmod entInfo)
    ); progn
  ); if
  (princ)
)

(defun ask-int (var msg mod lim / tmp)
  ;; Asks for an integer value to set into the variable, defaulting to the previous value
  (cond
    ( (= mod 1)
      (set var
	(cond
	  ( (getint
	      (strcat "\n" msg " <"
		      (if (eval var) (itoa (eval var)) "1")
		      ">: "
	      ); strcat
	    ); getint
	  )
	  ((eval (cond ((eval var)) (1))))
	); cond
      ); set
    ); mod1
    ( (= mod 2)
      (while
	(>(setq tmp
	    (cond
	      ( (getint
		  (strcat "\n" msg " <"
			  (if (eval var) (itoa (eval var)) "1")
			  ">: "
		  ); strcat
		); getint
	      )
	      ((eval (cond ((eval var)) (1))))
	      ); cond
	    ); setq
	  (eval lim)
	); if current input is bigger than the limit
	(princ (strcat " must be less than " (itoa (eval lim))))
	); while
      (set var tmp)
    ); mod2
  ); cond
)

(defun Replace(/ term txt tmpsl tmppl tmpsc tmppc entName entType entInfo)
  ; initialize
  (ask-int '*pnlLim "Enter number of panels per string" 1 9001)
  (ask-int '*strLim "Enter number of strings existing" 1 9001)
  (ask-int '*pnlCur "Enter current panel number" 2 *pnlLim)
  (ask-int '*strCur "Enter current string number" 2 *strLim)

  (setq term 0) 
  (while (= term 0)
    ; ask user for the label to modify
    (while (and (/= "TEXT" entType) (/= "MTEXT" entType))
      (setq entName (car (entsel "\nSelect the label: ")))
      (cond
	(entName (setq entType (cdr (assoc 0 (entget entName)))))
	(T (princ "No objects selected"))
	; read selected entity's type if present; otherwise repeat
      ); cond
    );while
    (setq entType 0)

    ; replace the chosen label
    (setq entInfo (entget entName))
    (setq txt (strcat (itoa *pnlCur) "." (itoa *strCur)))
    (setq entInfo (subst (cons 1 txt) (assoc 1 entInfo) entInfo))
    (entmod entInfo)
    (princ txt)
    (incrementBy 1)
  ); while
  (princ "\nEnd of the program.")
  (princ)
)

(defun Increment (/ term tmpj ss ssl index)
  (ask-int 'jump "Enter how much you would increment by" 1 9001)

  (setq term 0)
  (while (not ss)
    (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  ); while
  (setq ssl (sslength ss))
  (setq index 0)
  (while (and (> ssl index) (= term 0))
    (incrementEntityBy jump (ssname ss index))
    (setq index (1+ index))
  ); while
)

(defun Label(/ funcMod)
  (initget 1 "r i")
  (setq funcMod (getkword "\nWould you like to [R]eplace label, or [I]ncrement label? "))
  (cond
    ((= funcmod "r") (Replace))
    ((= funcmod "i") (Increment))
  )
)
