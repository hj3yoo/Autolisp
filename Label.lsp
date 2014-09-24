;This program performs a macro to replace existing text
;with the incremental numbering.
;--------------------------------------------------------------
;Sep 23, 2014 : Initial commitment
;		Fixed a bug where current string & panel number
;			can be set higher than its limit
;		Refactoring program
;Sep 24, 2014 : Incr function added
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

(defun incrementEntityBy (jump inp / index entInfo txt pnlNum strNum)
  (setq entInfo (entget inp))
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
  (setq txt (strcat (itoa pnlNum) "." (itoa strNum)))
  (setq entInfo (subst (cons 1 txt) (assoc 1 entInfo) entInfo))
  (entmod entInfo)
  (princ)
  ;(if (> strNum *strLim)
  ;TODO: alert when it reaches above the limit
  ;); if
)

; currently working on it
;(defun ask-int (var msg mod / vval tmp)
;  ;; Asks for an integer value to set into the variable, defaulting to the previous value
;  (cond
;    ( (= mod 1)
;      (setq vval (getint (strcat "\n" msg " <"
;				(cond (var (itoa var)) ("1"))
;				; offer previous value if present; otherwise 1
;				">: "
;			 ); strcat
;		 ); getint
;      ); setq
;      (setq var
;	     (cond (vval) (var) (1))
;      ); setq
;    ); mod1
;    ( (= mod 2)
;      (while
;	(> (setq vval (getint (strcat "\n" msg " <"
;				      (cond (var (itoa var)) ("1"))
;				      ; offer previous value if present; otherwise 1
;				      ">: "
;				); strcat
;		      ); getint
;	   ); setq
;	lim
;	); if current panel number is bigger than the limit
;	(princ (strcat " must be less than " (itoa lim)))
;      ); while
;     (setq var
;	 (cond (vval) (var) (1))
;  ); setq
;    ); mod2
;  ); cond
;)

(defun C:Label(/ term txt tmpsl tmppl tmpsc tmppc entName entType entInfo)
  ; initialize
;  (ask-int (eval *pnlLim) "Enter number of panels per string" 1)
;  (ask-int '*strLim "Enter number of strings existing" 1)
;  (ask-int *pnlCur "Enter current panel number" 2)
;  (ask-int *strCur "Enter current string number" 2)
  (setq tmppl
	 (getint
	   (strcat "\nEnter number of panels per string <"
		   (cond (*strLim (itoa *strLim)) ("1"))
		   ; offer previous value if present; otherwise 1
		   ">: "
	   ); strcat
	 ); getint
  ); setq
  (setq *pnlLim
	 (cond (tmppl) (*pnlLim) (1))
  ); setq
  (setq tmpsl
	 (getint
	   (strcat "\nEnter number of strings existing <"
		   (cond (*strLim (itoa *strLim)) ("1"))
		   ; offer previous value if present; otherwise 1
		   ">: "
	   ); strcat
	 ); getint
  ); setq
  (setq *strLim
	 (cond (tmpsl) (*strLim) (1))
  ); setq
  (while
    (> (setq tmppc
	      (getint
		(strcat "\nEnter current panel number <"
			(cond (*pnlCur (itoa *pnlCur)) ("1"))
			; offer previous value if present; otherwise 1
			">: "
		); strcat
	      ); getint
	    ); setq
       *pnlLim
    ); if current panel number is bigger than the limit
    (princ (strcat " must be less than " (itoa *pnlLim)))
  ); while
  (setq *pnlCur
	 (cond (tmppc) (*pnlCur) (1))
  ); setq
  (while
    (> (setq tmpsc
	      (getint
		(strcat "\nEnter current string number <"
			(cond (*strCur (itoa *strCur)) ("1"))
			; offer previous value if present; otherwise 1
			">: "
		); strcat
	      ); getint
	    ); setq
       *strLim
    ); if current string number is bigger than the limit
    (princ (strcat " must be less than " (itoa *strLim)))
  ); while
  (setq *strCur
	 (cond (tmpsc) (*strCur) (1))
  ); setq

  (setq term 0) 
  (while (= term 0)
    ; ask user for the label to modify
    (while (and (/= "TEXT" entType) (/= "MTEXT" entType))
      (setq entName (car (entsel "\nSelect the label: ")))
      (cond
	(entName (setq entType (cdr (assoc 0 (entget entName)))))
	(t (princ "No objects selected"))
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

(defun C:Incr (/ tmpj ss ssl index)
  (setq tmpj
	 (getint
	   (strcat "\nEnter how much you would increment by: <"
		   (cond (jump (itoa jump)) ("1"))
		   ; offer previous value if present; otherwise 1
		   ">: "
	   ); strcat
	 ); getint
  ); setq
  (setq jump
	 (cond (tmpj) (jump) (1))
  ); setq
  (while (not ss)
    (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  ); while
  (setq ssl (sslength ss))
  (setq index 0)
  (while (> ssl index)
    (incrementEntityBy jump (ssname ss index))
    (setq index (1+ index))
  )
)

