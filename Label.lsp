;This program performs a macro to replace existing text
;with the incremental numbering.
;--------------------------------------------------------------
;Sep 23, 2014 : Initial commitment
;		Fixed a bug where current string & panel number
;			can be set higher than its limit
;		Refactoring program
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

(defun C:Label(/ term txt tmpsl tmppl tmpsc tmppc entName entType entInfo)
  ; initialize
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
