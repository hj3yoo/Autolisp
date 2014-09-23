;This program performs a macro to replace existing text
;with the incremental numbering.
;--------------------------------------------------------------
;Sep 23, 2014 : Initial commitment
;		Fixed a bug where current string & panel number
;			can be set higher than its limit
;--------------------------------------------------------------

(defun C:Label(/ term txt temp entName entType)
  (setq *strLim
    (cond
      ( (getint
          (strcat "\nEnter number of strings existing <"
            (cond (*strLim (itoa *strLim)) ("1"))
              ; offer previous value if present; otherwise 1
            ">: "
          ); strcat
        ); getint
      ); User-input condition [nil on Enter]
      (*strLim) (1)
    ); cond
  );*strLim
  (setq *pnlLim
    (cond
      ( (getint
          (strcat "\nEnter number of panels per string <"
            (cond (*pnlLim (itoa *pnlLim)) ("1"))
              ; offer previous value if present; otherwise 1
            ">: "
          ); strcat
        ); getint
      ); User-input condition [nil on Enter]
      (*pnlLim) (1)
    ); cond
  );*pnlLim
  (setq *strCur
    (cond
      ( (not
	  (while
	    (> (setq temp
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
	); not
        temp
      ); User-input condition [nil on Enter]
      (*strCur) (1)
      ; maintains previous value if no inputs were given;
    ); cond
  );*strCur
  (setq *pnlCur
    (cond
      ( (not
	  (while
	    (> (setq temp
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
	); not
        temp
      ); User-input condition [nil on Enter]
      (*pnlCur) (1)
      ; maintains previous value if no inputs were given;
    ); cond
  );*pnlCur

  (setq term 0) 
  (while (= term 0)
    (while (and (/= "TEXT" entType) (/= "MTEXT" entType))
      (setq entName (car (entsel "\nSelect the label: ")))
      (cond
	(entName (setq entType (cdr (assoc 0 (entget entName)))))
	(t (princ "No objects selected"))
	; reads selected entity's type if present; otherwise repeat
      );cond
    );while
    (setq entType 0)
    (setq obj (entget entName))
    (setq txt (strcat (itoa *pnlCur) "." (itoa *strCur)))
    (setq obj (subst (cons 1 txt) (assoc 1 obj) obj))
    (entmod obj)
    (princ txt)
    (if (>= *pnlCur *pnlLim)
      (progn
	(setq *pnlCur 1)
	(setq *strCur (+ *strCur 1))
      );progn
      (setq *pnlCur (+ *pnlCur 1))
    );if
    (if (> *strCur *strLim)
      (progn
	(alert "You have reached the end of the last string.")
	(setq term 1)
	(setq strCur nil)
	(setq pnlCur nil)
      );progn
    );if
  ); while
  (princ "\nEnd of the program.")
  (princ)
)
