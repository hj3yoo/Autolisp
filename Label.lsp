;This program performs a macro to replace existing text
;with the incremental numbering.
;--------------------------------------------------------------
;Sep 23, 2014 : Initial commitment
;--------------------------------------------------------------

(defun C:Label(/ term txt entname)
  (setq *strLim
    (cond
      ( (getint
          (strcat "\nEnter Number of Strings Existing <"
            (cond (*strLim (itoa *strLim)) ("1"))
              ; offer current value if present; otherwise 1
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
          (strcat "\nEnter Number of Panels per String <"
            (cond (*pnlLim (itoa *pnlLim)) ("1"))
              ; offer current value if present; otherwise 1
            ">: "
          ); strcat
        ); getint
      ); User-input condition [nil on Enter]
      (*pnlLim) (1)
    ); cond
  );*pnlLim
  (setq *strCur
    (cond
      ( (getint
	  (strcat "\nEnter Current String Number <"
		  (cond (*strCur (itoa *strCur)) ("1"))
		  ; offer current value if present; otherwise 1
		  ">: "
          ); strcat
        ); getint
      ); User-input condition [nil on Enter]
      (*strCur) (1)
    ); cond
  );*strCur
  (setq *pnlCur
    (cond
      ( (getint
          (strcat "\nEnter Current Panel Number <"
            (cond (*pnlCur (itoa *pnlCur)) ("1"))
              ; offer current value if present; otherwise 1
            ">: "
          ); strcat
        ); getint
      ); User-input condition [nil on Enter]
      (*pnlCur) (1)
    ); cond
  );*pnlCur

  (setq term 0) 
  (while (= term 0)
    (setq entname (car (entsel "\nSelect Existing Label: ")
	   );car
    );setq
    (if (= entname nil)
      ()
      (progn
	(setq obj (entget entname))
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
	  );progn
	);if
	(setq entname nil)
      ); progn
    ); if
  ); while
  (princ "\nEnd of the Program.")
  (princ)
)
