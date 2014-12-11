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
;Dec 03, 2014 : Check function added
;		User can now type individual commands
;			separately in the command line
;Dec 04, 2014 : Check function now prompts a list of missing
;		panels at the end
;		Comments added to various functions
;		Increment function now asks user for jump
;		variable at the menu
;Dec 05, 2014 : Minor cleanup
;Dec 09, 2014 : Preparation to add AutoLayer function
;			- Automatically changes layer
;			  according to its panel number
;Dec 11, 2014 : autoLay function added
;--------------------------------------------------------------



(defun saveVars(/ indOpt indStr)
  ;; Save the input from the dialog box
  (setq *pnlCur (atoi (get_tile "pnlCur") ) )
  (setq *strCur (atoi (get_tile "strCur") ) )
  (setq *pnlLim (atoi (get_tile "pnlLim") ) )
  (setq *strLim (atoi (get_tile "strLim") ) )
  (setq *jump (atoi (get_tile "jump") ) )
  (setq *pnlInit (atoi (get_tile "pnlInit") ) )
  (setq *strInit (atoi (get_tile "strInit") ) )
  (setq *pnlTerm (atoi (get_tile "pnlTerm") ) )
  (setq *strTerm (atoi (get_tile "strTerm") ) )
  (setq *cbLim (atoi (get_tile "cbLim") ) )
  (setq *cbOpt nil)
  (setq *cbStrLim nil)
  (setq indOpt 1)
  (setq indStr 1)
  (while (<= indOpt *cbLim)
    (setq *cbOpt (cons (get_tile (strcat "cb" (itoa indOpt) "LayList") ) *cbOpt) )
    (setq indOpt (1+ indOpt) )
  )
  (while (<= indStr *cbLim)
    (setq *cbStrLim (cons (cond
			    ( (get_tile (strcat "cb" (itoa indStr) "StrLim") )
			      (atoi (get_tile (strcat "cb" (itoa indStr) "StrLim") ) )
			    )
			    (T "0")
			  )
		    *cbStrLim)
    )
    (setq indStr (1+ indStr) )
  )
  ;(setq *cb1Opt (get_tile "cb1LayList") )
  ;(setq *cb2Opt (get_tile "cb2LayList") )
  ;(setq *cb3Opt (get_tile "cb3LayList") )
  ;(setq *cb4Opt (get_tile "cb4LayList") )
  ;(setq *cb1LayName (nth (atoi *cb1Opt) *layList) )
  ;(setq *cb2LayName (nth (atoi *cb2Opt) *layList) )
  ;(setq *cb3LayName (nth (atoi *cb3Opt) *layList) )
  ;(setq *cb4LayName (nth (atoi *cb4Opt) *layList) )
  ;(setq *cb1StrLim (atoi (get_tile "cb1StrLim") ) )
  ;(setq *cb2StrLim (atoi (get_tile "cb2StrLim") ) )
  ;(setq *cb3StrLim (atoi (get_tile "cb3StrLim") ) )
  ;(setq *cb4StrLim (atoi (get_tile "cb4StrLim") ) )
  (setq *toggle (cond ( (= (get_tile "autoLay") "0") nil) (T T) ) )
)

(defun autoLay (strNum / ind found layName strLim)
  (setq layName nil)
  (setq found nil)
  (setq ind 1)
  (while (and (<= ind *cbLim) (not found) )
    (if strLim
      (setq strLim (+ strLim (nth (- *cbLim ind) *cbstrLim) ) )
      (setq strLim (nth (- *cbLim 1) *cbStrLim) )
    )
    (if (and (< (- strLim (nth (- *cbLim ind) *cbStrLim) ) strNum) (>= strLim strNum) )
      (progn
	(setq layName (nth (atoi (nth (- *cbLim ind) *cbOpt) ) *layList) )
	(setq found T)
      )
      (setq ind (1+ ind) )
    )
  )
  (if (not found)
    (setq layName nil)
  )
  (princ layName)
)

;; NOTE: incrementEntityBy terminates AFTER changing the entity,
;;       whereas incrementBy only assigns values to *pnlCur and *strCur.
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
  
  (if (or (> strNum *strLim) (<= strNum 0))
    ; Terminate if the label is outside of the limit
    ; Otherwise, increment the label
    (progn
      (alert "The label is outside of the limit.
      \nPlease check the panel and string numbers again.")
      (setq term 1)
    )
    (progn
      (setq txt (strcat (itoa pnlNum) "." (itoa strNum) ) )
      (setq entInfo (subst (cons 1 txt) (assoc 1 entInfo) entInfo) )
      (if (and *toggle (autoLay *strCur) )
        (setq entInfo (subst (cons 8 (autoLay *strCur) ) (assoc 8 entInfo) entInfo) )
      )
      (entmod entInfo)
    )
  )
  (princ)
)

;(defun ask-int (var msg lim / tmp)
;  ;; Ask for an integer value to parse into the variable,
;  ;; defaulting to the previous value
;  (while
;    (>(setq tmp
;	(cond
;	  ( (getint (strcat "\n" msg " <" (if (eval var) (itoa (eval var) ) "1")
;			    ">: ") ) )
;	  ( (eval (cond ( (eval var) ) (1) ) ) )
;	)
;      )
;      (eval lim)
;    )
;    ;; If current input is bigger than the limit
;    (princ (strcat " must be less than " (itoa (eval lim) ) ) )
;  )
;  (set var tmp)
;)

(defun Replace(/ term txt entName entType entInfo)
  (setq ret T)
  (setq term 0)
  
  (while (= term 0)
    ;; Ask for the label to modify
    (while (and (/= "TEXT" entType) (/= "MTEXT" entType) )
      (setq entName (car (entsel "\nSelect the label: ") ) )
      (cond
	(entName (setq entType (cdr (assoc 0 (entget entName) ) ) ) )
	(T (princ "No objects selected") )
	;; read selected entity's type if present; otherwise repeat
      )
    )
    (setq entType 0)

    ;; Replace the chosen label
    (setq entInfo (entget entName) )
    (setq txt (strcat (itoa *pnlCur) "." (itoa *strCur) ) )
    (setq entInfo (subst (cons 1 txt) (assoc 1 entInfo) entInfo) )
    (if (and *toggle (autoLay *strCur) )
      (setq entInfo (subst (cons 8 (autoLay *strCur) ) (assoc 8 entInfo) entInfo) )
    )
    (entmod entInfo)
    (princ txt)
    (incrementBy 1)
  )
  (princ "\nEnd of the program.")
  (princ)
)

(defun Increment (/ term tmpj ss ssl index)
  (setq ret T)
  (setq term 0)

  ;; Ask for a number to increment the label by
  ;;(ask-int 'jump "Enter how much you would increment by" 9001)

  ;; Ask for set of labels to modify
  (while (not ss)
    (setq ss (ssget '( (0 . "TEXT,MTEXT") ) ) )
  )
  (setq ssl (sslength ss) )

  ;; For each labels selected, increment
  (setq index 0)
  (while (and (> ssl index) (= term 0) )
    (incrementEntityBy *jump (ssname ss index) )
    (setq index (1+ index) )
  )
)

(defun FindAll (/ entInfo entName entType blkName ss index count)
  (setq ret T)

  ;; Ask for a model block reference to find
  (while (/= entType "INSERT")
    (setq entName (car (entsel "\nChoose a block reference: ") ) )
    (cond
      (entName (setq entType (cdr (assoc 0 (entget entName) ) ) ) )
      (T (princ "No block reference selected"))
    )
  )
  (princ (setq blkName (cdr (assoc 8 (entget entName) ) ) ) )

  ;; Ask for set of blocks to find from
  (while (not ss)
    (setq ss (ssget '( (0 . "INSERT") ) ) )
  )
  
  ;; Count the number of same block reference
  (setq index 0)
  (setq count 0)
  (while (< index (sslength ss))
    (if (= (cdr (assoc 8 (entget (ssname ss index) ) ) ) blkName)
      (setq count (1+ count) )
    )
    (setq index (1+ index) )
  )

  ;; Report the result
  (princ (strcat "\nThere are " (itoa count) " \"" blkname
		 "\" within the selection.") )
  (princ)
)

(defun Check (/ ss ssl pnlNum strNum index chkTxt lblTxt entName
	      found compl len indSp indRow errMsg)
  (setq ret T)

  (setq indRow 0)
  (setq compl T)
  (setq errMsg "")
  
  ;; Ask for set of labels to check
  (while (not ss)
    (setq ss (ssget '( (0 . "TEXT,MTEXT") ) ) )
  )

  ;; Starting from the lowest number, look for an identical label
  ;; within the selected set
  (setq pnlNum 1)
  (setq strNum 1)
  (while (and (<= strNum *strTerm) (<= pnlNum *pnlTerm) )
    (setq index 0)
    (setq ssl (sslength ss) )
    (setq chkTxt (strcat (itoa pnlNum) "." (itoa strNum) ) )
    (while (and (> ssl index) (not found) )
      (setq entName (ssname ss index) )
      (setq lblTxt (cdr (assoc 1 (entget entName) ) ) )
      (if (= chkTxt lblTxt)
	(progn
	  (ssdel entName ss)
	  (setq found T)
	)
      )
      (setq index (1+ index) )
    )

    ;; If the number wasn't found, add it to the list of missing labels
    (if (not found)
      (progn
	(setq len (strlen chkTxt) )
	(setq indSp 0)
	(while (>= (- 8 len) indSp)
	  (setq chkTxt (strcat chkTxt " ") )
	  (setq indSp (1+ indSp) )
	)
	(cond
	  ( (= indRow 7)
	    (progn
	      (setq errMsg (strcat errMsg chkTxt "\n") )
	      (setq indRow 0)
	    )
	  )
	  (T
	    (progn
	      (setq errMsg (strcat errMsg chkTxt) )
	      (setq indRow (1+ indRow) )
	    )
	  )
	)
	(setq compl nil)
      )
      (setq found nil)
    )
    
    (setq pnlNum (1+ pnlNum) )
    ;; Step up between strings
    (while (> pnlNum *pnlLim)
      (setq pnlNum (- pnlNum *pnlLim) )
      (setq strNum (1+ strNum) )
    )
  )
  
  ;; Report the result
  (if compl
    (alert "There were no missing panel number.")
    (progn
      (alert (strcat "Following panel numbers are missing : \n" errMsg) )
      (princ (strcat "\n" errMsg) )
    )
  )
  (princ)
)

(defun Label(/ ddiag dcl_id ret lay intStr intOpt)
  (setq ret nil)

  ;; Create a list of layer's name
  (setq *layList nil)
  (while (setq lay (cdadr (tblnext "layer" (not lay))))
    (setq *layList (cons lay *layList))
  )
  (setq *layList (acad_strlsort *layList))
  
  ;; Try to load the DCL file from disk into memory
  (if(not(setq dcl_id (load_dialog "Menu.dcl") ) )
    (progn
      (alert "The DCL file could not be loaded.")
      (exit)
    )
    (progn
      ;; Try to load the definition inside the DCL file
      (if (not (new_dialog "Menu" dcl_id) )
        (progn
          (alert "The definition could not be found inside the DCL file")
          (exit)
        )

	;; If the definition was loaded
        (progn
	  
	  (setq indList 1)
	  (while (<= indList 8)
	    (start_list (strcat "cb" (itoa indList) "LayList") 3)
	    (mapcar 'add_list *layList)
	    (end_list)
	    (setq indList (1+ indList) )
	  )
	  ;(start_list "cb1LayList" 3)
	  ;(mapcar 'add_list *layList)
	  ;(end_list)
	  ;(start_list "cb2LayList" 3)
	  ;(mapcar 'add_list *layList)
	  ;(end_list)
	  ;(start_list "cb3LayList" 3)
	  ;(mapcar 'add_list *layList)
	  ;(end_list)
	  ;(start_list "cb4LayList" 3)
	  ;(mapcar 'add_list *layList)
	  ;(end_list)

	  
	  ;; Generate initial value for variables
  	  (set_tile "pnlCur" (cond (*pnlCur (itoa *pnlCur) ) (T "1") ) )
	  (set_tile "strCur" (cond (*strCur (itoa *strCur) ) (T "1") ) )
	  (set_tile "pnlLim" (cond (*pnlLim (itoa *pnlLim) ) (T "1") ) )
	  (set_tile "strLim" (cond (*strLim (itoa *strLim) ) (T "1") ) )
	  (set_tile "jump" (cond (*jump (itoa *jump) ) (T "1") ) )
	  (set_tile "pnlInit" (cond (*pnlInit (itoa *pnlInit) ) (T "1") ) )
	  (set_tile "strInit" (cond (*strInit (itoa *strInit) ) (T "1") ) )
	  (set_tile "pnlTerm" (cond (*pnlTerm (itoa *pnlTerm) ) (T "1") ) )
	  (set_tile "strTerm" (cond (*strTerm (itoa *strTerm) ) (T "1") ) )
	  (set_tile "autoLay" (cond (*toggle "1") (T "0") ) )
	  (set_tile "cbLim" (cond (*cbLim (itoa *cbLim) ) (T "8") ) )
	  (setq indStr 1)
	  (while (<= indStr *cbLim)
	    (set_tile (strcat "cb" (itoa indStr) "StrLim")
		      (cond
			(*cbStrLim (itoa (nth (- *cbLim indStr) *cbStrLim) ) )
			(T "0")
		      )
	    )
	    (setq indStr (1+ indStr) )
	  )
	  (setq indOpt 1)
	  (while (<= indOpt *cbLim)
	    (set_tile (strcat "cb" (itoa indOpt) "LayList")
		      (cond
			(*cbOpt (nth (- *cbLim indOpt) *cbOpt)  )
			(T "0")
		      )
	    )
	    (setq indOpt (1+ indOpt) )
	  )		      
	  ;(set_tile "cb1LayList" (cond (*cb1Opt *cb1Opt) (T "") ) )
	  ;(set_tile "cb2LayList" (cond (*cb2Opt *cb2Opt) (T "") ) )
	  ;(set_tile "cb3LayList" (cond (*cb3Opt *cb3Opt) (T "") ) )
	  ;(set_tile "cb4LayList" (cond (*cb4Opt *cb4Opt) (T "") ) )
	  ;(set_tile "cb1StrLim" (cond (*cb1StrLim (itoa *cb1StrLim) ) (T "1") ) )
	  ;(set_tile "cb2StrLim" (cond (*cb2StrLim (itoa *cb2StrLim) ) (T "1") ) )
	  ;(set_tile "cb3StrLim" (cond (*cb3StrLim (itoa *cb3StrLim) ) (T "1") ) )
	  ;(set_tile "cb4StrLim" (cond (*cb4StrLim (itoa *cb4StrLim) ) (T "1") ) )
	
          ;; If an action event occurs, do this function
          (action_tile "cancel" "(done_dialog 1)")
          (action_tile "accept" "(saveVars)(done_dialog 2)")
	  (action_tile "replace" "(saveVars)(done_dialog 3)")
	  (action_tile "increment" "(saveVars)(done_dialog 4)")
	  (action_tile "findAll" "(saveVars)(done_dialog 5)")
	  (action_tile "check" "(saveVars)(done_dialog 6)")

	  ;; Execution of action tiles
          (setq ddiag(start_dialog) )

          (if (= ddiag 1) (princ) )
          (if (= ddiag 2) (princ) )
	  (if (= ddiag 3) (Replace) )
	  (if (= ddiag 4) (Increment) )
	  (if (= ddiag 5) (FindAll) )
	  (if (= ddiag 6) (Check) )
        )
      )
    )
  )
)

;; User function
(defun C:LABEL (/)
  (label)
  (princ)
)
(defun C:INCR (/)
  (increment)
  (princ)
)
(defun C:REPL (/)
  (replace)
  (princ)
)
(defun C:CHECK (/)
  (check)
  (princ)
)
