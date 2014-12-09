Menu : dialog {
          label = "Menu";				// Puts a label on the dialog box
	  initial_focus = "pnlCur";			// Sets the initial focus
	  : row {
	  : boxed_column {
	      : boxed_row {
		  : column {
		      : row {
			  : button {			// Replace button
			      key = "replace";
			      label = "  Replace  ";
			      is_default = false;
			      mnemonic = "R";
			  }
		      }
		  }
		  : spacer_1 {}
		  : column {
		      : edit_box {				// Input for *pnlCur
			  key = "pnlCur";
			  label = "Starting at: ";
			  edit_width = 3;
			  value = "";
		      }
		  }
		  : column {
		      : edit_box {			// Input for *strcur
			  key = "strCur";
			  edit_width = 3;
			  value = "";
		      }
		  }
	      }
	      : boxed_row {
		  : column {
		      : row {
			  : button {				// Increment button
			      key = "increment";
			      label = "Increment";
			      is_default = false;
			      mnemonic = "I";
			  }
		      }
		  }
		  : spacer_1 {}
		  : column {
		      : row {
			  : column {
			      : edit_box {			// Input for jump                
				  key = "jump";
				  label = "Incremental of: ";
				  edit_width = 3;
				  value = "";
			      }
			  }
		      }
		  }
	      }
	      : boxed_row {
		  : column {
		      : button {			// Check button
			  key = "check";
			  label = "  Check  ";
			  is_default = false;
			  mnemonic = "h";
		      }
		  }
		  : spacer_1 {}
		  : column {
		      : row {
			  : column {
			      : edit_box {
				  key = "pnlInit";
				  label = "From: ";
				  edit_width = 3;
				  value = "";
			      }
			  }
			  : column {
			      : edit_box {
				  key = "strInit";
				  edit_width = 3;
				  value = "";
			      }
			  }
		      }
		      : row {
			  : column {
			      : edit_box {
				  key = "pnlTerm";
				  label = "To: ";
				  edit_width = 3;
				  value = "";
			      }
			  }
			  : column {
			      : edit_box {
				  key = "strTerm";
				  edit_width = 3;
				  value = "";
			      }
			  }
		      }
		  }
	      }
	      : boxed_row {
		  : column {
		      : edit_box {			// Input for *pnlLim
			  key = "pnlLim";
			  label = "Number of panels per string: ";
			  edit_width = 3;
			  value = "";
		      }
		      : edit_box {			// Input for *strLim
			  key = "strLim";
			  label = "Number of strings: ";
			  edit_width = 3;
			  value = "";
		      }
		  } 
	      }
	      : row {
		  : spacer_0 {}
		  : button {				// OK button
		      key = "accept";
		      label = " Okay ";
		      width = 5;
		      is_default = true;
		      mnemonic = "O";
		  }
		  : button {			// Cancel button
		      key = "cancel";
		      label = " Cancel ";
		      width = 5;
		      is_default = false;
		      is_cancel = true;
		      mnemonic = "C";
		  }
	      }
	  }
	  : boxed_column {
	      width = 60;
	      : row {
		  : toggle {
		      key = "autoLay";
		      label = " Automatically change layer according to panel number";
		      value = "1";
		  }
	      }
	      : row {
		  : text {
		      value = "CB #1";
		      width = 3;
		  }
		  : popup_list {
		      key = "cb1LayList";
		      width = 20;
		      value = "";
		  }
		  : edit_box {
		      key = "cb1StrNum";
		      label = " contains ";
		      width = 3;
		      value = "";
		  }
		  : text {
		      value = "strings";
		      width = 3;
		  }
	      }
	      : row {
		  : text {
		      value = "CB #2";
		      width = 3;
		  }
		  : popup_list {
		      key = "cb2LayList";
		      width = 20;
		      value = "";
		  }
		  : edit_box {
		      key = "cb2StrNum";
		      label = " contains ";
		      width = 3;)(
		      value = "";
		  }
		  : text {
		      value = "strings";
		      width = 3;
		  }
	      }
	      : row {
		  : text {
		      value = "CB #3";
		      width = 3;
		  }
		  : popup_list {
		      key = "cb3LayList";
		      width = 20;
		      value = "";
		  }
		  : edit_box {
		      key = "cb3StrNum";
		      label = " contains ";
		      width = 3;
		      value = "";
		  }
		  : text {
		      value = "strings";
		      width = 3;
		  }
	      }
	      : row {
		  : text {
		      value = "CB #4";
		      width = 3;
		  }
		  : popup_list {
		      key = "cb4LayList";
		      width = 20;
		      value = "";
		  }
		  : edit_box {
		      key = "cb4StrNum";
		      label = " contains ";
		      width = 3;
		      value = "";
		  }
		  : text {
		      value = "strings";
		      width = 3;
		  }
	      }
	  }
	  }
      }
