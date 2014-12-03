Menu : dialog {
          label = "Menu";			// Puts a label on the dialog box
          initial_focus = "pnlCur";		// Sets the initial focus
          : row {
            : column {
              : row {
                : button {			// Replace button
                    key = "replace";
                    label = " Replace ";
                    is_default = false;
                    mnemonic = "R";
                }
              }
              : row {
                : button {			// Increment button
                    key = "increment";
                    label = " Increment ";
                    is_default = false;
                    mnemonic = "I";
                }
              }
              : row {
                : button {			// FindAll button
                    key = "findAll";
                    label = " FindAll ";
                    is_default = false;
                    mnemonic = "F";
                }
              }
              : row {
                : button {			// Check button
                    key = "check";
                    label = " Check ";
                    is_default = false;
                    mnemonic = "h";
                }
              }
            }
            : column {
              : boxed_row {
                : column {
                  : edit_box {			// Input for *pnlCur                
                    key = "pnlCur";
                    label = "Current panel number: ";
                    edit_width = 5;
                    value = "";
                  }
                  : edit_box {			// Input for *strcur
                    key = "strCur";
                    label = "Current string number: ";
                    edit_width = 5;
                    value = "";
                  }
                } 
              }
              : boxed_row {
                : column {
                  : edit_box {			// Input for *pnlLim
                    key = "pnlLim";
                    label = "Number of panels per string: ";
                    edit_width = 5;
                    value = "";
                  }
                  : edit_box {			// Input for *strLim
                    key = "strLim";
                    label = "Number of strings: ";
                    edit_width = 5;
                    value = "";
                  }
                } 
              }
            }
          }
          : row {
            : column {
              : boxed_row {
                : button {			// OK button
                  key = "accept";
                  label = " Okay ";
                  is_default = true;
                  mnemonic = "O";
                }
                : button {			// Cancel button
                  key = "cancel";
                  label = " Cancel ";
                  is_default = false;
                  is_cancel = true;
                  mnemonic = "C";
                }
              }
            }
          }            
				}
