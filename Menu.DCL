Menu : dialog {
          label = "Menu";			// Puts a label on the dialog box
          initial_focus = "pnlCur";		// Sets the initial focus
          : boxed_row {
            : column {
              : row {
                : button {			// Replace button
                  key = "replace";
                  label = " Replace ";
                  width = 5;
                  is_default = false;
                  mnemonic = "R";
                }
              }
            }
            : spacer_0 {
              width = 10;
            }
            : column {
              : row {
                : edit_box {			// Input for *pnlCur                
                  key = "pnlCur";
                  label = "Starting panel number: ";
                  edit_width = 3;
                  value = "";
                }
              }
              : row {
                : edit_box {			// Input for *strcur
                  key = "strCur";
                  label = "Starting string number: ";
                  edit_width = 3;
                  value = "";
                }
              }
            } 
          }
          : boxed_row {
            : column {
              : row {
                : button {			// Increment button
                  key = "increment";
                  label = " Increment ";
                  is_default = false;
                  mnemonic = "I";
                }
              }
            }
            : spacer_0 {
              width = 10;
            }
            : column {
              : row {
                : column {
                  : edit_box {			// Input for jump                
                    key = "jump";
                    label = "Incremental of: ";
                    edit_width = 5;
                    value = "";
                  }
                }
              }
            }
          }
//              : row {
//                : button {			// FindAll button
//                    key = "findAll";
//                    label = " FindAll ";
//                    is_default = false;
//                    mnemonic = "F";
//                }
//              }
          : boxed_row {
            : column {
              : button {			// Check button
                key = "check";
                label = " Check ";
                is_default = false;
                mnemonic = "h";
              }
            }
            : spacer_0 {
              width = 10;
            }
            : column {
              : row {
                : edit_box {
                  key = "pnlInit";
                  label = "From: ";
                  edit_width = 3;
                  value = "";
                }
                : edit_box {
                  key = "strInit";
                  edit_width = 3;
                  value = "";
                }
              }
              : row {
                : edit_box {
                  key = "pnlTerm";
                  label = "To: ";
                  edit_width = 3;
                  value = "";
                }
                : edit_box {
                  key = "strTerm";
                  edit_width = 3;
                  value = "";
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
            : spacer_0 {
              width = 10;
            }
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
