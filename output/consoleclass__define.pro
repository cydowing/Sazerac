Function consoleclass::init, _extra = console_options

  Compile_opt idl2, hidden
 
self.wID = 9999

if n_elements(console_options) gt 0 then begin

  ntags = n_tags(console_options)
  tags = tag_names(console_options)
  
  for i = 0, ntags-1 do begin
  
    tag = (tag_names(console_options))[i]
      
    case 1 of
      
      strlowcase(tag) eq 'file': begin
        
        logfield = where(strlowcase(tags) eq 'log', /NULL)
        
        if logfield ne !NULL then begin
          self.Logpath = console_options.file
        endif else begin
          self.print, 2, 'No log file name specified...'
          self.print, 2, "The output log name is set to 'idl_console_output.log' and will be located in user's directory..."
          self.LogPath = 'idl_console_output.log'
        endelse
      
        Openw, Lun, self.LogPath, /get_lun
        self.Consolelun = Lun
        self.Consolesetup = 1
        self.print, 2, 'Log file mode enable...'
  
      end
      
      ; Added to avoid any warning output in the log file as it will look for this tag
      strlowcase(tag) eq 'log':
      
      ; Enable quiet mode
      strlowcase(tag) eq 'quiet' : begin
        self.print, 2, 'Quiet mode enable...'
        self.Consolesetup = 2
;        print, codeString[code], codeString[0], stringText, format = '(a-7,tr1,a2,tr1,a-255)'
        
      end
      
      strlowcase(tag) eq 'verbose': self.print, 2, 'verbose mode enable...'
      
      strlowcase(tag) eq 'gui': begin
        self.print, 2, 'GUI mode enable...'
        self.Consolesetup = 4
        dum= where(strlowcase(tags) eq 'wid', /NULL)

        if dum ne !NULL then begin
          self.wID = console_options.wid
        endif else begin
          self.print, 2, 'No widgetID provided specified...'
          self.print, 2, "The output is redirected to the console..."
        endelse
          
       end
      
      else : begin
;        self.print, 2, "Was looking at statement " + tag
;        self.print, 2, 'Unknown case for consoleclass, moving on...'
      end
      
    endcase

  endfor
    
endif

  ; Initializing the object
  Return, 1

End



Pro consoleclass::cleanup

  Compile_opt idl2
   
  ;ptr_free, self.consoleSetup, self.consoleLun 
    
End  



Pro consoleclass::help

  help, self, /obj
  return

End


;; Function to output information on GUI console
;Pro consoleclass::printLog, pointerState, code, stringText
;
;codeString = ["::","INFO","WARNING","ERROR"]
;tempString = string(codeString[code], codeString[0], stringText, format = '(a-7,tr1,a2,tr1,a-255)')
;
;widget_control, (*pointerState).wtLog, set_value=tempString, /APPEND
;
;End
;
;; Function to output array information on GUI console
;Pro consoleclass::printLogArray, pointerState, code, stringArray
;
;  codeString = ["::","INFO","WARNING","ERROR"]
;
;  n = n_elements(stringArray)
;  stringFormat= '(a-7,tr1,a2,tr3,'+string(n)+'(a, :, ", "))'
;  s = STRING(codeString[code], codeString[0], stringArray, FORMAT = stringFormat)
;  widget_control, (*pointerState).wtLog, set_value=s, /APPEND
;
;End






Pro consoleclass::Progress, code, n, m

  flag = 0

  ;INFO    :: -> code 1
  ;WARNING :: -> code 2
  ;ERROR   :: -> code 3
  codeString = ["::","INFO","WARNING","ERROR"]
  
  case self.Consolesetup of
    0:Print, FORMAT='(%"%s", a-7,tr1,a2,tr1, %"%d job process over %d...",$)', string(13b), codeString[code], codeString[0], n, m
    1:
    2:
    3:
    4:
  endcase
  
End

; Function to output information on console
Pro consoleclass::print, code, stringText

flag = 0

;INFO    :: -> code 1
;WARNING :: -> code 2
;ERROR   :: -> code 3
codeString = ["::","INFO","WARNING","ERROR"]


case self.consoleSetup of
0:print, codeString[code], codeString[0], stringText, format = '(a-7,tr1,a2,tr1,a-255)'
1:printf, self.consoleLun, codeString[code], codeString[0], stringText, format = '(a-7,tr1,a2,tr1,a-255)'
2:
3:
4: begin
    if self.wID ne 9999 then begin
      tempString = string(codeString[code], codeString[0], stringText, format = '(a-7,tr1,a2,tr1,a-255)')
      widget_control, self.wID, set_value=tempString, /APPEND
    endif else begin
      self.consoleSetup = 0
      self.print, code, stringText
    endelse
  end

endcase

End

; Function to output information on console
Pro consoleclass::printArray, code, stringArray

flag = 0

;INFO    :: -> code 1
;WARNING :: -> code 2
;ERROR   :: -> code 3
codeString = ["::","INFO","WARNING","ERROR"]

n = n_elements(stringArray)
stringFormat= '(a-7,tr1,a2,tr3,'+string(n)+'(a, :, ", "))'

case self.consoleSetup of
0:print, FORMAT = stringFormat, codeString[code], codeString[0], stringArray
1:printf, self.consoleLun, codeString[code], codeString[0], stringArray, FORMAT = stringFormat
2:
3:
4:begin
    if self.wID ne 9999 then begin
      s = STRING(codeString[code], codeString[0], stringArray, FORMAT = stringFormat)
      widget_control, self.wID, set_value=s, /APPEND
    endif else begin
      self.consoleSetup = 0
      self.printArray, code, stringArray
    endelse
    
  end
endcase


End



; Function to output information on console
Pro consoleclass::printLUT, code, array, thres

  flag = 0

  ;INFO    :: -> code 1
  ;WARNING :: -> code 2
  ;ERROR   :: -> code 3
  codeString = ["::","INFO","WARNING","ERROR"]

  invalid = where(array eq thres, /NULL)
  
  ; Converting array to string array
  array = string(array)
  nArray = n_elements(array)
  
  ; substitution of invalid value
  if invalid ne !NULL then array[invalid] = '       empty'  ; Here we add 8-1 space in front of empty to align string
  
  ; Counting the number of line to
  modulo = narray mod 8.
  divNArray = narray / 8
  if modulo eq 0 then begin
    
    array = reform(array, 8, divNArray)
    n = divNArray
  
    for i=0,n-1 do begin
        
      stringFormat= '(a-7,tr1,a2,tr3,'+String(n)+'(8a-12, :, " | "))'
  
      case self.Consolesetup of
        0:Print, FORMAT = stringFormat, codeString[code], codeString[0], Array[*,i]
        1:Printf, self.Consolelun, codeString[code], codeString[0], Array[*,i], FORMAT = stringFormat
        2:
        3:
        4:begin
            if self.wID ne 9999 then begin
              s = STRING(codeString[code], codeString[0], Array[*,i], FORMAT = stringFormat)
              widget_control, self.wID, set_value=s, /APPEND
            endif else begin
              self.consoleSetup = 0
              self.printLUT, code, array, thres
            endelse
            
          end
      endcase
    
    endfor
    
  endif else begin

    templ = strarr(divNArray*8)
    templ[0] = array[0:(divNArray*8)-1]
    tempr = array[divNArray*8:*]
    
    array = Reform(templ, 8, divNArray)
    n = divNArray

    for i=0,n-1 do begin

      stringFormat= '(a-7,tr1,a2,tr3,'+String(n)+'(8a-12, :, " | "))'

      case self.Consolesetup of
        0:Print, FORMAT = stringFormat, codeString[code], codeString[0], Array[*,i]
        1:Printf, self.Consolelun, codeString[code], codeString[0], Array[*,i], FORMAT = stringFormat
        2:
        3:
        4: begin
            if self.wID ne 9999 then begin
              s = STRING(codeString[code], codeString[0], Array[*,i], FORMAT = stringFormat)
              widget_control, self.wID, set_value=s, /APPEND
            endif else begin
              self.consoleSetup = 0
              self.printLUT, code, array, thres
            endelse
           end
      endcase

    endfor
    
    ; Printing the last line
    stringFormat= '(a-7,tr1,a2,tr3,'+String(n)+ '(' + String(n_elements(tempr)) + 'a-12, :, " | "))'

    case self.Consolesetup of
      0:Print, FORMAT = stringFormat, codeString[code], codeString[0], tempr
      1:Printf, self.Consolelun, codeString[code], codeString[0], tempr, FORMAT = stringFormat
      2:
      3:
      4:begin
          if self.wID ne 9999 then begin
            s = STRING(codeString[code], codeString[0], tempr, FORMAT = stringFormat)
            widget_control, self.wID, set_value=s, /APPEND
          endif else begin
            self.consoleSetup = 0
            self.printLUT, code, array, thres
          endelse
          
        end
      
    endcase

    
    
  endelse


End

Pro consoleclass::printsep


self.print,1,'========================================================='

End


Function consoleclass::setMode, val

self.previousMode = self.consoleSetup
self.consoleSetup = val
return, 1

End


Function consoleclass::restoreMode

  self.Consolesetup = self.Previousmode
  Return, 1

End


Function consoleclass::setwID, wID = wID

  self.consoleSetup = 4
  self.wID = wID
  return, 1

End



Pro consoleclass__define

  COMPILE_OPT idl2, HIDDEN
  
  ; Definition of the data hold by the object
  void = {consoleclass, $
    consoleSetup     :0B, $             ; Execution information output, 0:console,1:file,3:quiet
    previousMode     :0B,$
    logPath          :'', $             ; Path the to log file
    consoleLun       :0B, $             ; Path to the LAS file
    wID              :0 $                ; Widget ID
  }

End




