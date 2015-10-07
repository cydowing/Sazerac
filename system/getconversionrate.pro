;+
; This function gets from the internet the current currency conversion rate.
; The conversion rate is return as a double.
;
; Author: Antoine Cottin
;              Carbomap Ltd
;              
; History: 11/02/2015 - initial development
;          02/11/2015 - updated the url as previous one didn't exist anymore
;          07/11/2015 - delet temp file that holds the conversion rates
;          
;-
Function getConversionRate, FROM = FROM, TO = TO

COMPILE_OPT idl2, HIDDEN

if not keyword_set(FROM) then begin
  print, 'Keyword FROM needs to be specify...'
  return, 0
endif

if not keyword_set(FROM) then begin
  print, 'Keyword TO needs to be specify...'
  return, 0
endif

; Creating the URL
url = strcompress('http://api.fixer.io/latest?symbols=' + strupcase(string(from)) + ',' + strupcase(string(to)), /remove_all)

; Creating a temp file to store the information
os = os_define()
tempConversionRateFile = strcompress(os.home + os.sep + 'currencyRate.txt', /REMOVE_ALL)

netObject = Obj_New('IDLnetURL')
void = netObject -> Get(URL=url, FILENAME=tempConversionRateFile)
Obj_Destroy, netObject
  
; Opening the temp file
openr, lun, tempConversionRateFile, /GET_LUN
line = ''
readf, lun, line
free_lun, lun, /FORCE

file_delete, tempConversionRateFile

; Parsing the line to get the information
lead = STRPOS(line, '"rates":{')
tail = STRPOS(line, '}}', /REVERSE_SEARCH)
interimString = '"rates":{' + '"' + strupcase(string(to)) + '":'
rate = STRMID(line, lead+strlen(interimString), tail-lead-strlen(interimString))

return, double(rate)
 
 End