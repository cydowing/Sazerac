;+
; This function gets from the internet the current currency conversion rate.
; The conversion rate is return as a double.
;
; Author: Antoine Cottin
;              Carbomap Ltd
;              11/02.2015
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

;  http://rate-exchange.appspot.com/currency?from=USD&to=EUR
;from = 'USD'
;to = 'EUR'

; Creating the URL
url = strcompress('http://rate-exchange.appspot.com/currency?from=' + strupcase(string(from)) + '&to=' + strupcase(string(to)), /remove_all)

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

; Parsing the line to get the information
lead = STRPOS(line, '"rate": ')
tail = STRPOS(line, ', "from":')
rate = STRMID(line, lead+strlen('"rate": '), tail-lead-strlen('"rate": '))
 
return, double(rate)
 
 End