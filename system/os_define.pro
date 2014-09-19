Function os_define

; Creating structure to return
os = {osStructure, sep:'',osRoot:''}

; Specific path separator
pathSep = PATH_SEP()

CASE !VERSION.OS_FAMILY OF
  'unix'    : osRoot = '/'
  'Windows' : osRoot = STRMID(!DIR, 0, 2)
ENDCASE

os.sep = pathSep
os.osRoot = osRoot

return, os

End