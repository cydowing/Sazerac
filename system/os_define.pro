Function os_define

; Creating structure to return
os = {osStructure, osType:'',osRoot:'',sep:''}

; Specific path separator
pathSep = PATH_SEP()

CASE !VERSION.OS_FAMILY OF
  'unix'    : osRoot = '/'
  'Windows' : osRoot = STRMID(!DIR, 0, 2)
ENDCASE

os.osType = strlowcase(!VERSION.OS_FAMILY)
os.sep = pathSep
os.osRoot = osRoot

return, os

End