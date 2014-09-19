Function os_define

; Specific path separator
pathSep = PATH_SEP()

CASE !VERSION.OS_FAMILY OF

  'unix'    : osRoot = '/'
  'Windows' : osRoot = STRMID(!DIR, 0, 2)

ENDCASE

os = {sep : pathSep, osRoot : osRoot}
return, os

End