Function os_define

COMPILE_OPT idl2, HIDDEN

; Creating structure to return
os = {osStructure, osType:'',osRoot:'',sep:'',home:''}

; Specific path separator
pathSep = PATH_SEP()

CASE !VERSION.OS_FAMILY OF
  'unix'    :  begin
    osRoot = '/'
    Spawn, 'echo $HOME', homeDir
    End
  'Windows' : begin
    osRoot = STRMID(!DIR, 0, 2)
    Spawn, 'echo %USERPROFILE%', homeDir
    end
ENDCASE

os.osType = strlowcase(!VERSION.OS_FAMILY)
os.sep = pathSep
os.osRoot = osRoot
os.home = homeDir

return, os

End