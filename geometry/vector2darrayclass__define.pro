;+
; To be revisited for a better handle or initialisation
; possibility to initialize directly with a vector2darrayclass argument
;-
Function vector2darrayclass::init, cox, coy

  Compile_opt idl2

  self.pt = ptr_new( [[cox],[coy]] )
  self.column = (size(cox,/dimensions))[0]
  self.row = 2

;  case n_params() of
;    0 : begin
;          self.pt = ptr_new(/allocate_heap)
;          self.column = 0
;          self.row = 0
;        end
;    1 : begin
;          if size(cox, /TYPE) ne 8 then begin
;            ; checking if it a matrix
;            tempSize = size(cox,/dimensions)
;            if n_elements(tempSize) eq 1 then begin
;              print, 'Wrong size of input data...'
;              print, 'Make sure that one of the dimension is 3...'
;              return, 0
;            endif else begin
;              dum = where(tempSize eq 2, count, complement = comp)
;              if count eq 0 then begin
;                print, 'Wrong size of input data...'
;                print, 'Make sure that one of the dimension is 2...'
;                return, 0
;              endif else begin
;                if dum[0] eq 0 then begin
;                  self.pt = ptr_new(transpose(cox))
;                  self.column = tempSize[1]
;                  self.row = tempSize[0]
;                endif else begin
;                  self.pt = ptr_new(cox)
;                  self.column = tempSize[0]
;                  self.row = tempSize[1]
;                endelse
;              endelse
;            endelse
;         endif else begin
;          dim = size(cox.xyz(), /dimensions)
;          self.pt = ptr_new( cox.xyz() )
;          self.column = dim[0]
;          self.row = dim[1]
;         endelse
;        end
;        
;    2 : begin
;          if (size(cox,/dimensions))[0] ne (size(coy,/dimensions))[0] then begin
;             print, 'Wrong size of input data...'
;             return, 0
;          endif else begin
;             ; It is the user duty to provid columns major array coordinates
;             self.pt = ptr_new( [[cox],[coy]] )
;             self.column = (size(cox,/dimensions))[0]
;             self.row = 2
;          endelse
;        end
;    else : print, 'Wrong number of elements for initialization...'
;  
;  endcase

  
  ; Initializing the object
  return, 1
  
End




Pro vector2darrayclass::cleanup

  Compile_opt idl2
  
End


Function vector2darrayclass::x, value

  return, (*self.pt)[*,0]
  
End



Function vector2darrayclass::y

  return, (*self.pt)[*,1]
  
End



Function vector2darrayclass::xy

  return, (*self.pt)
  
End


Function vector2darrayclass::coordinateValueByIndex, i, j

  if i ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif
  
  if j ge self.row then begin
    print, 'Row value outside of data range...'
    return, 0
  endif
  
  return, (*self.pt)[i,j]
  
End


Function vector2darrayclass::setX, i, value

  if i ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif else begin
    (*self.pt)[i,0] = value
    return, 'done'
  endelse
  
End



Function vector2darrayclass::setY, i, value

  if i ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif else begin
    (*self.pt)[i,1] = value
    return, 'done'
  endelse
  
End



Function vector2darrayclass::extractPoint, pointId

  if pointId ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif else begin
    return, pointclass_sazerac($
                      self.coordinateValueByIndex(pointId,0) ,$
                      self.coordinateValueByIndex(pointId,1) ,$
                      self.coordinateValueByIndex(pointId,2)  $
                      )
  endelse
  
End


Function vector2darrayclass::translate, vector2

  (*self.pt)[*,0] += vector2.x()
  (*self.pt)[*,1] += vector2.y()
  (*self.pt)[*,2] += vector2.z()
  return, 1

End


Function vector2darrayclass::scaleUp, vector2

  (*self.pt)[*,0] *= vector2.x()
  (*self.pt)[*,1] *= vector2.y()
  (*self.pt)[*,2] *= vector2.z()
  return, 1


End


Function vector2darrayclass::scaleDown, vector2

  temp = 1. / vector2.xyz()
  (*self.pt)[*,0] *= temp[0]
  (*self.pt)[*,1] *= temp[1]
  (*self.pt)[*,2] *= temp[2]
  return, 1
  
  
End


Function vector2darrayclass::opposite

  return, -(*self.pt)

End


Function vector2darrayclass::length

  return, sqrt( ((*self.pt)[*,0])^2 + ((*self.pt)[*,1])^2 + ((*self.pt)[*,2])^2 )
  
end



Function vector2darrayclass::horizLength

  return, sqrt( ((*self.pt)[*,0])^2 + ((*self.pt)[*,1])^2 )
  
end


Function vector2darrayclass::squareLength

  return, ((*self.pt)[*,0])^2 + ((*self.pt)[*,1])^2 + ((*self.pt)[*,2])^2
 
end


Pro vector2darrayclass::normalizeLength

  inv = 1. / self.length()
  (*self.pt) *= inv
  
End


Function vector2darrayclass::getNormLength

  inv = 1. / self.length()
  return, ( (*self.pt) * inv )
  
End


Function vector2darrayclass::dot, vector2

  dot = ((*self.pt)[*,0] * vector2.x()) + ((*self.pt)[*,1] * vector2.y()) + ((*self.pt)[*,2] * vector2.z())
  return, dot
  
End


Function vector2darrayclass::getRadAngle, vector2

  return, acos( self.dot(vector2) / ( self.length() * vector2.length() ) )
  
End


Function vector2darrayclass::getDegAngle, vector2

  return, 180./!PI * acos( self.dot(vector2) / ( self.length() * vector2.length() ) )
  
End


Function vector2darrayclass::det, vector2

  detX = ((*self.pt)[*,1] * vector2.z()) - ((*self.pt)[*,2] * vector2.y())
  detY = ((*self.pt)[*,2] * vector2.x()) - ((*self.pt)[*,0] * vector2.z())
  detZ = ((*self.pt)[*,0] * vector2.y()) - ((*self.pt)[*,1] * vector2.x())
;  print, detX
;  print, detY
;  print, detZ
  return, vector2darrayclass(detX, detY, detZ)

End


Function vector2darrayclass::paralVolume, vector2

  tempvec = vector2darrayclass(self.det(vector2))
  return, tempvec.length()

End


;+
; a function that creates a local coordinate system
; based on the vector hold in self.
; 
; IDL> print, vec6.localCoordinateSystem()
; <ObjHeapVar6(VECTORCLASS)><ObjHeapVar7(VECTORCLASS)>
; IDL> t = vec6.localCoordinateSystem()
; IDL> print, t[0].xyz()
; 0.0000000     0.062378287     -0.99805260
;-
Function vector2darrayclass::localCoordinateSystem

  tempv2 = fltarr(self.column,3)

  id = where( (*self.pt)[*,0] gt (*self.pt)[*,1], count, complement = compId )
  
  if count gt 0 then begin
    invLen = 1. / sqrt( ((*self.pt)[id,0])^2 + ((*self.pt)[id,2])^2 )
    tempv2[id,0] = -(*self.pt)[id,2] * invLen
    tempv2[id,1] = 0.0
    tempv2[id,2] = (*self.pt)[id,0] * invLen
  endif
  if n_elements(compId) gt 0 then begin
    invLen = 1. / sqrt( ((*self.pt)[compId,0])^2 + ((*self.pt)[compId,2])^2 )
    tempv2[compId,0] = 0.0
    tempv2[compId,1] = -(*self.pt)[compId,2] * invLen
    tempv2[compId,2] = (*self.pt)[compId,0] * invLen
  endif
  print, tempv2[*,0]
  print, tempv2[*,1]
  print, tempv2[*,2]
  print, 'v2'
  v2 = vector2darrayclass(tempv2[*,0], tempv2[*,1], tempv2[*,2])
  
  ;v3 = fltarr(self.column)
  print, 'v3'
  v3 = vector2darrayclass(self.det(v2))

  ;with this line the 3 vectors coordinates are return
  ;return, [ [self.xyz()],[v2.xyz()],[v3.xyz()] ] 
  
  ; with this line the actual vector objects are return - most likely more efficient
  return, [ v2, v3 ]
  
End



Pro vector2darrayclass__define

  void = {vector2darrayclass, $
    pt       : ptr_new() ,$  ; pointer to the points array store as fltarr(n,2)
    column   : 0         ,$  ; number of columns in the array
    row      : 0          $  ; number of rows in the array
  }

End

