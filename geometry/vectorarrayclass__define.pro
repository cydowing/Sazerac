;+
; To be revisited for a better handle or initialisation
; possibility to initialize directly with a vectorarrayclass argument
;-
Function vectorarrayclass::init, cox, coy, coz

  Compile_opt idl2

  case n_params() of
    0 : begin
          self.pt = ptr_new(/allocate_heap)
          self.column = 0UL
          self.row = 0
        end
    1 : begin
          if strlowcase(obj_class(cox)) ne 'vectorclass' then begin
            ; checking if it a matrix
            tempSize = size(cox,/dimensions)
            if n_elements(tempSize) eq 1 then begin
              print, 'Wrong size of input data...'
              print, 'Make sure that one of the dimension is 3...'
              return, 0
            endif else begin
            dum = where(size(cox,/dimensions) eq 3, count, complement = comp, /L64)
            if count eq 0 then begin
              print, 'Wrong size of input data...'
              print, 'Make sure that one of the dimension is 3...'
              return, 0
            endif else begin
              if dum[0] eq 0 then begin
                self.pt = ptr_new(transpose(cox))
                self.column = dum[1]
                self.row = dum[0]
              endif else begin
                self.pt = ptr_new(cox)
                self.column = dum[0]
                self.row = dum[1]
              endelse
            endelse
          endelse
         endif else begin
              self.pt = ptr_new( transpose(cox.xyz()) )
              self.column = 1
              self.row = 3
         endelse
        end
        
    3 : begin
          if (size(cox,/dimensions))[0] ne (size(coy,/dimensions))[0] or $
             (size(cox,/dimensions))[0] ne (size(coz,/dimensions))[0] then begin
             print, 'Wrong size of input data...'
             return, 0
          endif else begin
             ; It is the user duty to provid columns major array coordinates
             self.pt = ptr_new( [[cox],[coy],[coz]] )
             self.column = (size(cox,/dimensions))[0]
             self.row = 3
          endelse
        end
    else : print, 'Wrong number of elements for initialization...'
  
  endcase
  
  ; Initializing the object
  return, 1
  
End


Pro vectorarrayclass::cleanup

  Compile_opt idl2
  
End





Function vectorarrayclass::x

  return, (*self.pt)[*,0]
  
End



Function vectorarrayclass::y

  return, (*self.pt)[*,1]
  
End


Function vectorarrayclass::z

  return, (*self.pt)[*,2]
  
End


Function vectorarrayclass::xyz

  return, (*self.pt)
  
End


Function vectorarrayclass::coordinateValueByIndex, i, j

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


Function vectorarrayclass::setX, i, value

  if i ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif else begin
    (*self.pt)[i,0] = value
    return, 'done'
  endelse
  
End



Function vectorarrayclass::setY, i, value

  if i ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif else begin
    (*self.pt)[i,1] = value
    return, 'done'
  endelse
  
End


Function vectorarrayclass::setZ, i, value

  if i ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif else begin
    (*self.pt)[i,2] = value
    return, 'done'
  endelse
  
End


Function vectorarrayclass::extractPoint, pointId

  if pointId ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif else begin
    return, pointclass($
                      self.coordinateValueByIndex(pointId,0) ,$
                      self.coordinateValueByIndex(pointId,1) ,$
                      self.coordinateValueByIndex(pointId,2)  $
                      )
  endelse
  
End


Function vectorarrayclass::translate, vector2

  (*self.pt)[*,0] += vector2.x()
  (*self.pt)[*,1] += vector2.y()
  (*self.pt)[*,2] += vector2.z()
  return, 1

End


Function vectorarrayclass::scaleUp, vector2

  (*self.pt)[*,0] *= vector2.x()
  (*self.pt)[*,1] *= vector2.y()
  (*self.pt)[*,2] *= vector2.z()
  return, 1


End


Function vectorarrayclass::scaleDown, vector2

  temp = 1. / vector2.xyz()
  (*self.pt)[*,0] *= temp[0]
  (*self.pt)[*,1] *= temp[1]
  (*self.pt)[*,2] *= temp[2]
  return, 1
  
  
End


Function vectorarrayclass::opposite

  return, -(*self.pt)

End


Function vectorarrayclass::length

  return, sqrt( ((*self.pt)[*,0])^2 + ((*self.pt)[*,1])^2 + ((*self.pt)[*,2])^2 )
  
end




Function vectorarrayclass::horizLength

  return, sqrt( ((*self.pt)[*,0])^2 + ((*self.pt)[*,1])^2 )

End



Function vectorarrayclass::squareLength

  return, ((*self.pt)[*,0])^2 + ((*self.pt)[*,1])^2 + ((*self.pt)[*,2])^2
 
end


Function vectorarrayclass::normalizeLength

  inv = 1. / self.length()
  dinv = [[inv],[inv],[inv]]
  (*self.pt) *= dinv
  dinv = !NULL
  return, 1
  
End




Function vectorarrayclass::normalizeLengthBy, value

  inv = 1. / value
  dinv = replicate(inv, self.column, self.row)
  (*self.Pt) *= dinv
  dinv = !NULL
  Return, 1

End




Function vectorarrayclass::getNormLength

  inv = 1. / self.length()
  return, ( (*self.pt) * inv )
  
End



Pro vectorarrayclass::normalizeHorizLength

  inv = 1. / self.horizlength()
  ((*self.pt)[*,0:1]) *= inv
  
End



Function vectorarrayclass::absDot, vector2

  dot = abs( ((*self.pt)[*,0] * vector2.x()) + ((*self.pt)[*,1] * vector2.y()) + ((*self.pt)[*,2] * vector2.z()) )
  return, dot

End


Function vectorarrayclass::Dot, vector2

  dot = ((*self.pt)[*,0] * vector2.x()) + ((*self.pt)[*,1] * vector2.y()) + ((*self.pt)[*,2] * vector2.z())
  return, dot

End


Function vectorarrayclass::getNormHorizLength

  inv = 1. / self.horizlength()
  return, ( ((*self.pt)[*,0:1]) * inv )
  
End


Function vectorarrayclass::getRadAngle, vector2

  return, acos( self.dot(vector2) / ( self.length() * vector2.length() ) )
  
End


Function vectorarrayclass::getDegAngle, vector2

  return, 180./!PI * acos( self.dot(vector2) / ( self.length() * vector2.length() ) )
  
End


Function vectorarrayclass::det, vector2

  detX = ((*self.pt)[*,1] * vector2.z()) - ((*self.pt)[*,2] * vector2.y())
  detY = ((*self.pt)[*,2] * vector2.x()) - ((*self.pt)[*,0] * vector2.z())
  detZ = ((*self.pt)[*,0] * vector2.y()) - ((*self.pt)[*,1] * vector2.x())
;  print, detX
;  print, detY
;  print, detZ
  return, vectorarrayclass(detX, detY, detZ)

End


Function vectorarrayclass::paralVolume, vector2

  tempvec = vectorarrayclass(self.det(vector2))
  return, tempvec.length()

End



Function vectorarrayclass::create2Dvector

  return, vectorarrayclass((*self.pt)[*,0],(*self.pt)[*,1],(*self.pt)[*,2]*0.0)

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
Function vectorarrayclass::localCoordinateSystem

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
  v2 = vectorarrayclass(tempv2[*,0], tempv2[*,1], tempv2[*,2])
  
  ;v3 = fltarr(self.column)
  print, 'v3'
  v3 = vectorarrayclass(self.det(v2))

  ;with this line the 3 vectors coordinates are return
  ;return, [ [self.xyz()],[v2.xyz()],[v3.xyz()] ] 
  
  ; with this line the actual vector objects are return - most likely more efficient
  return, [ v2, v3 ]
  
End



Function vectorarrayclass::getSubArray, index

  Return, vectorarrayclass($
                  (*self.Pt)[index,0],$
                  (*self.Pt)[index,1],$
                  (*self.Pt)[index,2] $
                  )

End



Pro vectorarrayclass__define

  void = {vectorarrayclass, $
    pt       : ptr_new() ,$  ; pointer to the points array store as fltarr(n,3)
    column   : 0UL       ,$  ; number of columns in the array
    row      : 0          $  ; number of rows in the array
  }

End

