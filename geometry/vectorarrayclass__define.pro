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
      if size(cox,/TYPE) ne 11 then begin
        ; checking if it a matrix
        tempSize = size(cox,/dimensions)
        if n_elements(tempSize) eq 1 then begin
          print, 'Wrong size of input data...'
          print, 'Make sure that one of the dimension is 3...'
          return, 0
        endif else begin
          dum = where(tempSize eq 3, count)
          if count eq 0 then begin
            print, 'Wrong size of input data...'
            print, 'Make sure that one of the dimension is 3...'
            return, 0
          endif else begin
            if dum[0] eq 0 then begin
              self.pt = ptr_new(transpose(cox))
              self.column = tempSize[1]
              self.row = tempSize[0]
            endif else begin
              self.pt = ptr_new(cox)
              self.column = tempSize[0]
              self.row = tempSize[1]
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



;+
; This is an overload of the + function
; Can remplace the translate method
;-
Function vectorarrayclass::_overloadPlus, Left, Right

  if Strlowcase(Obj_class(Left)) ne Strlowcase(Obj_class(Right)) then begin
    Print, 'You try to add to differents objects types...'
    Return, 0
  endif

  if self.n() ne right.n() then begin
    Print, "The vectors array don't have the same size..."
    Return, 0
  endif


  Return, vectorarrayclass(self.xyz() + Right.xyz())

End



;+
; This is an overload of the + function
; Can remplace the translate method
;-
Function vectorarrayclass::_overloadMinus, Left, Right

  if Strlowcase(Obj_class(Left)) ne Strlowcase(Obj_class(Right)) then begin
    Print, 'You try to add to differents objects types...'
    Return, 0
  endif

  if self.n() ne right.n() then begin
    Print, "The vectors array don't have the same size..."
    Return, 0
  endif

  Return, vectorclass(self.xyz() - Right.xyz())

End



;+
; This is an overload of the * function
; Can remplace the translate method
;-
Function vectorarrayclass::_overloadAsterisk, Left, Right

  if Strlowcase(Obj_class(Left)) ne Strlowcase(Obj_class(Right)) then begin
    Print, 'You try to add to differents objects types...'
    Return, 0
  endif

  if self.n() ne right.n() then begin
    Print, "The vectors array don't have the same size..."
    Return, 0
  endif

  Return, vectorclass(self.xyz() * Right.xyz())

End



;+
; This is an overload of the / function
; Can remplace the translate method
;-
Function vectorarrayclass::_overloadSlash, Left, Right

  if Strlowcase(Obj_class(Left)) ne Strlowcase(Obj_class(Right)) then begin
    Print, 'You try to add to differents objects types...'
    Return, 0
  endif

  if self.n() ne right.n() then begin
    Print, "The vectors array don't have the same size..."
    Return, 0
  endif

  Return, vectorclass(self.xyz() / Right.xyz())

End



Function vectorarrayclass::addToAverage, Right

  if Strlowcase(Obj_class(self)) ne Strlowcase(Obj_class(Right)) then begin
    Print, 'You try to add to differents objects types...'
    Return, 0
  endif

  if self.n() ne right.n() then begin
    Print, "The vectors array don't have the same size..."
    Return, 0
  endif

  (self.xyz()) += Right.xyz()

  Return, 1

End



Function vectorarrayclass::x, id

  if n_elements(id) eq 0 then begin

    return, (*self.pt)[*,0]

  endif else begin

    if id ge self.n() then print, 'Provided dimension too big...'

    return, (*self.pt)[id,0]

  endelse

End



Function vectorarrayclass::y, id

  if n_elements(id) eq 0 then begin

    return, (*self.pt)[*,1]

  endif else begin

    if id ge self.n() then print, 'Provided dimension too big...'

    return, (*self.pt)[id,1]

  endelse

End


Function vectorarrayclass::z, id

  if n_elements(id) eq 0 then begin

    return, (*self.pt)[*,2]

  endif else begin

    if id ge self.n() then print, 'Provided dimension too big...'

    return, (*self.pt)[id,2]

  endelse

End



Function vectorarrayclass::xyz, id

  if n_elements(id) eq 0 then begin

    return, (*self.pt)

  endif else begin

    if id ge self.n() then print, 'Provided dimension too big...'

    return, (*self.pt)[id,*]

  endelse

End



Function vectorarrayclass::n

  Return, self.column

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


Function vectorarrayclass::extractVector, pointId

  if pointId ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif else begin
    return, vectorclass($
      self.x(pointId) ,$
      self.y(pointId) ,$
      self.z(pointId)  $
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


Function vectorarrayclass::opposite, index = index

  if keyword_set(index) then begin
    temp = (*self.pt)
    temp[index, *] *= -1.
    return, vectorarrayclass(temp)
  endif else return, -(*self.pt)

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


Function vectorarrayclass::dot, vector2

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

  if n_elements(index) eq 1 then begin
    Return, vectorclass($
      (*self.Pt)[index,0],$
      (*self.Pt)[index,1],$
      (*self.Pt)[index,2] $
      )

  endif else begin
    Return, vectorarrayclass($
      (*self.Pt)[index,0],$
      (*self.Pt)[index,1],$
      (*self.Pt)[index,2] $
      )
  endelse

End



Pro vectorarrayclass__define

  void = {vectorarrayclass, $
    pt       : ptr_new()        ,$  ; pointer to the points array store as fltarr(n,3)
    column   : 0UL             ,$  ; number of columns in the array
    row      : 0                      ,$  ; number of rows in the array
    inherits IDL_Object     $ ; Override the operators
  }

End

