Function pointarrayclass_sazerac::init, cox, coy, coz

  Compile_opt idl2
  
  case n_params() of
    0 : begin
          
        end
    1 : begin
            if cox eq !NULL then begin
              self.pt = ptr_new(cox)
            endif else begin
              if (size(cox,/dimensions))[0] ne 3 and (size(cox,/dimensions))[1] ne 3 then begin
                print, 'Wrong size of input data...'
                print, 'Make sure that one of the dimension is 3...'
                return, 0
              endif else begin
                rowId = where(size(cox,/dimensions) eq 3, complement = ccomp)
                ; case the matrix is square -> assume that the input is in the correct order
                if n_elements(rowId) eq 2 then rowId = 1 & ccomp = 3
                case rowID of
                  0: begin
                        self.pt = ptr_new(transpose(cox))
                        self.column = ccomp
                     end
                  1: begin
                        self.pt = ptr_new(cox)
                        self.column = ccomp
                     end
                 endcase
;                if (size(cox,/dimensions))[0] gt (size(cox,/dimensions))[1] then begin
;                  self.pt = ptr_new(cox)
;                  self.column = (size(cox,/dimensions))[0]
;                  if n_elements(size(cox,/dimensions)) gt 1 then self.row = (size(cox,/dimensions))[1]
;                endif else self.pt = ptr_new(transpose(cox))
              endelse
            endelse
        end
    3 : begin
            if size(cox, /N_DIMENSIONS) ne 1 then cox = transpose(cox)
            if size(coy, /N_DIMENSIONS) ne 1 then coy = transpose(coy)
            if size(coz, /N_DIMENSIONS) ne 1 then coz = transpose(coz)
             ; It is the user duty to provid columns major array coordinates
             self.pt = ptr_new( [[cox],[coy],[coz]] )
             self.column = (size(cox,/dimensions))[0]
             self.row = 3
        end
    else : print, ' pointarrayclass_sazerac - Wrong number of elements for initialization...'
  
  endcase
  
  ; Initializing the object
  bbPoints = self.getBoundingBox(/POINTS)
  dum = self.bboxclass_sazerac::init(bbPoints[0], bbPoints[1])
  
  return, 1
  
End


Pro pointarrayclass_sazerac::cleanup

  Compile_opt idl2
  
    
End


Function pointarrayclass_sazerac::x, id

  if n_elements(id) eq 0 then begin
  
    return, (*self.pt)[*,0]
  
  endif else begin
    
    if id ge self.getDim() then print, 'Provided dimension too big...'
    
    return, (*self.pt)[id,0]
    
  endelse
  
End



Function pointarrayclass_sazerac::y, id

  if n_elements(id) eq 0 then begin
  
    return, (*self.pt)[*,1]
  
  endif else begin
    
    if id ge self.getDim() then print, 'Provided dimension too big...'
    
    return, (*self.pt)[id,1]
    
  endelse
  
End


Function pointarrayclass_sazerac::z, id

  if n_elements(id) eq 0 then begin
  
    return, (*self.pt)[*,2]
  
  endif else begin
    
    if id ge self.getDim() then print, 'Provided dimension too big...'
    
    return, (*self.pt)[id,2]
    
  endelse
  
End


Function pointarrayclass_sazerac::xyz, id

  if n_elements(id) eq 0 then begin
  
    return, (*self.pt)
  
  endif else begin
    
    if id ge self.getDim() then print, 'Provided dimension too big...'
    
    return, (*self.pt)[id,*]
    
  endelse
  
End


Function pointarrayclass_sazerac::coordinateValueByIndex, i, j
  
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


Function pointarrayclass_sazerac::setX, i, value

  if i ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif else begin
    (*self.pt)[i,0] = value
    return, 'done'
  endelse
  
End



Function pointarrayclass_sazerac::setY, i, value

  if i ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif else begin
    (*self.pt)[i,1] = value
    return, 'done'
  endelse
  
End


Function pointarrayclass_sazerac::setZ, i, value

  if i ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif else begin
    (*self.pt)[i,2] = value
    return, 'done'
  endelse
  
End


Function pointarrayclass_sazerac::extractPoint, pointId

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



Function pointarrayclass_sazerac::extractMultiplePoint, pointId

    Return, pointarrayclass_sazerac($
      (*self.pt)[pointId,0] ,$
      (*self.pt)[pointId,1] ,$
      (*self.pt)[pointId,2] $
      )

End


Function pointarrayclass_sazerac::findmin, $
                                      INDEX = INDEX, $
                                      VALUE = VALUE, $
                                      X = X, Y = Y, Z = Z

if keyword_set(X) then range = 0
if keyword_set(Y) then range = 1
if keyword_set(Z) then range = 2


if n_elements(size((*self.pt),/DIMENSIONS)) eq 1 then dum = min( (*self.pt)[range], minSub ) else $
                                                      dum = min( (*self.pt)[*,range], minSub )
  
if keyword_set(VALUE) then return, dum
if keyword_set(INDEX) then return, minsub else return, pointclass_sazerac(self.xyz(minSub))


End


Function pointarrayclass_sazerac::findmax, $
  INDEX = INDEX, $
  VALUE = VALUE, $
  X = X, Y = Y, Z = Z

  if keyword_set(X) then range = 0
  if keyword_set(Y) then range = 1
  if keyword_set(Z) then range = 2

  if n_elements(size((*self.pt),/DIMENSIONS)) eq 1 then dum = max( (*self.pt)[range], maxSub ) else $
                                                        dum = max( (*self.pt)[*,range], maxSub )

  if keyword_set(VALUE) then return, dum
  if keyword_set(INDEX) then return, maxsub else return, pointclass_sazerac(self.xyz(maxSub))


End


Function pointarrayclass_sazerac::getBoundingBox, POINTS = POINTS

ULx = self.findmax(/VALUE, /X)
ULy = self.findmax(/VALUE, /Y)
ULz = self.findmax(/VALUE, /Z)
pointUL = pointclass_sazerac(ULx, ULy, ULz)
LRx = self.findmin(/VALUE, /X)
LRy = self.findmin(/VALUE, /Y)
LRz = self.findmin(/VALUE, /Z)
pointLR = pointclass_sazerac(LRx, LRy, LRz)

if Keyword_set(POINTS) then Return, [pointUL, pointLR] else Return, bboxclass_sazerac(pointUL, pointLR)

End


Function pointarrayclass_sazerac::worldBound, POINTS = POINTS

 if Keyword_set(POINTS) then Return, self.getBoundingBox(/POINTS) else $
                             Return, self.getBoundingBox()

End



Function pointarrayclass_sazerac::findaverage, $
                                            INDEX = INDEX, $
                                            X = X, Y = Y, Z = Z

  dum = mean( (*self.pt), dimension = 1 )

  if keyword_set(X) then return, dum[0]
  if keyword_set(Y) then return, dum[1]
  if keyword_set(Z) then return,  dum[2]

End



Function pointarrayclass_sazerac::addPointToArray, pt2

return, 1

End


Function pointarrayclass_sazerac::removePointToArray, pointId

return, 1

End


Function pointarrayclass_sazerac::addVector, vector

  (*self.pt)[*,0] += vector.x()
  (*self.pt)[*,1] += vector.y()
  (*self.pt)[*,2] += vector.z()
  return, 1

End


Function pointarrayclass_sazerac::subVector, vector

  (*self.pt)[*,0] -= vector.x()
  (*self.pt)[*,1] -= vector.y()
  (*self.pt)[*,2] -= vector.z()
  return, 1
  
End


Function pointarrayclass_sazerac::sqrtDistance, point

  return, ( ((*self.pt)[*,0] - point.x())^2 + ((*self.pt)[*,1] - point.y())^2 + ((*self.pt)[*,2] - point.z())^2 )
  
End


Function pointarrayclass_sazerac::distance, point

  return, sqrt( self.sqrtDistance(point))
  
End



Function pointarrayclass_sazerac::orthogonalDistance, origin, vector

  ; TBD - not finished as not method to compute dot in an array approach
  vec = self.makeVectorArray(origin)
  return, abs(vec.dot(vector)) / vector.getnormlength()

End

;+
; This function should be called makeVectorArrayFromSinglePoint to make more sense
;-
Function pointarrayclass_sazerac::makeVectorArray, point2

  if strlowcase(obj_class(point2)) eq 'pointarrayclass_sazerac' then begin
    dum = self.makeVectorArrayFromPointArray(point2)
    return, dum
  endif else begin
    return, vectorarrayclass($
                            [replicate(point2.x(), self.column)-(*self.pt)[*,0]], $
                            [replicate(point2.y(), self.column)-(*self.pt)[*,1]], $
                            [replicate(point2.z(), self.column)-(*self.pt)[*,2]]  $
                            )
  endelse
  
End


Function pointarrayclass_sazerac::makeVectorArrayFromPointArray, point2

  if strlowcase(obj_class(point2)) eq 'pointclass_sazerac' then begin
    dum = self.makeVectorArray(point2)
    return, dum
  endif else begin
    return, vectorarrayclass($
      point2.x()-(*self.pt)[*,0], $
      point2.y()-(*self.pt)[*,1], $
      point2.z()-(*self.pt)[*,2]  $
      )
;    return, vectorarrayclass($
;      point2.x()-(*self.pt)[0,*], $
;      point2.y()-(*self.pt)[1,*], $
;      point2.z()-(*self.pt)[2,*]  $
;      )
    endelse
    
End


; With this function create a stack of vectorArray where each vectorArray holds the vectors from
;the points array to one point of point2 pointarray
Function pointarrayclass_sazerac::makeVectorArrayStackFromPointArray, point2

  dim = point2.getDim()
  oArr = objarr(dim)
  
  for i = 0, dim-1 do begin
    
    ; Extracing the boundary point
    tempPoint = pointclass_sazerac(point2.xyz(i))
    ; Duplicating the pointclass_sazerac to an pointarrayclass_sazerac
    ptArr = tempPoint.duplicateTopointarrayclass_sazerac(self.getDim())
    ; Reseting the z coordinate for 'flat' vector
    dum = ptArr.transformTo2D()
    ; Creating the vectorArrayClass
    temp = self.makeVectorArrayFromPointArray(ptArr)
    oArr[i] = temp
    
  endfor

  return, oArr

End


Function pointarrayclass_sazerac::distanceFromPointArray, vecBobj

vecA = (*self.pt)
vecB = vecBobj.xyz()

;dVecA = size(vecA, /DIMENSIONS)
;dVecB = size(vecB, /DIMENSIONS)
dVecA = self.getDim()
dVecB = vecBobj.getDim()


;  a = indgen(2,3)+23
aa = rebin((reform(transpose(VecA),1,dVecA[1],dVecA[0])),dVecB[0],dVecA[1],dVecA[0])

;  b = indgen(4,3)-3
bb = rebin(vecB, dVecB[0],dVecB[1],dVecA[0])

cc = (aa - bb)^2

tt = sqrt(total(cc,2))
mm = min(tt, minSub, DIMENSION = 1)

minDistIndex = minSub - (indgen(dVeca[0])*dvecB[0])

return, minDistIndex


End



Function pointarrayclass_sazerac::getDim

  return, [n_elements((*self.pt)[*,0]),3]
  
End


Function pointarrayclass_sazerac::transformTo2D

  (*self.pt)[*,2] = (*self.pt)[*,2] * 0.D
  return, self
  
End



Pro pointarrayclass_sazerac__define

  void = {pointarrayclass_sazerac, $
    pt       : ptr_new()       ,$     ; pointer to the points array store as fltarr(n,3)
    column   : 0UL             ,$     ; number of columns in the array
    row      : 0               ,$     ; number of rows in the array
    inherits bboxclass_sazerac $      ; Inherance of the bboclass
  }

End


