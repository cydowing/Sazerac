Function transformationarrayclass::init, MATRIX = MATRIX, INVMATRIX = INVMATRIX, DIMENSION = DIMENSIONS

  Compile_opt idl2

  case 1 of
    keyword_set(MATRIX) eq 1 AND keyword_set(MATRIX) eq 1: self.dimension = (size(matrix, /DIMENSION))[0]
    keyword_set(DIMENSIONS) eq 1: self.dimension = DIMENSIONS 
    else: self.dimension = 1
  endcase
  
  if keyword_set(MATRIX) then self.matrix = ptr_new(MATRIX) else begin
    matrix = dblarr(self.dimension,4,4)
    matrix[*,0,0] = 1
    matrix[*,1,1] = 1
    matrix[*,2,2] = 1
    matrix[*,3,3] = 1
    self.matrix = ptr_new(matrix)
  endelse
  
  if keyword_set(INVMATRIX) then self.invmatrix = ptr_new(INVMATRIX) else begin
    invmatrix = dblarr(self.dimension,4,4)
    invmatrix[*,0,0] = 1
    invmatrix[*,1,1] = 1
    invmatrix[*,2,2] = 1
    invmatrix[*,3,3] = 1
    self.invmatrix = ptr_new(matrix)
  endelse
 
  ; Initializing the object
  return, 1
  
End


Pro transformationarrayclass::cleanup

  Compile_opt idl2
  
;  ptr_free, $
;    self.world
    
End


Function transformationarrayclass::getMatrix

  return, *(self.matrix)
  
End


Function transformationarrayclass::getInvMatrix

  return, *(self.invmatrix)
  
End


Function transformationarrayclass::invertTransformation

  return, transformationarrayclass(self.invmatrix, self.matrix)
  
End


;-----------------
;- To Be Adapted -
;-----------------
;+
; Not sure this is 100% correct, need more investigations on the subject
; It just consider that the substration of the two transformation matrix
; should gives a null matrix ?!
;-
Function transformationarrayclass::transformEquality, transform

  if strlowcase(obj_class(transform)) ne 'transformationarrayclass' then begin
    print, 'Please provide a transformation class object (transformationarrayclass)...'
    return, 0
  endif else begin
    if total( self.matrix - transform.getmatrix() ) eq 0 then return, 1 else return, 0
  endelse
    
End


;-----------------
;- To Be Adapted -
;-----------------
;+
; Not sure this is 100% correct, need more investigations on the subject
; It just consider that the substration of the two transformation matrix
; should gives a null matrix ?!
;-
Function transformationarrayclass::isIdentity

  if total( self.matrix - identity(4) ) eq 0 then return, 1 else return, 0
  
End


Function transformationarrayclass::translate, vector2

  if strlowcase(obj_class(vector2)) ne 'vectorarrayclass' then begin
    print, 'Please provide a vector array class object (vectorarrayclass)...'
    return, 0
  endif else begin
    matrix = *(self.matrix)
    matrix[0,3,0] = vector2.x()
    matrix[0,3,1] = vector2.y()
    matrix[0,3,2] = vector2.z()
    
    invmatrix = *(self.matrix)
    invmatrix[0,3,0] = -vector2.x()
    invmatrix[0,3,1] = -vector2.y()
    invmatrix[0,3,2] = -vector2.z()
    
;    Original
;    matrix = [$
;      [ 1, 0, 0, vector2.x() ],$
;      [ 0, 1, 0, vector2.y() ],$
;      [ 0, 0, 1, vector2.z() ],$
;      [ 0, 0, 0, 1 ] $
;      ]
;    invmatrix = [$
;      [ 1, 0, 0, -vector2.x() ],$
;      [ 0, 1, 0, -vector2.y() ],$
;      [ 0, 0, 1, -vector2.z() ],$
;      [ 0, 0, 0, 1 ] $
;      ]
    return, transformationarrayclass(MATRIX = matrix, INVMATRIX = invmatrix, DIMENSION = self.dimension)
  endelse
  
End


Function transformationarrayclass::scale, sx, sy, sz

  if n_params() ne 3 then begin
    print, 'Please provide one scale value for each axis...'
    return, 0
  endif else begin
    
    matrix = *(self.matrix)
    matrix[*,0,0] = sx
    matrix[*,1,1] = sy
    matrix[*,2,2] = sz
    
    invmatrix = *(self.matrix)
    invmatrix[*,3,0] = 1./sx
    invmatrix[*,3,1] = 1./sy
    invmatrix[*,3,2] = 1./sz    
    
;    matrix = [$
;      [ sx, 0, 0, 0 ],$
;      [ 0, sy, 0, 0 ],$
;      [ 0, 0, sz, 0 ],$
;      [ 0, 0, 0, 1 ] $
;      ]
;    invmatrix = [$
;      [ 1./sx, 0, 0, 0 ],$
;      [ 0, 1./sy, 0, 0 ],$
;      [ 0, 0, 1./sz, 0 ],$
;      [ 0, 0, 0, 1 ] $
;      ]

    return, transformationarrayclass(MATRIX = matrix, INVMATRIX = invmatrix)
  endelse
  
End


;-----------------
;- To Be Adapted -
;-----------------
;+
; Unifinished method as I'm not sure what the exact implementation is
; from book physically based rendering, p.80
;-
Function transformationarrayclass::hasScale

  la2 = ( self.matrix * (vectorclass(1,0,0)).squareLength() )
  print, la2
  lb2 = ( self.matrix * (vectorclass(0,1,0)).squareLength() )
  print, lb2
  lc2 = ( self.matrix * (vectorclass(0,0,1)).squareLength() )
  print, lc2
  
  return, 1
  
End


Function transformationarrayclass::rotateX, angle

  if n_params() ne 1 then begin
    print, 'Please provide one rotation angle...'
    return, 0
  endif else begin
    tcos = cos(angle)
    tsin = sin(angle)
    
    matrix = *(self.matrix)
    matrix[*,1,1] = tcos
    matrix[*,2,1] = (-tsin)
    matrix[*,1,2] = tsin
    matrix[*,2,2] = tcos
    
;    matrix = [$
;      [ 1, 0, 0, 0 ],$
;      [ 0, tcos, -tsin, 0 ],$
;      [ 0, tsin, tcos,  0 ],$
;      [ 0, 0, 0, 1 ] $
;      ]

    return, transformationarrayclass(MATRIX=matrix, INVMATRIX=transpose(matrix, [0,2,1]))
  endelse
End


Function transformationarrayclass::rotateY, angle

  if n_params() ne 1 then begin
    print, 'Please provide one rotation angle...'
    return, 0
  endif else begin
    tcos = cos(angle)
    tsin = sin(angle)
    
    matrix = *(self.matrix)
    matrix[*,0,0] = tcos
    matrix[*,2,0] = tsin
    matrix[*,0,2] = -tsin
    matrix[*,2,2] = tcos

    return, transformationarrayclass(MATRIX=matrix, INVMATRIX=transpose(matrix, [0,2,1]))
    
;    matrix = [$
;      [ tcos, 0, tsin, 0 ],$
;      [ 0, 1, 0, 0 ],$
;      [ -tsin, 0, tcos, 0 ],$
;      [ 0, 0, 0, 1 ] $
;      ]
;    return, transformationarrayclass(matrix, transpose(matrix))


   endelse
  
End

Function transformationarrayclass::rotateZ, angle

  if n_params() ne 1 then begin
    print, 'Please provide one rotation angle...'
    return, 0
  endif else begin
    tcos = cos(angle)
    tsin = sin(angle)
    
    matrix = *(self.matrix)
    matrix[*,0,0] = tcos
    matrix[*,1,0] = -tsin
    matrix[*,0,1] = tsin
    matrix[*,1,1] = tcos
    
;    matrix = [$
;      [ tcos, -tsin, 0, 0 ],$
;      [ tsin, tcos, 0,  0 ],$
;      [ 0, 0, 1, 0 ],$
;      [ 0, 0, 0, 1 ] $
;      ]

    return, transformationarrayclass(MATRIX=matrix, INVMATRIX=transpose(matrix, [0,2,1]))
   endelse
   
End

;-----------------
;- To Be Adapted -
;-----------------
Function transformationarrayclass::rotateAnyAxis, angle, vecAxis

  if n_elements(angle) eq 0 then begin
    print, 'Please provide an angle value...'
    return, 0
  endif
  if strlowcase(obj_class(vectorAxis)) ne 'vectorclass' then begin 
    print, 'Please provide one rotation angle...'
    return, 0
  endif
  
  vecAxis.normalizeLength
  s = sin(angle)
  c = cos(angle)
  
  mat = fltarr(4,4)
  mat[0,0] = vecAxis.x() * vecAxis.x() + ( 1. - vecAxis.x() * vecAxis.x() ) * c
  mat[1,0] = vecAxis.x() * vecAxis.y() * ( 1. - c ) - vecAxis.z() * s
  mat[2,0] = vecAxis.x() * vecAxis.z() * ( 1. - c ) + vecAxis.y() * s
  mat[3,0] = 0
  
  mat[0,1] = vecAxis.x() * vecAxis.y() * ( 1. - c ) + vecAxis.z() * s
  mat[1,1] = vecAxis.y() * vecAxis.y() + ( 1. - vecAxis.y() * vecAxis.y() ) * c
  mat[2,1] = vecAxis.y() * vecAxis.z() * ( 1. - c ) - vecAxis.x() * s
  mat[3,1] = 0
  
  mat[0,2] = vecAxis.x() * vecAxis.z() * ( 1. - c ) - vecAxis.y() * s
  mat[1,2] = vecAxis.y() * vecAxis.z() * ( 1. - c ) + vecAxis.x() * s
  mat[2,2] = vecAxis.z() * vecAxis.z() + ( 1. - vecAxis.z() * vecAxis.z() ) * c
  mat[3,2] = 0
  
  mat[0,3] = 0
  mat[1,3] = 0
  mat[2,3] = 0
  mat[3,3] = 1
  
  return, transformationarrayclass(mat, transpose(mat))
End



;-----------------
;- To Be Adapted -
;-----------------
Function transformationarrayclass::lookAt, camPos, camAim, upVec
  
  if n_params() ne 3 then begin
    print, 'Please provide three values...'
    return, 0
  endif
  if strlowcase(obj_class(camPos)) ne 'pointclass_sazerac' then begin
    print, 'Please provide a point class object (pointclass_sazerac)...'
    return, 0
  endif
  if strlowcase(obj_class(camAim)) ne 'pointclass_sazerac' then begin
    print, 'Please provide a point class object (pointclass_sazerac)...'
    return, 0
  endif
  if strlowcase(obj_class(upVec)) ne 'vectorclass' then begin
    print, 'Please provide a vector class object (vectorclass)...'
    return, 0
  endif
  
  mat = fltarr(4,4)
  
  ; Initializing the camera position
  mat[3,0] = camPos.x()
  mat[3,1] = camPos.y()
  mat[3,2] = camPos.z()
  mat[3,3] = 1
  
  dir = (camPos.makeVector(camAim))
  dir = (camPos.makeVector(camAim))
  dir.normalizeLength
  upVec.normalizeLength
  ; print, upVec, dir
  left = vectorclass(upVec.det(dir))
  left.normalizeLength
  newUp = vectorclass(dir.det(left))
  newUp.normalizeLength
  
  mat[0,0] = left.x()
  mat[0,1] = left.y()
  mat[0,2] = left.z()
  mat[0,3] = 0.
  
  mat[1,0] = newUp.x()
  mat[1,1] = newUp.y()
  mat[1,2] = newUp.z()
  mat[1,3] = 0.
  
  mat[2,0] = dir.x()
  mat[2,1] = dir.y()
  mat[2,2] = dir.z()
  mat[2,3] = 0.
  
  print,'Matrice debbug...'
  print, invert(mat)
  print, 'Matrice inverse...'
  print, mat
  
  return, transformationarrayclass(invert(mat), mat)
  

End


;-----------------
;- To Be Adapted -
;-----------------
;+
; This function transform a 3 coordinates point into a 4 coordinates point
; by adding an homogeneous weight (1 by default)
;-
Function transformationarrayclass::transfPoint, point, weight

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(point)) ne 'pointclass_sazerac' then begin
    print, 'Please provide a point class object (pointclass_sazerac)...'
    return, 0
  endif else begin
    if n_elements(weight) ne 0 then p = transpose([point.xyz(),weight]) $
          else p = transpose([point.xyz(),1.])
    return, p
  endelse

End



;-----------------
;- To Be Adapted -
;-----------------
;+
; This function transform a 3 coordinates point into a 4 coordinates point
; by adding an homogeneous weight (1 by default)
;-
Function transformationarrayclass::transfVector, vec, weight

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(vec)) ne 'vectorclass' then begin
    print, 'Please provide a vector class object (vectorclass)...'
    return, 0
  endif else begin
    if n_elements(weight) ne 0 then v = transpose([vec.xyz(),weight]) $
          else v = transpose([vec.xyz(),1.])
    return, v
  endelse
  
End


;-----------------
;- To Be Adapted -
;-----------------
;+
; This function transform a 3 coordinates point into a 4 coordinates point
; by adding an homogeneous weight (1 by default)
;-
Function transformationarrayclass::transfPointArray, point, weight

;  if n_params() ne 1 then begin
;    print, 'Please provide a value...'
;    return, 0
;  endif
  if strlowcase(obj_class(point)) ne 'pointarrayclass_sazerac' then begin
    print, 'Please provide a point array class object (pointarrayclass_sazerac)...'
    return, 0
  endif else begin
    temp = size(point.xyz(),/dimensions)
    p = fltarr(temp[0],temp[1]+1)
    if n_elements(weight) ne 0 then begin
      p[0,0] = point.xyz()
      p[0,temp[1]] = replicate(weight,temp[0])
    endif else begin
      p[0,0] = point.xyz()
      p[0,temp[1]] = replicate(1.,temp[0])
    endelse
    return, p
  endelse

End


;-----------------
;- To Be Adapted -
;-----------------
;+
; This function transform a 3 coordinates point into a 4 coordinates point
; by adding an homogeneous weight (1 by default)
;-
Function transformationarrayclass::transfVectorArray, vec, weight

;  if n_params() ne 1 then begin
;    print, 'Please provide a value...'
;    return, 0
;  endif
  if strlowcase(obj_class(vec)) ne 'vectorarrayclass' then begin
      print, 'Please provide a vector array class object (vectorarrayclass)...'
      return, 0
    endif else begin
      temp = size(vec.xyz(),/dimensions)
      p = fltarr(temp[0],temp[1]+1)
      if n_elements(weight) ne 0 then begin
        p[0,0] = vec.xyz()
        p[0,temp[1]] = replicate(weight,temp[0])
      endif else begin
        p[0,0] = vec.xyz()
        p[0,temp[1]] = replicate(1.,temp[0])
      endelse
      return, p
    endelse
  
End



;-----------------
;- To Be Adapted -
;-----------------
Function transformationarrayclass::pointTransform, point

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(point)) ne 'pointclass_sazerac' then begin
    print, 'Please provide a point class object (pointclass_sazerac)...'
    return, 0
  endif else begin
    p = self.transfPoint(point)
    x = p[0]
    y = p[1]
    z = p[2]
    w = p[3]
    if w ne 1 then invw = 1. / w
    newx = self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z + self.matrix[3,0] * w
    newy = self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z + self.matrix[3,1] * w
    newz = self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z + self.matrix[3,2] * w
    neww = self.matrix[0,3] * x + self.matrix[1,3] * y + self.matrix[2,3] * z + self.matrix[3,3] * w
    if w eq 1 then return, pointclass_sazerac(newx, newy, newz) else return, pointclass_sazerac( ([newx, newy, newz]*invw) )
  endelse
  
End



;-----------------
;- To Be Adapted -
;-----------------
Function transformationarrayclass::pointTransformWithPtr, point

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(point)) ne 'pointclass_sazerac' then begin
    print, 'Please provide a point class object (pointclass_sazerac)...'
    return, 0
  endif else begin
    p = self.transfPoint(point)
    x = p[0]
    y = p[1]
    z = p[2]
    w = p[3]
    if w ne 1 then invw = 1. / w
    newx = self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z + self.matrix[3,0] * w
    newy = self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z + self.matrix[3,1] * w
    newz = self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z + self.matrix[3,2] * w
    neww = self.matrix[0,3] * x + self.matrix[1,3] * y + self.matrix[2,3] * z + self.matrix[3,3] * w
    if w eq 1 then return, ptr_new(pointclass_sazerac(newx, newy, newz)) else return, ptr_new(pointclass_sazerac( ([newx, newy, newz]*invw) ))
  endelse
  
End



;-----------------
;- To Be Adapted -
;-----------------
Function transformationarrayclass::pointArrayTransform, point

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(point)) ne 'pointarrayclass_sazerac' then begin
    print, 'Please provide a point array class object (pointarrayclass_sazerac)...'
    return, 0
  endif else begin
    p = self.transfPointArray(point)
    x = p[*,0]
    y = p[*,1]
    z = p[*,2]
    w = p[*,3]
    if w[0] ne 1 then invw = 1. / w
    newx = self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z + self.matrix[3,0] * w
    newy = self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z + self.matrix[3,1] * w
    newz = self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z + self.matrix[3,2] * w
    neww = self.matrix[0,3] * x + self.matrix[1,3] * y + self.matrix[2,3] * z + self.matrix[3,3] * w
    if w eq 1 then return, pointarrayclass_sazerac(newx, newy, newz) else return, pointarrayclass_sazerac( ([newx, newy, newz]*invw) )
  endelse
  
End



;-----------------
;- To Be Adapted -
;-----------------
Function transformationarrayclass::pointArrayTransformWithPtr, point

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(point)) ne 'pointarrayclass_sazerac' then begin
    print, 'Please provide a point array class object (pointarrayclass_sazerac)...'
    return, 0
  endif else begin
    p = self.transfPointArray(point)
    x = p[*,0]
    y = p[*,1]
    z = p[*,2]
    w = p[*,3]
    if w[0] ne 1 then invw = 1. / w
    newx = self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z + self.matrix[3,0] * w
    newy = self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z + self.matrix[3,1] * w
    newz = self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z + self.matrix[3,2] * w
    neww = self.matrix[0,3] * x + self.matrix[1,3] * y + self.matrix[2,3] * z + self.matrix[3,3] * w
    if w eq 1 then return, ptr_new(pointarrayclass_sazerac(newx, newy, newz)) else return, ptr_new(pointarrayclass_sazerac(([newx, newy, newz]*invw)))
  endelse
  
End
;-

;############################
; ORIGINAL METHOD
;############################
;Function transformationarrayclass::vectorTransform, vec2
;
;  if n_params() ne 1 then begin
;    print, 'Please provide a value...'
;    return, 0
;  endif
;  if strlowcase(obj_class(vec2)) ne 'vectorclass' then begin
;    print, 'Please provide a vector class object (vectorclass)...'
;    return, 0
;  endif else begin
;    x = vec2.x()
;    y = vec2.y()
;    z = vec2.z()
;    return, vectorclass( $
;      self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z, $
;      self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z, $
;      self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z  $
;      )
;  endelse
;  
;End
;
;
;Function transformationarrayclass::vectorTransformWithPtr, vec2
;
;  if n_params() ne 1 then begin
;    print, 'Please provide a value...'
;    return, 0
;  endif
;  if strlowcase(obj_class(vec2)) ne 'vectorarrayclass' then begin
;    print, 'Please provide a vector class object (vectorarrayclass)...'
;    return, 0
;  endif else begin
;    x = vec2.x()
;    y = vec2.y()
;    z = vec2.z()
;    return, ptr_new( vectorclass( $
;      self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z, $
;      self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z, $
;      self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z  $
;      ) )
;  endelse
;  
;End
;#################################
; End of original methods
;#################################

;-
; Modified version of the original methods
; It should handle the vectorclass and vectorarrayclass seamlessly
; to be test...
;-
Function transformationarrayclass::vectorTransform, vec2

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(vec2)) eq 'vectorarrayclass' then begin
    x = vec2.x()
    y = vec2.y()
    z = vec2.z()
    return, vectorarrayclass($
                          (*(self.matrix))[*,0,0] * x + (*(self.matrix))[*,1,0] * y + (*(self.matrix))[*,2,0] * z, $
                          (*(self.matrix))[*,0,1] * x + (*(self.matrix))[*,1,1] * y + (*(self.matrix))[*,2,1] * z, $
                          (*(self.matrix))[*,0,2] * x + (*(self.matrix))[*,1,2] * y + (*(self.matrix))[*,2,2] * z  $ 
                          )                         
;    return, vectorarrayclass( $
;                          self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z, $
;                          self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z, $
;                          self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z  $
;        )
  endif else begin
    print, 'Please provide a vector class or vector array object (vectorclass or vertocarrayclass)...'
    return, 0
  endelse

End


Function transformationarrayclass::vectorTransformWithPtr, vec2

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(vec2)) ne 'vectorclass' or strlowcase(obj_class(vec2)) ne 'vectorarrayclass' then begin
    print, 'Please provide a vector class or vector array object (vectorclass or vertocarrayclass)...'
    return, 0
  endif else begin
    x = vec2.x()
    y = vec2.y()
    z = vec2.z()
    if strlowcase(obj_class(vec2)) eq 'vectorclass' then begin
      return, ptr_new( vectorclass( $
        self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z, $
        self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z, $
        self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z  $
        ) )
    endif else begin
      return, ptr_new( vectorarrayclass( $
        self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z, $
        self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z, $
        self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z  $
        ) )
    endelse
  endelse
    
End
;############################################
; End of modified methods
;############################################


;-
;Function transformationarrayclass::vectorArrayTransform, vec2
;
;  if n_params() ne 1 then begin
;    print, 'Please provide a value...'
;    return, 0
;  endif
;  if strlowcase(obj_class(vec2)) ne 'vectorarrayclass' then begin
;    print, 'Please provide a vector array class object (vectorarrayclass)...'
;    return, 0
;  endif else begin
;    x = vec2.x()
;    y = vec2.y()
;    z = vec2.z()
;    return, vectorarrayclass( $
;      self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z, $
;      self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z, $
;      self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z  $
;      )
;  endelse
;  
;End
;
;
;Function transformationarrayclass::vectorArrayTransformWithPtr, vec2
;
;  if n_params() ne 1 then begin
;    print, 'Please provide a value...'
;    return, 0
;  endif
;  if strlowcase(obj_class(vec2)) ne 'vectorarrayclass' then begin
;    print, 'Please provide a vector array class object (vectorarrayclass)...'
;    return, 0
;  endif else begin
;    x = vec2.x()
;    y = vec2.y()
;    z = vec2.z()
;    return, ptr_new( vectorarrayclass( $
;      self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z, $
;      self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z, $
;      self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z  $
;      ) )
;  endelse
;  
;End
;-


;-
; This function should handle normalclass
;-
Function transformationarrayclass::normalTransform, normVec

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(normVec)) ne 'normalclass' then begin
    print, 'Please provide a normal class object (normalclass)...'
    return, 0
  endif else begin
  x = normVec.x()
  y = normVec.y()
  z = normVec.z()
  return, normalclass( $
    self.invMatrix[0,0] * x + self.invMatrix[0,1] * y + self.invMatrix[0,2] * z, $
    self.invMatrix[1,0] * x + self.invMatrix[1,1] * y + self.invMatrix[1,2] * z, $
    self.invMatrix[2,0] * x + self.invMatrix[2,1] * y + self.invMatrix[2,2] * z  $
    )
  endelse
  
End


Function transformationarrayclass::rayTransform, ray2

  if n_params() ne 1 then begin
    print, 'Please provide one value...'
    return, 0
  endif
  if strlowcase(obj_class(ray2)) ne 'rayclass' then begin
    print, 'Please provide a ray class object (rayclass)...'
    return, 0
  endif else begin
    retOrigin = self.pointTransform(ray2.getOrigin())
    retDirection = self.vectorTransform(ray2.getDirection())
    return, rayclass(retOrigin, retDirection, ray2.getMint(), ray2.getMaxt())
  endelse
  
End


Function transformationarrayclass::diffRayTransform, ray2

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(ray2)) ne 'rayclass' then begin
    print, 'Please provide a ray class object (rayclass)...'
    return, 0
  endif else begin
    retOrigin = self.pointTransform(ray.getOrigin())
    retDirection = self.vectorTransform(ray.getDirection())
    return, rayclass(retOrigin, retDirection)
  endelse
  
End


Function transformationarrayclass::transformBBox, bBox

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(bBox)) ne 'bboxclass' then begin
    print, 'Please provide a bounding box class object (bboxclass)...'
    return, 0
  endif else begin
    bbox_coor = bBox.getBox()
    dum0 = self.pointTransform(bbox_coor[0])
    dum1 = self.pointTransform(bbox_coor[1])
    return, bboxclass(dum0, dum1)
  endelse
  
End


Function transformationarrayclass::multiplyTransform, transf2

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(transf2)) ne 'transformationarrayclass' then begin
    print, 'Please provide a transformation class object (transformationarrayclass)...'
    return, 0
  endif else begin
;    m1 = self.getMatrix() ## transf2.getMatrix()
;    m2 = transf2.getInvMatrix() ## self.getInvMatrix()
    
    tempRes = transformationarrayclass(DIMENSION = self.dimension)
    mat = tempRes.getMatrix()
    invmat = mat
    obj_destroy, tempRes
    
    mat1 = self.getMatrix()
    mat2 = transf2.getMatrix()
    invmat1 = self.getinvMatrix()
    invmat2 = transf2.getinvMatrix()
    
    for i=0,3 do begin      ; 
      for j=0,3 do begin
        mat[*,i,j] = total(mat1[*,*,j] * mat2[*,i,*])
        invmat[*,i,j] = total(invmat1[*,*,j] * invmat2[*,i,*])
      endfor
    endfor
    
    return, transformationarrayclass(MATRIX = mat, INVMATRIX = invmat)
  endelse

End


Function transformationarrayclass::swapsHandeness

  return, determ( (self.getMatrix())[0:2,0:2] ) lt 0.0
  
End


Function transformationarrayclass::orthographic, zNer, zFar

  tempTrsfrm = transformationarrayclass()
  tempScale = vectorclass( [1., 1., 1.] / (zFar-Fnear) )
  tempTrans = vectorclass( 0., 0., -zNear )
  return, transformationarrayclass( (tempTrsfrm.scale(tempScale)).getMatrix() * (tempTrsfrm.translate(tempTrans)).getMatrix() )
  
End


Function transformationarrayclass::perspective, fov, n, f

  tpersp = [$
          [1.0, 0.0, 0.0, 0.0],$
          [0.0, 1.0, 0.0, 0.0],$
          [0.0, 0.0, f/(f-n),-f/(f-n)],$
          [0.0, 0.0, 1.0, 0.0] $
          ]
   invTanAng = 1.0 / tan( (!dtor * fov) / 2.0)
   persp = transformationarrayclass(tpersp)
   return, persp.scale(invTanAng, invTanAng, 1.0)

End

 

Pro transformationarrayclass__define

  void = {transformationarrayclass, $
    dimension : 0ULL,$
    matrix    : ptr_new() ,$
    invmatrix : ptr_new()  $
  }

End

