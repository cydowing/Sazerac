Pro bboxclass__define

  void = {bboxclass, $
    ptMin   : pointclass ,$
    ptMax   : pointclass $
  }
  
End


Function bboxclass::init, inOne, inTwo

  Compile_opt idl2
  
;  if strlowcase(obj_class(inOne)) ne 'pointclass' then begin
;    print, 'Please provide a point class object (pointclass) for the first argument...'
;    return, 0
;  endif
;  
;  if strlowcase(obj_class(inTwo)) ne 'pointclass' then begin
;    print, 'Please provide a point class object (pointclass) for the second argument...'
;    return, 0
;  endif
  
  case n_params() of
    0: begin
        self.ptMin = pointclass(!values.F_INFINITY, !values.F_INFINITY, !values.F_INFINITY)
        self.ptMax = pointclass(-!values.F_INFINITY, -!values.F_INFINITY, -!values.F_INFINITY)
      end
    1: begin
          self.ptMin = inOne
          self.ptMax = pointclass(-!values.F_INFINITY, -!values.F_INFINITY, -!values.F_INFINITY)
      end
    2: begin 
        if obj_valid(inOne) eq 0 or obj_valid(inTwo) eq 0 then begin
          print, 'Please provide a point class object (pointclass)...'
          return, 0
        endif else begin
        self.ptMin = pointclass( inOne.x() < inTwo.x(), inOne.y() < inTwo.y(), inOne.z() < inTwo.z() )
        self.ptMax = pointclass( inOne.x() > inTwo.x(), inOne.y() > inTwo.y(), inOne.z() > inTwo.z() )
        endelse
      end
    else: print, 'Hummm... something went wrong...'
  endcase
  
  ; Initializing the object
  return, 1
  
End


Pro bboxclass::cleanup

  Compile_opt idl2
  
;  ptr_free, $
;    self.world
    
End


Function bboxclass::getptMin

  return, self.ptMin
  
End


Function bboxclass::getptMax

  return, self.ptMax
  
End


Function bboxclass::getBox

  return, [self.ptMin, self.ptMax]
  
End


Function bboxclass::setptMin, point

  self.ptMin = point
  return, 1
  
End


Function bboxclass::setptMax, point

  self.ptMax = point
  return, 1
  
End


Function bboxclass::unionWithPoint, point

  if obj_class(point) ne strupcase('pointclass') then begin
    print, 'Please provide a point class object (pointclass)...'
    return, 0
  endif else begin
    newbox = bboxclass
    tminX = ( (self.ptMin).x() < point.x() )
    tminY = ( (self.ptMin).y() < point.y() )
    tminZ = ( (self.ptMin).z() < point.z() )
    tmaxX = ( (self.ptMax).x() > point.x() )
    tmaxY = ( (self.ptMax).y() > point.y() )
    tmaxZ = ( (self.ptMax).z() > point.z() )
    dum = newbox.setptMin( pointclass( tminX, tminY, tminZ ) )
    dum = newbox.setPtMax( pointclass( tmaxX, tmaxY, tmaxZ ) )
  endelse
  
  return, newbox
  
End


Function bboxclass::unionWithBox, box

  if obj_class(box) ne strupcase('bboxclass') then begin
    print, 'Please provide a bounding box class object (bboxclass)...'
    return, 0
  endif else begin
    newbox = bboxclass
    tminX = ( (self.ptMin).x() < (box.getptMin()).x() )
    tminY = ( (self.ptMin).y() < (box.getptMin()).y() )
    tminZ = ( (self.ptMin).z() < (box.getptMin()).z() )
    tmaxX = ( (self.ptMax).x() > (box.getptMax()).x() )
    tmaxY = ( (self.ptMax).y() > (box.getptMax()).y() )
    tmaxZ = ( (self.ptMax).z() > (box.getptMax()).z() )
    dum = newbox.setptMin( pointclass( tminX, tminY, tminZ ) )
    dum = newbox.setPtMax( pointclass( tmaxX, tmaxY, tmaxZ ) )
  endelse
  
  return, newbox
  
End


Function bboxclass::overlapping, box

  if obj_class(box) ne strupcase('bboxclass') then begin
    print, 'Please provide a bounding box class object (bboxclass)...'
    return, 0
  endif else begin
    return, ($
      ( (self.ptMax).x() ge (box.getptMin()).x() ) and ( (self.ptMin).x() le (box.getptMax()).x() ) and $
      ( (self.ptMax).y() ge (box.getptMin()).y() ) and ( (self.ptMin).y() le (box.getptMax()).y() ) and $
      ( (self.ptMax).z() ge (box.getptMin()).z() ) and ( (self.ptMin).z() le (box.getptMax()).z() ) $
            )
  endelse
  
End


Function bboxclass::pointInside, point

  if obj_class(point) ne strupcase('pointclass') then begin
    print, 'Please provide a point class object (pointclass)...'
    return, 0
  endif else begin
    return, ($
      ( point.x() ge (self.ptMin).x() and point.x() le (self.ptMax).x() ) and $
      ( point.y() ge (self.ptMin).y() and point.y() le (self.ptMax).y() ) and $
      ( point.z() ge (self.ptMin).z() and point.z() le (self.ptMax).z() ) $
            )
  endelse
  
End


;+
; This function extend/pad the bounding box by a fix value across all
; the dimensions.
;-
Function bboxclass::pad, vector2

  if obj_class(vector2) ne strupcase('vectorclass') then begin
    print, 'Please provide a vector class object (vectorclass)...'
    return, 0
  endif else begin
    dum = (self.ptMin).addVector(vector2)
    dum = (self.ptMax).addVector(vector2)
  endelse
  
  return, self

End

;+
;This function returns a float value
;-
Function bboxclass::surfaceArea

  temp = (self.ptMin).makeVector(self.ptMax)
  return, 2. * (temp.x() *temp.y() + temp.x() * temp.z() + temp.y() * temp.z() )
  
End


;+
; This function returns the volume of the bounding box in world units.
; This function returns a float value
;-
Function bboxclass::boxVolume

  temp = (self.ptMin).makeVector(self.ptMax)
  return, temp.x() *temp.y() * temp.z()
  
End


;+
; This function computes the longest dimension of the bounding box
; It will return 0 for x, 1 for y and 2 for z.
;-
Function bboxclass::maximumExtend

  diag = (self.ptMin).makeVector(self.ptMax)
  if diag.x() lt diag.y() and diag.x() gt diag.z() then return, 0
  if diag.y() gt diag.z() then return, 1 else return, 2
  
End


Function bboxclass::interpolatePoint, point

  if strlowcase(obj_class(point)) ne 'pointclass' then begin
    print, 'Please provide a point class object (pointclass)...'
    return, 0
  endif else begin
    return, pointclass(lerp(point.x(),(self.ptMin).x(),(self.ptMax).x()), $
                       lerp(point.y(),(self.ptMin).y(),(self.ptMax).y()), $
                       lerp(point.z(),(self.ptMin).z(),(self.ptMax).z())  $
                       )
  endelse

End


Function bboxclass::offset, point

  if strlowcase(obj_class(point)) ne 'pointclass' then begin
    print, 'Please provide a point class object (pointclass)...'
    return, 0
  endif else begin
    return, vectorclass($
                        ( point.x() - (self.ptMin).x() / (self.ptMax).x() - point.x() ), $
                        ( point.y() - (self.ptMin).y() / (self.ptMax).y() - point.y() ), $
                        ( point.z() - (self.ptMin).z() / (self.ptMax).z() - point.z() )  $
                        )
  endelse

End


Function bboxclass::returnCornerAsArray
  ta = (self.ptMin).xyz()
  tb = (self.ptMax).xyz()
  return, [ta, tb]
  
End


Function bboxclass::boundingSphere

  c = pointclass( (.5 * (self.ptMin).xyz()) + (.5 * (self.ptMax).xyz()) )
  rad = self.pointInside(c) ? c.distance(self.ptMax) : 0.0
  return, [c, rad]

End


Function bboxclass::intersectP, ray, hitt0, hitt1

  t0 = (ray.getMint()).xyz()
  t1 = (ray.getMaxt()).xyz()
    
  rayOr = (ray.getOrigin()).xyz()
  rayDir = (ray.getDirection()).xyz()
  pMin = (self.getpMin()).xyz()
  pMax = (self.getpMax()).xyz()
  
  for i = 0, 2, 1 do begin
    invRayDir = 1.0 / rayDir[i]
    tNear = (pMin[i] - rayOr[i]) * invRayDir
    tFar = (pMax[i] - rayOr[i]) * invRayDir
  

  if (tNear gt tFar) then begin
    tempNear = tNear
    tNear = tFar
    tFar = tempNear
  endif
  
  t0 = tNear > t0 ? tNear : t0
  t1 = tFar < t1 ? tFar : t1
  
  if (t0 gt t1) then return, 0
  
  endfor

End

