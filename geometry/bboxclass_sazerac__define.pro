Function bboxclass_sazerac::init, inOne, inTwo

  Compile_opt idl2, HIDDEN
  
;  if strlowcase(obj_class(inOne)) ne 'pointclass_sazerac' then begin
;    print, 'Please provide a point class object (pointclass_sazerac) for the first argument...'
;    return, 0
;  endif
;  
;  if strlowcase(obj_class(inTwo)) ne 'pointclass_sazerac' then begin
;    print, 'Please provide a point class object (pointclass_sazerac) for the second argument...'
;    return, 0
;  endif
  
  case n_params() of
    0: begin
        self.ptMin = pointclass_sazerac(!values.F_INFINITY, !values.F_INFINITY, !values.F_INFINITY)
        self.ptMax = pointclass_sazerac(-!values.F_INFINITY, -!values.F_INFINITY, -!values.F_INFINITY)
      end
    1: begin
          self.ptMin = inOne
          self.ptMax = pointclass_sazerac(-!values.F_INFINITY, -!values.F_INFINITY, -!values.F_INFINITY)
      end
    2: begin 
        if obj_valid(inOne) eq 0 or obj_valid(inTwo) eq 0 then begin
          print, 'Please provide a point class object (pointclass_sazerac)...'
          return, 0
        endif else begin
        self.ptMin = pointclass_sazerac( inOne.x() < inTwo.x(), inOne.y() < inTwo.y(), inOne.z() < inTwo.z() )
        self.ptMax = pointclass_sazerac( inOne.x() > inTwo.x(), inOne.y() > inTwo.y(), inOne.z() > inTwo.z() )
        endelse
      end
    else: print, 'Hummm... something went wrong...'
  endcase
  
  ; Initializing the object
  return, 1
  
End


Pro bboxclass_sazerac::cleanup

  Compile_opt idl2
  
;  ptr_free, $
;    self.world
    
End


Function bboxclass_sazerac::getptMin

  return, self.ptMin
  
End


Function bboxclass_sazerac::getptMax

  return, self.ptMax
  
End


Function bboxclass_sazerac::getBox

  return, [self.ptMin, self.ptMax]
  
End


Function bboxclass_sazerac::setptMin, point

  self.ptMin = point
  return, 1
  
End


Function bboxclass_sazerac::setptMax, point

  self.ptMax = point
  return, 1
  
End


Function bboxclass_sazerac::unionWithPoint, point

  if obj_class(point) ne strupcase('pointclass_sazerac') then begin
    print, 'Please provide a point class object (pointclass_sazerac)...'
    return, 0
  endif else begin
    newbox = bboxclass_sazerac
    tminX = ( (self.ptMin).x() < point.x() )
    tminY = ( (self.ptMin).y() < point.y() )
    tminZ = ( (self.ptMin).z() < point.z() )
    tmaxX = ( (self.ptMax).x() > point.x() )
    tmaxY = ( (self.ptMax).y() > point.y() )
    tmaxZ = ( (self.ptMax).z() > point.z() )
    dum = newbox.setptMin( pointclass_sazerac( tminX, tminY, tminZ ) )
    dum = newbox.setPtMax( pointclass_sazerac( tmaxX, tmaxY, tmaxZ ) )
  endelse
  
  return, newbox
  
End


Function bboxclass_sazerac::unionWithBox, box

  if obj_class(box) ne strupcase('bboxclass_sazerac') then begin
    print, 'Please provide a bounding box class object (bboxclass_sazerac)...'
    return, 0
  endif else begin
    newbox = bboxclass_sazerac
    tminX = ( (self.ptMin).x() < (box.getptMin()).x() )
    tminY = ( (self.ptMin).y() < (box.getptMin()).y() )
    tminZ = ( (self.ptMin).z() < (box.getptMin()).z() )
    tmaxX = ( (self.ptMax).x() > (box.getptMax()).x() )
    tmaxY = ( (self.ptMax).y() > (box.getptMax()).y() )
    tmaxZ = ( (self.ptMax).z() > (box.getptMax()).z() )
    dum = newbox.setptMin( pointclass_sazerac( tminX, tminY, tminZ ) )
    dum = newbox.setPtMax( pointclass_sazerac( tmaxX, tmaxY, tmaxZ ) )
  endelse
  
  return, newbox
  
End


Function bboxclass_sazerac::overlapping, box

  if obj_class(box) ne strupcase('bboxclass_sazerac') then begin
    print, 'Please provide a bounding box class object (bboxclass_sazerac)...'
    return, 0
  endif else begin
    return, ($
      ( (self.ptMax).x() ge (box.getptMin()).x() ) and ( (self.ptMin).x() le (box.getptMax()).x() ) and $
      ( (self.ptMax).y() ge (box.getptMin()).y() ) and ( (self.ptMin).y() le (box.getptMax()).y() ) and $
      ( (self.ptMax).z() ge (box.getptMin()).z() ) and ( (self.ptMin).z() le (box.getptMax()).z() ) $
            )
  endelse
  
End


Function bboxclass_sazerac::pointInside, point

  if obj_class(point) ne strupcase('pointclass_sazerac') then begin
    print, 'Please provide a point class object (pointclass_sazerac)...'
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
Function bboxclass_sazerac::pad, vector2

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
Function bboxclass_sazerac::surfaceArea

  temp = (self.ptMin).makeVector(self.ptMax)
  return, 2. * (temp.x() *temp.y() + temp.x() * temp.z() + temp.y() * temp.z() )
  
End


;+
; This function returns the volume of the bounding box in world units.
; This function returns a float value
;-
Function bboxclass_sazerac::boxVolume

  temp = (self.ptMin).makeVector(self.ptMax)
  return, temp.x() *temp.y() * temp.z()
  
End


;+
; This function computes the longest dimension of the bounding box
; It will return 0 for x, 1 for y and 2 for z.
;-
Function bboxclass_sazerac::maximumExtend

  diag = (self.ptMin).makeVector(self.ptMax)
  if diag.x() lt diag.y() and diag.x() gt diag.z() then return, 0
  if diag.y() gt diag.z() then return, 1 else return, 2
  
End


Function bboxclass_sazerac::interpolatePoint, point

  if strlowcase(obj_class(point)) ne 'pointclass_sazerac' then begin
    print, 'Please provide a point class object (pointclass_sazerac)...'
    return, 0
  endif else begin
    return, pointclass_sazerac(lerp(point.x(),(self.ptMin).x(),(self.ptMax).x()), $
                       lerp(point.y(),(self.ptMin).y(),(self.ptMax).y()), $
                       lerp(point.z(),(self.ptMin).z(),(self.ptMax).z())  $
                       )
  endelse

End


Function bboxclass_sazerac::offset, point

  if strlowcase(obj_class(point)) ne 'pointclass_sazerac' then begin
    print, 'Please provide a point class object (pointclass_sazerac)...'
    return, 0
  endif else begin
    return, vectorclass($
                        ( point.x() - (self.ptMin).x() / (self.ptMax).x() - point.x() ), $
                        ( point.y() - (self.ptMin).y() / (self.ptMax).y() - point.y() ), $
                        ( point.z() - (self.ptMin).z() / (self.ptMax).z() - point.z() )  $
                        )
  endelse

End


Function bboxclass_sazerac::returnCornerAsArray, FLEURDELAS = FLEURDELAS

  ta = (self.ptMin).xyz()
  tb = (self.ptMax).xyz()
  ; Fleurdelas bbox formating -> [xMax, xMin, yMax, yMin, zMax, zMin]
  if keyword_set(FLEURDELAS) then return, [tb[0],ta[0],tb[1],ta[1],tb[2],ta[2]] else return, [ta, tb]
  
End


Function bboxclass_sazerac::boundingSphere

  c = pointclass_sazerac( (.5 * (self.ptMin).xyz()) + (.5 * (self.ptMax).xyz()) )
  rad = self.pointInside(c) ? c.distance(self.ptMax) : 0.0
  return, [c, rad]

End


Function bboxclass_sazerac::intersectP, ray, hitt0, hitt1

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
  
  if (hitt0) then hitt0 = t0
  if (hitt1) then hitt1 = t1

  return, 1
  
End



Pro bboxclass_sazerac__define

  void = {bboxclass_sazerac, $
    ptMin   : pointclass_sazerac() ,$
    ptMax   : pointclass_sazerac() $
  }

End


