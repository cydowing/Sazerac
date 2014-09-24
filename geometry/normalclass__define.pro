Function normalclass::init, cox, coy, coz

  Compile_opt idl2
;  print, n_params()
;  print, 'toto'
  case n_params() of
    1 : begin
          if n_elements(cox) eq 3 then begin
            self.vx = float(cox[0])
            self.vy = float(cox[1])
            self.vz = float(cox[2])
          endif else print, 'Normalclass - Wrong number of elements for initialization...'
        end
    3 : begin
          if n_elements(cox) ne 0 then self.vx = float(cox) else self.vx = 0.
          if n_elements(coy) ne 0 then self.vy = float(coy) else self.vy = 0.
          if n_elements(coz) ne 0 then self.vz = float(coz) else self.vz = 0.
        end
  else : begin
          self.vx = 0.
          self.vy = 0.
          self.vz = 0.
        end
  
  endcase
  
  ; Initializing the object
  return, 1
  
End


Pro normalclass::cleanup

  Compile_opt idl2
  
End


Function normalclass::x

  return, self.vx
  
End



Function normalclass::y

  return, self.vy
  
End


Function normalclass::z

  return, self.vz
  
End


Function normalclass::xyz

  return, [self.vx, self.vy, self.vz]
  
End


Pro normalclass::setX, value

  self.vx = value
  ;return, 'done'
  
End



Pro normalclass::setY, value

  self.vy = value
  ;return, 'done'
  
End


Pro normalclass::setZ, value

  self.vz = value
  ;return, 'done'
  
End


Function normalclass::translate, normalclass

  self.vx += normalclass.x()
  self.vy += normalclass.y()
  self.vz += normalclass.z()
  return, [self.vx, self.vy, self.vz]

End


Function normalclass::scaleUp, normalclass

  self.vx *= normalclass.x()
  self.vy *= normalclass.y()
  self.vz *= normalclass.z()
  return, [self.vx, self.vy, self.vz]


End


Function normalclass::scaleDown, normalclass

  temp = 1. / normalclass.xyz()
  self.vx *= temp[0]
  self.vy *= temp[1]
  self.vz *= temp[2]
  return, [self.vx, self.vy, self.vz]
  
  
End


Function normalclass::opposite

  return, [-self.vx, -self.vy, -self.vz]

End


Function normalclass::length

  return, sqrt( (self.vx)^2 + (self.vy)^2 + (self.vz)^2 )
  
end


Pro normalclass::normalizeLength

  inv = 1. / self.length()
  self.vx *= inv
  self.vy *= inv
  self.vz *= inv
  
End


Function normalclass::getNormLength

  inv = 1. / self.length()
  return, ( [self.vx, self.vy, self.vz] * inv )
  
End


Function normalclass::dot, vector2

  dot = (self.vx * vector2.x()) + (self.vy * vector2.y()) + (self.vz * vector2.z())
  return, dot
  
End


Function normalclass::getRadAngle, vector2

  return, acos( self.dot(vector2) / ( self.length() * vector2.length() ) )
  
End


Function normalclass::getDegAngle, vector2

  return, 180./!PI * acos( self.dot(vector2) / ( self.length() * vector2.length() ) )
  
End


Function normalclass::det, vector2

  detX = (self.vy * vector2.z()) - (self.vz * vector2.y())
  detY = (self.vz * vector2.x()) - (self.vx * vector2.z())
  detZ = (self.vx * vector2.y()) - (self.vy * vector2.x())
  return, [detX, detY, detZ]

End


Function normalclass::paralVolume, vector2

  tempvec = normalclass(self.det(vector2))
  return, tempvec.length()

End


;+
; a function that creates a local coordinate system
; based on the vector hold in self.
; 
; IDL> print, vec6.localCoordinateSystem()
; <ObjHeapVar6(normalclass)><ObjHeapVar7(normalclass)>
; IDL> t = vec6.localCoordinateSystem()
; IDL> print, t[0].xyz()
; 0.0000000     0.062378287     -0.99805260
;-
Function normalclass::localCoordinateSystem

  if self.vx gt self.vy then begin
    invLen = 1. / sqrt( (self.vx)^2 + (self.vz)^2 )
    v2 = normalclass(-self.vz * invLen, 0., self.vx * invLen)
  endif else begin
    invLen = 1. / sqrt( (self.vy)^2 + (self.vz)^2 )
    v2 = normalclass(0., self.vz * invLen, -self.vy * invLen) 
  endelse
  
  v3 = normalclass(self.det(v2))

  ;with this line the 3 vectors coordinates are return
  ;return, [ [self.xyz()],[v2.xyz()],[v3.xyz()] ] 
  
  ; with this line the actual vector objects are return - most likely more efficient
  return, [ v2, v3 ]
  
End


Pro normalclass__define

  void = {normalclass, $
    vx    : 0. ,$
    vy    : 0. ,$
    vz    : 0. $
  }

End

