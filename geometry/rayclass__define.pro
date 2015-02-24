Function rayclass::init, orig, dir, mint, maxt

  Compile_opt idl2
  
  case n_params() of
    0: begin
        self.origin = pointclass_sazerac()
        self.direction = vectorclass()
      end
    1: begin
      if strlowcase(obj_class(orig)) eq 'vectorclass' then begin
          self.origin = pointclass_sazerac()
          self.direction = orig
      endif 
      if strlowcase(obj_class(orig)) eq 'pointclass_sazerac' then begin
          self.origin = orig
          self.direction = vectorclass()
      endif
      end
    2: begin
      ;print, '2 arguments found...' 
      self.origin = orig
      self.direction = dir
      end
    3: begin
      ;print, '3 arguments found...'
      self.origin = orig
      self.direction = dir
      self.mint = mint
    end
    4: begin
      ;print, '4 arguments found...'
      self.origin = orig
      self.direction = dir
      self.mint = mint
      self.maxt = maxt
    end
    else: print, 'Hummm... something went wrong...'
  endcase
  
  ; Initializing the object
  return, 1
  
End


Pro rayclass::cleanup

  Compile_opt idl2
  
;  ptr_free, $
;    self.world
    
End


Function rayclass::setOrigin, point

  self.origin = point
  return, 1
  
End

Function rayclass::setDirection, vector2

  self.direction = vector2
  return, 1
  
End


Function rayclass::setMint, value

  self.mint = value
  return, 1
  
End


Function rayclass::setMaxt, value

  self.maxt = value
  return, 1
  
End


Function rayclass::setTime, value

  self.time = value
  return, 1
  
End


Function rayclass::setDepth, value

  self.depth = value
  return, 1
  
End


Function rayclass::getOrigin

  return, self.origin
  
End


Function rayclass::getDirection

  return, self.direction
  
End


Function rayclass::getMint

  return, self.mint
  
End


Function rayclass::getMaxt

  return, self.maxt
  
End


Function rayclass::getTime

  return, self.time
  
End


Function rayclass::getDepth

  return, self.depth
  
End


Function rayclass::createChild

  child = rayclass()
  child.time = self.time
  child.depth = self.depth + 1
  return, child
  
End


Function rayclass::traceRay, t

;  print, (self.origin).xyz()
;  print, (self.direction).xyz()
  temp = [ (self.origin).xyz() + (replicate(t,3) * (self.direction).xyz() ) ]
  return, pointclass_sazerac( temp )

End



Pro rayclass__define

  void = {rayclass, $
    origin    : pointclass_sazerac(),$
    direction : vectorclass(),$
    mint      : 0.D,$
    maxt      : 0.D,$
    time      : 0.D,$
    depth     : 0B $
  }

End

