Pro plsrayarrayclass__define

  void = {plsrayarrayclass, $
    origin    : pointarrayclass(),$
    direction : vectorarrayclass(),$
    mint      : 0.D,$
    maxt      : 0.D,$
    time      : 0.D,$
    depth     : 0B $
  }
  
End


Function plsrayarrayclass::init, orig, dir, mint, maxt

  Compile_opt idl2
  
  case n_params() of
    0: begin
        self.origin = pointarrayclass()
        self.direction = vectorarrayclass()
      end
    1: begin
      if strlowcase(obj_class(orig)) eq 'vectorarrayclass' then begin
          self.origin = pointarrayclass()
          self.direction = orig
      endif 
      if strlowcase(obj_class(orig)) eq 'pointarrayclass' then begin
          self.origin = orig
          self.direction = vectorarrayclass()
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


Pro plsrayarrayclass::cleanup

  Compile_opt idl2
  
;  ptr_free, $
;    self.world
    
End


Function plsrayarrayclass::setOrigin, point

  self.origin = point
  return, 1
  
End

Function plsrayarrayclass::setDirection, vector2

  self.direction = vector2
  return, 1
  
End


Function plsrayarrayclass::setMint, value

  self.mint = value
  return, 1
  
End


Function plsrayarrayclass::setMaxt, value

  self.maxt = value
  return, 1
  
End


Function plsrayarrayclass::setTime, value

  self.time = value
  return, 1
  
End


Function plsrayarrayclass::setDepth, value

  self.depth = value
  return, 1
  
End


Function plsrayarrayclass::getOrigin

  return, self.origin
  
End


Function plsrayarrayclass::getDirection

  return, self.direction
  
End


Function plsrayarrayclass::getMint

  return, self.mint
  
End


Function plsrayarrayclass::getMaxt

  return, self.maxt
  
End


Function plsrayarrayclass::getTime

  return, self.time
  
End


Function plsrayarrayclass::getDepth

  return, self.depth
  
End


Function plsrayarrayclass::createChild

  child = plsrayarrayclass()
  child.time = self.time
  child.depth = self.depth + 1
  return, child
  
End


Function plsrayarrayclass::traceRay, t

;  print, (self.origin).xyz()
;  print, (self.direction).xyz()
  temp = [ (self.origin).xyz() + (replicate(t,3) * (self.direction).xyz() ) ]
  return, pointarrayclass( temp )

End
