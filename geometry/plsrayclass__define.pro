Pro plsrayclass__define

  void = {plsrayclass, $
    origin    : pointclass(),$
    direction : vectorclass(),$
    mint      : 0.D,$
    maxt      : 0.D,$
    time      : 0.D,$
    depth     : 0B $
  }
  
End


Function plsrayclass::init, orig, dir, mint, maxt

  Compile_opt idl2
  
  case n_params() of
    0: begin
        self.origin = pointclass()
        self.direction = vectorclass()
      end
    1: begin
      if strlowcase(obj_class(orig)) eq 'vectorclass' then begin
          self.origin = pointclass()
          self.direction = orig
      endif 
      if strlowcase(obj_class(orig)) eq 'pointclass' then begin
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


Pro plsrayclass::cleanup

  Compile_opt idl2
  
;  ptr_free, $
;    self.world
    
End


Function plsrayclass::setOrigin, point

  self.origin = point
  return, 1
  
End

Function plsrayclass::setDirection, vector2

  self.direction = vector2
  return, 1
  
End


Function plsrayclass::setMint, value

  self.mint = value
  return, 1
  
End


Function plsrayclass::setMaxt, value

  self.maxt = value
  return, 1
  
End


Function plsrayclass::setTime, value

  self.time = value
  return, 1
  
End


Function plsrayclass::setDepth, value

  self.depth = value
  return, 1
  
End


Function plsrayclass::getOrigin

  return, self.origin
  
End


Function plsrayclass::getDirection

  return, self.direction
  
End


Function plsrayclass::getMint

  return, self.mint
  
End


Function plsrayclass::getMaxt

  return, self.maxt
  
End


Function plsrayclass::getTime

  return, self.time
  
End


Function plsrayclass::getDepth

  return, self.depth
  
End


Function plsrayclass::createChild

  child = plsrayclass()
  child.time = self.time
  child.depth = self.depth + 1
  return, child
  
End


Function plsrayclass::traceRay, t

;  print, (self.origin).xyz()
;  print, (self.direction).xyz()
  temp = [ (self.origin).xyz() + (replicate(t,3) * (self.direction).xyz() ) ]
  return, pointclass( temp )

End
