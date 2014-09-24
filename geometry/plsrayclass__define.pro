;Function plsrayclass::init, orig, dir, mint, maxt
Function plsrayclass::init, orig, dir, pulse

  Compile_opt idl2
  
  self.origin = orig
  self.direction = dir
  self.n = pulse.n
  self.pulse = pulse.pulse
  self.durAnchor = pulse.durationFromAnchor
  self.luTable = pulse.lut
 
  
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



Pro plsrayclass__define

  void = {plsrayclass, $
    origin    : pointclass(),$        ; Origin of the pluse = Anchor point
    direction : vectorclass(),$       ; Direction of the pulse = Normalized Anchor to Target vector
    n         : ptr_new(),$         ; Pointer to the number of samples per pulse segment,  concatenated into an single array
    pulse     : ptr_new(),$         ; Pointer to the wavefrom values concatenated together
    durAnchor : ptr_new(),$         ; Pointer to the Duration from Anchor values, concatenated into an single array
    luTable   : ptr_new(),$         ; Pointer to the lookup table for the pulse
    outX      : ptr_new(),$         ; Array of the time values for the OUTGOING pulse
    outY      : ptr_new(),$         ; Array of the energy/amplitude of the OUTGOING pulse
    inX       : ptr_new(),$         ; Array of the time values for the RETURNING pulse - note if multiple segement, then it will be an array of structure
    inY       : ptr_new(),$         ; Array of the time values for the RETURNING pulse - note if multiple segement, then it will be an array of structure
    depth     : 0B $
  }

End

