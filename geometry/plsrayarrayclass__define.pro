Function plsrayarrayclass::init, orig, dir, pulse

  Compile_opt idl2
  
  self.Origin = orig
  self.Direction = dir
  if n_elements(pulse) ne 0 then begin
    self.N = pulse.N
    self.Pulse = pulse.Pulse
    self.Duranchor = pulse.Durationfromanchor
    self.Lutable = pulse.Lut
  endif
  ; Initializing the object
  return, 1
  
End


Pro plsrayarrayclass::cleanup

  Compile_opt idl2
  
;  ptr_free, $
;    self.world
    
End


Function plsrayarrayclass::setOrigin, point, index

  self.origin = point
  return, 1
  
End

Function plsrayarrayclass::setDirection, vector2, index

  self.direction = vector2
  return, 1
  
End



Function plsrayarrayclass::getOrigin

  return, self.origin
  
End


Function plsrayarrayclass::getDirection

  return, self.direction
  
End



Function plsrayarrayclass::createChild

  child = plsrayarrayclass()
  child.time = self.time
  child.depth = self.depth + 1
  return, child
  
End


Function plsrayarrayclass::createUniqueChild, index

  child = plsrayclass()
  child.origin = self.Time
  child.direction = self.Depth + 1
  Return, child

End


Function plsrayarrayclass::traceRay, t

;  print, (self.origin).xyz()
;  print, (self.direction).xyz()
  temp = [ (self.origin).xyz() + (replicate(t,3) * (self.direction).xyz() ) ]
  return, pointarrayclass_sazerac( temp )

End



Pro plsrayarrayclass__define

  void = {plsrayarrayclass, $
    origin    : pointclass_sazerac(),$        ; Origin of the pluse = Anchor point
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

