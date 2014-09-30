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


Function plsrayclass::setDurAnchor, value

  self.durAnchor = value
  return, 1
  
End


Function plsrayclass::setLuTable, value

  self.luTable = value
  return, 1
  
End



Function plsrayclass::setN, value

  self.n = value
  Return, 1

End



Function plsrayclass::getOrigin

  return, self.origin
  
End


Function plsrayclass::getDirection

  return, self.direction
  
End


Function plsrayclass::getDurAnchor

  return, self.durAnchor
  
End


Function plsrayclass::getluTable

  return, self.luTable
  
End


Function plsrayclass::getN

  return, self.n
  
End


Function plsrayclass::getPulse

  return, self.pulse
  
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



Function plsrayclass::findSimilarRay, rayArr, $
  THRESDIST = THRESDIST, $
  THRESANGL = THRESANGL

  ; Find the closest anchor point rays
  if Keyword_set(THRESDIST) then distance = THRESDIST else distance = 1.
  if Keyword_set(THRESANGL) then angle = THRESANGL else angle = 5. * !DTOR

  oriArr = rayArr.getOrigin()
  dirArr = rayArr.getDirection()
  
  anchor = self.origin
  anchorArr = anchor.duplicateToPointArrayClass(oriArr.getDim())
  pointDist = anchorArr.distance(oriArr)

  dID = Where(pointDist le distance, /NULL)
  
  direction = dirArr.getSubArray(dID)
  rayDirection = self.getDirection()
;  rayDirectionArr = rayDirection.duplicateToPointArrayClass(n_elements(dID))
  angleArr = direction.getRadAngle(rayDirection)
  
  minAngle = min(angleArr, minSub)
  
  
  
    
  return, 1

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

