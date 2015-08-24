Function plsrayclass::init, orig, dir, pulse

  Compile_opt idl2
  
  self.origin = orig
  self.direction = dir
  self.n = pulse.n
  self.pulse = pulse.pulse
  self.durAnchor = pulse.durationFromAnchor
  self.luTable = pulse.lut
 
  ; Call consoleclass superclass Initialization method.
  dum = self->consoleclass::init()
  
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



;Function plsrayclass::getFirstSegment
;
;  nSamples = (*self.n)[1]
;  samples = (*self.pulse)[(*self.n)[0]:(*self.n)[0]+nSamples-1]
;  time = (*self.durAnchor)[1]
;  luTable = *self.luTable
;
;  time = indgen(nSamples) + time
;  coordinates = self.tracePulse(time)
;  intensity = luTable[samples]
;
;  Return, {coor:coordinates, int:intensity}
;
;End
;
;
;
;Function plsrayclass::getLastSegment
;
;n = self.getNumberOfSegment()
;nSamples = (*self.n)[n-1]
;samples = (*self.pulse)[-nSamples:*]
;time = (*self.durAnchor)[n-1]
;luTable = *self.luTable
;
;time = indgen(nSamples) + time
;coordinates = self.tracePulse(time)
;intensity = luTable[samples]
;
;Return, {coor:coordinates, int:intensity}
;
;End
;
;
;
;Function plsrayclass::getNthSegment, n
;
;  nSamples = (*self.n)[n]
;  if n eq 0 then begin
;    samples = (*self.pulse)[0:nSamples-1]
;  endif else begin
;    samples = (*self.pulse)[(*self.n)[n-1]:(*self.n)[n-1]+nSamples-1]
;  endelse
;  time = (*self.durAnchor)[n]
;  luTable = *self.luTable
;
;  time = indgen(nSamples) + time
;  coordinates = self.tracePulse(time)
;  intensity = luTable[samples]
;
;  Return, {coor:coordinates, int:intensity}
;
;End
;
;
;; TODO : issue with the array of structure formating
;Function plsrayclass::getAllSegments
;
;  nSeg = self.getNumberOfSegment()
;  luTable = *(self.luTable)
;  
;  for n = 0, nSeg-1 do begin
;
;    nSamples = (*self.n)[n]
;    if n eq 0 then begin
;      samples = (*self.pulse)[0:nSamples-1]
;    endif else begin
;      samples = (*self.pulse)[(*self.n)[n-1]:(*self.n)[n-1]+nSamples-1]
;    endelse
;    time = (*self.durAnchor)[n]
;;    luTable = *(self.luTable)
;
;    time = indgen(nSamples) + time
;    coordinates = self.tracePulse(time)
;    intensity = luTable[samples]
;
;    temp = {res, coor:coordinates, int:intensity} 
;
;    if n eq 0 then resCombo = temp else resComb = [resComb, temp]
;
;  endfor
;  
;  Return, temp
;
;End



Function plsrayclass::tracePulse, t

  x = (self.origin).X() + ( t * (self.direction).X() )
  y = (self.origin).Y() + ( t * (self.direction).Y() )
  z = (self.origin).Z() + ( t * (self.direction).Z() )
  
;  x_{first} = x_{anchor} + first_returning_sample * dx  
;  y_{first} = y_{anchor} + first_returning_sample * dy
;  z_{first} = z_{anchor} + first_returning_sample * dz

if n_elements(x) eq 1 then return, pointclass_sazerac(x,y,z) else return, pointarrayclass_sazerac(x,y,z)

End



; This function return a ray that has a similar anchor point and similar direction
; A distance threshold can be set for the anchor point distance
; For the angle, now we just take the smallest one on the selected rays
Function plsrayclass::findSimilarRay, rayArr, $
  THRESDIST = THRESDIST, $
  THRESANGL = THRESANGL

  self.printsep
  self.print, 1, 'Looking for similar ray'
  self.printsep
  
  ; Find the closest anchor point rays
  if Keyword_set(THRESDIST) then distance = THRESDIST else distance = 1.
  if Keyword_set(THRESANGL) then angle = THRESANGL else angle = 5. * !DTOR

  oriArr = rayArr.getOrigin()
  dirArr = rayArr.getDirection()
  
  anchor = self.origin
  anchorArr = anchor.duplicateTopointarrayclass_sazerac(oriArr.getDim())
  pointDist = anchorArr.distance(oriArr)

  dID = Where(pointDist le distance, /NULL)
  

  if dID ne !NULL then begin
 
    self.print, 1, Strcompress(String(N_elements(dID)), /REMOVE_ALL) + ' ray have been selected...'
    self.print, 1, 'Min Max distance from reference ray : ' + Strcompress(String(Min(pointDist[dID])), /REMOVE_ALL) + ' ' +  Strcompress(String(Max(pointDist[dID])), /REMOVE_ALL)

    direction = dirArr.getSubArray(dID)
    rayDirection = self.getDirection()
    ; rayDirectionArr = rayDirection.duplicateTopointarrayclass_sazerac(n_elements(dID))
    angleArr = direction.getRadAngle(rayDirection)
    
    ; Here avoid to use the angle threshold to make sure we get the smaller angle value
    minAngle = min(angleArr, minSub)
    
    self.print, 1, 'Angle between reference ray and similar ray : ' + strcompress(string(!radeg * minAngle), /REMOVE_ALL) + ' deg'
    self.print, 1, 'Distance between reference ray and similar ray : ' + strcompress(string(pointDist[did[minSub]]), /REMOVE_ALL) + ' m'
    ; Returning the index of the most similar ray      
    return, did[minSub]
  
  endif else return, !NULL

End





Pro plsrayclass__define

  void = {plsrayclass, $
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
    depth     : 0B, $
    inherits consoleclass $
  }

End

