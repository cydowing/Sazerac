Function byWater::init

;, orig, dir, pulse

dumray = self->plsrayclass::init()



;  , WAVE = WAVE, NSAMPLES = NSAMPLES, $
;    ORIGIN = ORIGIN, DIRECTION = DIRECTION, $
;    DFA = DFA, LUT = LUT, NAN = NAN, $
;    FORMATORIGIN = FORMATORIGIN, MANUFACTURER = MANUFACTURER
    

dumwave = self->waveformclass::init()

return, 1

End


Function byWater::cleanup

Return, 1

End



Function byWater::getFirstSegment

  nSamples = (*self.n)[1]
  samples = (*self.pulse)[(*self.n)[0]:(*self.n)[0]+nSamples-1]
  time = (*self.durAnchor)[1]
  luTable = *self.luTable

  time = indgen(nSamples) + time
  coordinates = self.tracePulse(time)
  intensity = luTable[samples]

  Return, {coor:coordinates, int:intensity}

End



Function byWater::getLastSegment

n = self.getNumberOfSegment()
nSamples = (*self.n)[n-1]
samples = (*self.pulse)[-nSamples:*]
time = (*self.durAnchor)[n-1]
luTable = *self.luTable

time = indgen(nSamples) + time
coordinates = self.tracePulse(time)
intensity = luTable[samples]

Return, {coor:coordinates, int:intensity}

End



Function byWater::getNthSegment, n

  nSamples = (*self.n)[n]
  if n eq 0 then begin
    samples = (*self.pulse)[0:nSamples-1]
  endif else begin
    samples = (*self.pulse)[(*self.n)[n-1]:(*self.n)[n-1]+nSamples-1]
  endelse
  time = (*self.durAnchor)[n]
  luTable = *self.luTable

  time = indgen(nSamples) + time
  coordinates = self.tracePulse(time)
  intensity = luTable[samples]

  Return, {coor:coordinates, int:intensity}

End


; TODO : issue with the array of structure formating
Function byWater::getAllSegments

  nSeg = self.getNumberOfSegment()
  luTable = *(self.luTable)
  
  for n = 0, nSeg-1 do begin

    nSamples = (*self.n)[n]
    if n eq 0 then begin
      samples = (*self.pulse)[0:nSamples-1]
    endif else begin
      samples = (*self.pulse)[(*self.n)[n-1]:(*self.n)[n-1]+nSamples-1]
    endelse
    time = (*self.durAnchor)[n]
;    luTable = *(self.luTable)

    time = indgen(nSamples) + time
    coordinates = self.tracePulse(time)
    intensity = luTable[samples]

    temp = {res, coor:coordinates, int:intensity} 

    if n eq 0 then resCombo = temp else resComb = [resComb, temp]

  endfor
  
  Return, temp

End



Pro byWater__define

void = {byWater, $
  void      : 0, $
  inherits waveformclass, $
  inherits plsrayclass $
  }
  
End