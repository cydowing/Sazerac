Function byWater::init, ORIGIN = ORIGIN, DIRECTION = DIRECTION, WAVE = WAVE
      

self.nWaveform = size(WAVE, /DIMENSION)
self.waveform = ptr_new(WAVE)
dumpray = self->plsrayclass::init(ORIGIN, DIRECTION)

return, 1

End


Function byWater::cleanup

Return, 1

End


Function byWater::getNumberOfSegment

Return, self.nWaveform

End

Function byWater::getSegmentObject, n

  if n lt 0 or n gt self.nWaveform then begin
    self.print, 3, 'Waveform object index outside of the available objects...'
    return, 0
  endif
  Return, ((*self.waveform)[n])

End



Function byWater::getOutputSegment

  Return, self.computeSegment(0)

End



Function byWater::getFirstSegment

  return, self.computeSegment(1)

End



Function byWater::getLastSegment

  n = self.nWaveform - 1
  return, self.computeSegment(n)

End



Function byWater::getNthSegment, n

  if n lt 0 or n gt self.nWaveform then begin
    self.print, 3, 'Segment number outside of the available segments...'
    return, 0
  endif
  n = self.nWaveform - 1
  return, self.computeSegment(n)

End


Function byWater::computeSegment, n

  nSamples = ((*self.waveform)[n]).n()
  samples = ((*self.waveform)[n]).wave()
  time = ((*self.waveform)[n]).durationFromAnchor()
  luTable = ((*self.waveform)[n]).lut()

  time = indgen(nSamples) + time
  coordinates = self.tracePulse(time)
  intensity = luTable[samples]

  Return, {coor:coordinates, int:intensity}
  
End


; TODO : issue with the array of structure formating
;Function byWater::getAllSegments
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



Pro byWater__define

void = {byWater, $
  nWaveform       : 0, $            ; Number of segment/waveformclass object hold in the bywater class object
  waveform        : ptr_new(), $    ; Pointer to an array of waveformclass object - one object per segment
  inherits plsrayclass $            ; Here we inherit from plsray class as we have onely one ray for multiple segment(s)
  }
  
End