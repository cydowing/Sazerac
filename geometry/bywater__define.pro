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



Function byWater::getNumberOfSegment

  Return, N_elements(*self.N)

End



Function byWater::getLastSegment

  n = self.getNumberOfSegment()
  nSamples = (*self.N)[n-1]
  samples = (*self.Pulse)[-nSamples:*]
  time = (*self.Duranchor)[n-1]
  luTable = *self.Lutable

  time = Indgen(nSamples) + time
  coordinates = self.tracePulse(time)
  intensity = luTable[samples]

  Return, {coor:coordinates, int:intensity}

End


Function byWater::getFirstSegment

  nSamples = (*self.N)[1]
  samples = (*self.Pulse)[(*self.N)[0]:(*self.N)[0]+nSamples-1]
  time = (*self.Duranchor)[1]
  luTable = *self.Lutable

  time = Indgen(nSamples) + time
  coordinates = self.tracePulse(time)
  intensity = luTable[samples]

  Return, {coor:coordinates, int:intensity}

End



Function byWater::getSegmentNumber, n

  nSamples = (*self.N)[n]
  if n eq 0 then begin
    samples = (*self.Pulse)[0:nSamples-1]
  endif else begin
    samples = (*self.Pulse)[(*self.N)[n-1]:(*self.N)[n-1]+nSamples-1]
  endelse
  time = (*self.Duranchor)[n]
  luTable = *self.Lutable

  time = Indgen(nSamples) + time
  coordinates = self.tracePulse(time)
  intensity = luTable[samples]

  Return, {coor:coordinates, int:intensity}
  Return, 1

End



Pro byWater__define

void = {byWater, $
  void      : 0, $
  inherits waveformclass, $
  inherits plsrayclass $
  }
  
End