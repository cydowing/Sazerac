Function ecef2lla, ecef, CSV = CSV

; Some WGS84 ellipsoid constants
;double a = 6378137.D               ; Earth's radius
e = 8.1819190842622e-2     ; Earth's eccentricity

asq = !CONST.R_EARTH^2
esq = e^2

x = ecef[*,0]
y = ecef[*,1]
z = ecef[*,2]

b = sqrt( asq * (1.-esq) )
bsq = b^2
ep = sqrt( (asq - bsq)/bsq )
p = sqrt( x^2 + y^2 )
th = atan(!CONST.R_EARTH * z, b * p)

lon = atan(y,x)
lat = atan( z + (ep^2) * b * ( (sin(th))^3 ), ( p - esq * !CONST.R_EARTH * (cos(th))^3 ) );

N = !CONST.R_EARTH/( sqrt( 1. - esq * (sin(lat))^2) )

alt = p / cos(lat) - N

; mod lat to 0-2pi
lon = lon mod (2. * !PI)

;correct for numerical instability in altitude near exact poles:
;(after this correction, error is about 2 millimeters, which is about
;the same as the numerical precision of the overall function)
k = abs(x) lt 1. and abs(y) lt 1.
alt[k] = abs(z[k])-b

lat = lat * !radeg
lon = lon * !radeg
ret = {lat : lat, long : lon, alti : alt}

if keyword_set(CSV) then write_csv, 'output_test.csv', lat, lon, alt
 
Return, ret

End

