Function trianglemeshclass_sazerac::init, nt, nv, vertInd, vcoor, norcoor, parentShape, scoor, uvcoor, alphacoor, transform

  Compile_opt idl2

  if n_params() eq 0 then begin
    
    print, 'No parameters passed, using default cube as starter...'
    sceneObj = sceneclass("/Users/antoine/Desktop/testwrite.txt")
    shapeObj = shapeclass()
    dum = self.init(plyObj.getNFaces(), plyObj.getNVertex(), plyObj.getFaces(), plyObj.getVertex(), plyObj.getNormals(), shapeObj)
    
  endif
  
  if n_elements(nt) ne 0 then begin
    self.nt = nt
    ;print, 'Number of triangles: ' +  strcompress(string(nt), /remove_all)
  endif else begin
    print, "Number of triangles required..."
    return, 0
  endelse
  if n_elements(nv) ne 0 then begin
    self.nv = nv
    ;print, 'Number of vertice: ' +  strcompress(string(nv), /remove_all)
  endif else begin
    print, "Number of vertice required..."
    return, 0
  endelse
  if n_elements(vertInd) ne 0 then begin
     self.pvertInd = ptr_new(vertInd) 
     ;print, 'Triangles index list: '
     ;print, vertInd
  endif else begin
    print, "Triangle's vertice index list required..."
    return, 0
  endelse
  if n_elements(vcoor) ne 0 then begin
    self.pvcoor = ptr_new(vcoor)
    ;print, 'Vertice coordinates list: '
    ;print, vcoor
  endif else begin
    print, "Vertice's coordinates required..."
    return, 0
  endelse
;  if n_elements(triDiffGeom) ne 0 then begin
;    self.ptriDiffGeom = ptr_new(triDiffGeom)
;  endif else begin
;    print, "Diffgeomclass object for each triangle required..."
;    return, 0
;  endelse
  if n_elements(norcoor) ne 0 then self.pnorcoor = ptr_new(norcoor) else self.pnorcoor = ptr_new(/allocate_heap)
  if n_elements(scoor) ne 0 then self.pscoor = ptr_new(scoor) else self.pscoor = ptr_new(/allocate_heap)
  if n_elements(uvcoor) ne 0 then self.puvcoor = ptr_new(uvcoor) else self.puvcoor = ptr_new(self.getUVs())
  if n_elements(alphacoor) ne 0 then self.pAlphaTexture = ptr_new(alphacoor) else self.pAlphaTexture = ptr_new(/allocate_heap)
  if n_elements(transform) ne 0 then self.ptransform = ptr_new(transform) else self.ptransform = ptr_new(/allocate_heap)
  if n_elements(parentShape) ne 0 then begin
    self.pParentShape = ptr_new(parentShape)
  endif else begin
    print, "shapeclass object required..."
    return, 0
  endelse
  
  
  ; Initializing the object
  return, 1
  
End





Function trianglemeshclass_sazerac::canIntersect

  ; By default a triangle mesh can be intersect by Rays.
  return, 1
  
End


Pro trianglemeshclass_sazerac::cleanup

  Compile_opt idl2
  
;  ptr_free, $
;    self.world
    
End


Function trianglemeshclass_sazerac::duplicate

  return, self
  
End


Function trianglemeshclass_sazerac::getNVertices

  return, self.nv
  
End


Function trianglemeshclass_sazerac::getNTriangles

  return, self.nt
  
End


Function trianglemeshclass_sazerac::getVertIndex, t

  if n_elements(t) eq 0 then begin
    return, (*self.pvertInd)
  endif else begin
    return, (*self.pvertIn)[t,*]
  endelse
  
End


Function trianglemeshclass_sazerac::setVertIndex, t, value

  (*self.pvertIn)[t,*] = value
  
End


Function trianglemeshclass_sazerac::getTriangleObjArr, t

  if n_elements(t) eq 0 then begin
    return, (*self.ptriObjArr)
  endif else begin
    return, (*self.ptriObjArr)[t]
  endelse
  
End


Function trianglemeshclass_sazerac::getVertCoor, t

  if n_elements(t) eq 0 then begin
    return, (*self.pvcoor)
  endif else begin
    return, (*self.pvcoor)[t,*]
  endelse
  
End

Function trianglemeshclass_sazerac::setVertCoor, t, value

  (*self.pvcoor)[t,*] = value
  
End


Function trianglemeshclass_sazerac::getNormCoor, t

  if n_elements(t) eq 0 then begin
    return, (*self.pnorcoor)
  endif else begin
    return, (*self.pnorcoor)[t,*]
  endelse
  
End


Function trianglemeshclass_sazerac::setNormCoor, t, value

  (*self.pnorcoor)[t,*] = value
  
End


Function trianglemeshclass_sazerac::getTangentCoor, t

  if n_elements(t) eq 0 then begin
    return, (*self.pscoor)
  endif else begin
    return, (*self.pscoor)[t,*]
  endelse
  
End


Function trianglemeshclass_sazerac::setTangentCoor, t, value

  (*self.pscoor)[t,*] = value
  
End


Function trianglemeshclass_sazerac::getUVCoor, t

  if n_elements(t) eq 0 then begin
    return, (*self.puvcoor)
  endif else begin
    return, (*self.puvcoor)[t,*]
  endelse
  
End


Function trianglemeshclass_sazerac::setUVCoor, t, value

  (*self.puvcoor)[t,*] = value
  
End


Function trianglemeshclass_sazerac::getAlphaCoor, t

  if n_elements(t) eq 0 then begin
    return, (*self.pAlphaTexture)
  endif else begin
    return, (*self.pAlphaTexture)[t,*]
  endelse
  
End


Function trianglemeshclass_sazerac::setAlphaCoor, t, value

  (*self.pAlphaTexture)[t,*] = value
  
End


Function trianglemeshclass_sazerac::getTransformation

  return, (*self.ptransform)
  
End


Function trianglemeshclass_sazerac::setTransformation, transformation

  if strlowcase(obj_class(transformation)) ne 'transformationclass_sazerac' then begin
    print, 'Please provide a transformation class object (transformationclass_sazerac)...'
    return, 0
  endif else begin
    self.ptransform = ptr_new(transformation)
  endelse

End



Function trianglemeshclass_sazerac::objectBound
 
  dum = self.getDataInObjectSpace()
  minX = min( dum.x(), max = maxX )
  minY = min( dum.y(), max = maxY )
  minZ = min( dum.z(), max = maxZ )
  return, bboxclass(pointclass_sazerac(minX, minY, minZ),$
                    pointclass_sazerac(maxX, maxY, maxZ))
  
  
End


Function trianglemeshclass_sazerac::worldBound

  minX = min( (*self.pvcoor)[*,0], max = maxX )
  minY = min( (*self.pvcoor)[*,1], max = maxY )
  minZ = min( (*self.pvcoor)[*,2], max = maxZ )
  return, bboxclass(pointclass_sazerac(minX, minY, minZ),$
                    pointclass_sazerac(maxX, maxY, maxZ))
    
End


Function trianglemeshclass_sazerac::transformToWorldSpace

  if ptr_valid(self.ptransform) ne 1 then begin
    print, 'Please provide a transformation class object (transformationclass_sazerac) during initialisation...'
    return, 0
  endif else begin
    self.pvcoor = ptr_new( (*ptransform).pointArrayTransformWithPtr((*self.pvcoor)) )
    return, 1
  endelse
  
End


Function trianglemeshclass_sazerac::transformToObjectSpace

  if ptr_valid(self.ptransform) ne 1 then begin
    print, 'Please provide a transformation class object (transformationclass_sazerac) during initialisation...'
    return, 0
  endif else begin
    invTrans = (*ptransform).invertTransformation()
    self.pvcoor = ptr_new( invTrans.pointArrayTransformWithPtr((*self.pvcoor)) )
    return, 1
  endelse
  
End


Function trianglemeshclass_sazerac::getDataInWorldSpace

  if ptr_valid(self.ptransform) ne 1 then begin
    print, 'Please provide a transformation class object (transformationclass_sazerac) during initialisation...'
    return, 0
  endif else begin
    return, (*ptransform).pointArrayTransformWithPtr((*self.pvcoor))
  endelse
  
End


Function trianglemeshclass_sazerac::getDataInObjectSpace

  if ptr_valid(self.ptransform) ne 1 then begin
    print, 'Please provide a transformation class object (transformationclass_sazerac) during initialisation...'
    return, 0
  endif else begin
    invTrans = (*ptransform).invertTransformation()
   return, invTrans.pointArrayTransformWithPtr((*self.pvcoor))
  endelse
  
End


Function trianglemeshclass_sazerac::createTriangleList


End


Function trianglemeshclass_sazerac::getTriangleVerticeCoordinates, ntri
  
  ; If not parameter pass, return the whole array of vertice coordinates
  if n_params() eq 0 then begin
    return, (*self.pvcoor)
    
  ; Else return the specific vertice, can be one or an array of vertice
  endif else begin
    if n_elements(ntri) ge 1 then begin
      p1 = pointarrayclass_sazerac( (*self.pvcoor)[(*self.pvertInd)[ntri,0],*] )
      p2 = pointarrayclass_sazerac( (*self.pvcoor)[(*self.pvertInd)[ntri,1],*] )
      p3 = pointarrayclass_sazerac( (*self.pvcoor)[(*self.pvertInd)[ntri,2],*] )
      return, [p1, p2, p3]
    endif
  endelse

End


Function trianglemeshclass_sazerac::intersect, ray, tHit, rayEpsilon, dg, hitTriangleIndex, angle

;  print,'*******'
;  print, 'Transform Ray Origin...'
;  print, (ray.getOrigin()).xyz()
;  print, 'Transform Ray Direction...'
;  print, (ray.getDirection()).xyz()
;  print, 'Passing the call to the Mesh Class Object...'
;  print,'*******'
  
  ; Making sure that IDL will not stop running if div by 0.0
  !EXCEPT=0
  
  ; Create an intersection index array. 
  ; Each time we test for a ray intersection condition and it fails,
  ; we add one to the array. The final intersecting triangle(s) will the one(s) with 0 value. 
  interArray = bytarr(self.nt) 
  
  ; Compute s1 - page 141 of pbr book
  p1 = pointarrayclass_sazerac( (*self.pvcoor)[(*self.pvertInd)[*,0],*] )
  p2 = pointarrayclass_sazerac( (*self.pvcoor)[(*self.pvertInd)[*,1],*] )
  p3 = pointarrayclass_sazerac( (*self.pvcoor)[(*self.pvertInd)[*,2],*] )
  e1 = ( p1.makeVectorArrayFromPointArray(p2) )
  e2 = ( p1.makeVectorArrayFromPointArray(p3) )
  dray = (ray.getDirection()).duplicateToVectorArrayClass(self.nt)
  s1 = dray.det(e2)
  
;  print, 'List of P1:'
;  print, p1.xyz()
;  print, 'List of P2:'
;  print, p2.xyz()
;  print, 'List of P3:'
;  print, p3.xyz()
  
  
  divisor = s1.dot(e1)
  ; Looking for degenerated triangles - divisor == 0.0
  degenInd = where(divisor eq 0.0, count)
  if count gt 0 then interArray[degenInd] += 1
  
  ; Compute inverse of the divisor to avoid the use of division
  invDiv = 1. / divisor
  
  ; Compute the first barycentric coordinate
  d = p1.makeVectorArray(ray.getOrigin())
  b1 = d.dot(s1) * invDiv
  inB1Ind = where( (b1 lt 0.) or (b1 gt 1.), count)
  if count gt 0 then interArray[inB1Ind] += 1
  
  ; Compute second barycentric coordinate
  s2 = d.det(e1)
  tempS2 = dray.dot(s2) ;changed to dot
  ;b2 = tempS2.xyz() * invDiv
  b2 = tempS2 * invDiv
  ;b2 = dray.det(s2) * invDiv
  inB2Ind = where( (b2 lt 0.) or ((b1 + b2) gt 1.), count)
  ;inB2Ind = where( (b2 ge 0.) and ((b1 + b2) le 1.), count)
  if count gt 0 then interArray[inB2Ind] += 1
  
  ; Compute t to intersection point
  t = e2.dot(s2) * invDiv
  rayInInd = where( (t lt ray.getMint()) or (t gt ray.getMaxt()), count)
  if count gt 0 then interArray[rayInInd] += 1
  
  ; Final triangle filtering
  hitTriangleInd = where(interArray eq 0, finalCount)
  
;  if finalCount gt 0 then begin
;    print, interArray
;    print, 'Intersection coordinates: ', (ray.getOrigin()).xyz() + (ray.getDirection()).xyz() * t  
;  
;    print, 'List of Tirangles Index:'
;    print,(*self.pvertInd)
;    
;    print, 'List of P1:'
;    print, p1.xyz()
;    print, 'List of P2:'
;    print, p2.xyz()
;    print, 'List of P3:'
;    print, p3.xyz()
;
;    print, 'List of E1:'
;    print, e1.xyz()
;    print, 'List of E2:'
;    print, e2.xyz()
;    print, 'List of DRAY:'
;    print, DRAY.xyz()
;    print, 'List of S1:'
;    print, s1.xyz()
;    
;    print, 'List of Divisor:'
;    print, divisor
;    print, 'List of invDivisor:'
;    print, invDiv
;    
;    print, 'List of d:'
;    print, d.xyz()
;    
;    print, 'List of B1:'
;    print, b1
;    print, 'List of S2:'
;    print, s2.xyz()
;    print, 'List of B2:'
;    print, b2
;    
;    print, 'List of T:'
;    print, t
;    
;  endif

  ; For debbug
  ;print, 'Number of triangle(s) intersect: ' + strcompress(string(finalCount), /remove_all) + '...'

  ; If the number of triangle is one - then is it straight forward.
  ; If multiple triangle intersect, then we need to find the one with the smaller t value
  ; which represents the closest one from the viewplane.
  ;
  
  ;########################################################
  ;##################### REWORKED #########################
  ;########################################################
  ; >>> This part might need some rework - as the bellow can be compute only for the intersecting triangle >>> quicker !!!
  ; 19.11.13 - I think this is the way to go but need testing to make sure it works !!!!
  
  ; If one triangle is hit then return this triangle index.
  ; If multiple triangles are hit, then select the closest one.
  
  ; For testing we've put dg as a number of face hit detector
  
  case finalCount of
    0: begin
      ;triangleIndex = !NULL
      return, 0
      end
    1: begin
      ;return, 0
      ;print, 'Only one triangle hit !'
      triangleIndex = hitTriangleInd
      closestHit = hitTriangleInd
      end
    else: begin
      closestHit = min(t[hitTriangleInd], minSub)
      triangleIndex = hitTriangleInd[minSub]
      ;triangleIndex = interArray[hitTriangleInd[minSub]]
      end
  endcase
  
  ; Getting UV coordinates for the selected triangle
  
  ; Make sure that the UVs exist in the object
  ; if (*self.puvcoor) ne !NULL then begin
    
;    p1uv = pointclass_sazerac( (*self.puvcoor)[(*self.pvertInd)[triangleIndex,0],0], (*self.puvcoor)[(*self.pvertInd)[triangleIndex,0],1], (*self.puvcoor)[(*self.pvertInd)[triangleIndex,0],1] * 0.0 )
;    p2uv = pointclass_sazerac( (*self.puvcoor)[(*self.pvertInd)[triangleIndex,1],0], (*self.puvcoor)[(*self.pvertInd)[triangleIndex,1],1], (*self.puvcoor)[(*self.pvertInd)[triangleIndex,1],1] * 0.0 )
;    p3uv = pointclass_sazerac( (*self.puvcoor)[(*self.pvertInd)[triangleIndex,2],0], (*self.puvcoor)[(*self.pvertInd)[triangleIndex,2],1], (*self.puvcoor)[(*self.pvertInd)[triangleIndex,2],1] * 0.0 )
    ;uv = self.getUVs()
    
    ; As an intersection have been found, we need to redefine variables to work with the selected triangle
    p1 = pointclass_sazerac( (*self.pvcoor)[(*self.pvertInd)[triangleIndex,0],*] )
    p2 = pointclass_sazerac( (*self.pvcoor)[(*self.pvertInd)[triangleIndex,1],*] )
    p3 = pointclass_sazerac( (*self.pvcoor)[(*self.pvertInd)[triangleIndex,2],*] )
    e1 = ( p1.makeVector(p2) )
    e2 = ( p1.makeVector(p3) )
    b1 = b1[triangleIndex]
    b2 = b2[triangleIndex]
    b0 = 1. - b1- b2
    
    ; XX: Rework of the partial derivative section
    dpdu = vectorclass()
    dpdv = vectorclass()
    uvs = (self.getUVs())[triangleIndex,*,*]
    ;print, 'UVs coordinates: ', uvs
    
    ; Computing deltas for triangles partial derivatives
    du1 = uvs[0,0,0] - uvs[0,2,0]
    du2 = uvs[0,1,0] - uvs[0,2,0]
    dv1 = uvs[0,0,1] - uvs[0,2,1] 
    dv2 = uvs[0,1,1] - uvs[0,2,1] 
    dp1 = p3.makeVector(p1)
    dp2 = p3.makeVector(p2)
    
    ; Computing partial derivatives for the triangle
;    du1 = p1uv.x() - p3uv.x()
;    du2 = p2uv.x() - p3uv.x()
;    dv1 = p1uv.y() - p3uv.y()
;    dv2 = p2uv.y() - p3uv.y()
;    dp1 = p3.makeVector(p1)
;    dp2 = p3.makeVector(p2)
    ;########################################################
    
    
    
  ;+
  ; ;########################################################
  ; ;##################### ORIGINAL #########################
  ; ;########################################################
  ;  ; Getting UV coordinates for each triangle
  ;  p1uv = pointarrayclass_sazerac( (*self.puvcoor)[(*self.pvertInd)[*,0],0], (*self.puvcoor)[(*self.pvertInd)[*,0],1], (*self.puvcoor)[(*self.pvertInd)[*,0],1] * 0.0 )
  ;  p2uv = pointarrayclass_sazerac( (*self.puvcoor)[(*self.pvertInd)[*,1],0], (*self.puvcoor)[(*self.pvertInd)[*,1],1], (*self.puvcoor)[(*self.pvertInd)[*,1],1] * 0.0 )
  ;  p3uv = pointarrayclass_sazerac( (*self.puvcoor)[(*self.pvertInd)[*,2],0], (*self.puvcoor)[(*self.pvertInd)[*,2],1], (*self.puvcoor)[(*self.pvertInd)[*,2],1] * 0.0 )
  ;  ;uv = self.getUVs()
  ;  
  ;  ; Computing partial derivatives for each triangles
  ;  du1 = p1uv.x() - p3uv.x()
  ;  du2 = p2uv.x() - p3uv.x()
  ;  dv1 = p1uv.y() - p3uv.y()
  ;  dv2 = p2uv.y() - p3uv.y()
  ;  dp1 = p3.makeVectorArrayFromPointArray(p1)
  ;  dp2 = p3.makeVectorArrayFromPointArray(p2)
  ; ;########################################################
  ;-
    
    determinant = du1 * dv2 - dv1 * du2
    if determinant eq 0.0 then begin
      dum = vectorclass(e2.det(e1))
      dum.normalizeLength
      newCoordSys = dum.localCoordinateSystem()
      dpdu = newCoordSys[0]
      dpdv = newCoordSys[1]
    endif else begin
      invdet = 1. / determinant
      dpdu = vectorclass( (dv2 * dp1.xyz() - dv1 * dp2.xyz()) * invdet )
      dpdv = vectorclass( (-du2 * dp1.xyz() + du1 * dp2.xyz()) * invdet )
    endelse
    
    
    ; Interpolate (u,v) triangle parametric coordinate at intersection coordinates
    b0 = 1 - b1 - b2
    tu = b0 * uvs[0,0,0] + b1 * uvs[0,1,0] + b2 * uvs[0,2,0] 
    tv = b0 * uvs[0,0,1] + b1 * uvs[0,1,1] + b2 * uvs[0,2,1]
    
;    print, 't: ', t[triangleIndex]
;    print, 'dpdu: ', dpdu.xyz()
;    print, 'dpdv: ', dpdv.xyz()
    
    ; Test intersection against alpha texture, if present
    if ptr_valid(pAlphaTexture) then begin
      dgLocal = diffgeomclass(ray.traceRay(t[triangleIndex]), tu, tv, dpdu, dpdv, normalclass(0.,0.,0.), normalclass(0.,0.,0.), (*self.pParentShape))
      if (*pAlphaTexture).evaluate(dgLocal) eq 0. then return, 0
    endif
    
    ; Fill in the differentialGeometryclass from triangle hit
    ; The calling sequence is different form the book !!!
    ;dg = diffgeomclass(ray.traceRay(t), dpdu, dpdv, normalclass(0.,0.,0.), normalclass(0.,0.,0.), tu, tv, (*self.pParentShape))
    dg = diffgeomclass(ray.traceRay(t[triangleIndex]), tu, tv, dpdu, dpdv, normalclass(0.,0.,0.), normalclass(0.,0.,0.), (*self.pParentShape))
    ; self.pDiffGeometry = ptr_new(dg)
    self.pDiffGeometry = ptr_new(dg)
    
;    print, 'Dg HitPoint:'
;    print, (dg.getHitPoint()).xyz()
;    print, 'Dg Normalized Normal:'
;    print, (dg.getNormalizedNormal()).xyz()
;    print, 'Dg UVcoor:'
;    print, dg.getUVcoor()
;    print, 'Dg SurfacePartialDerivative:'
;    dum = dg.getSurfacePartialDerivative()
;    print, (dum[0]).xyz(), (dum[1]).xyz()
;    print, 'Dg NormalPartialDerivative:'
;    dum = dg.getNormalPartialDerivative()
;    print, (dum[0]).xyz(), (dum[1]).xyz()
  ; endif
  
  ; inverting the z coordinates of the nn surface normal
  tempvec = (dg.getNormalizedNormal()).xyz()
  ;if tempvec[2] lt 0. then tempvec = [tempvec[0],tempvec[1], abs(tempvec[2])]
  nVec = vectorclass( tempvec )
  dirVec = ray.getDirection()
  
  ;theta = nVec.dot(dirVec) / ( nVec.length() * dirVec.length() )
  theta = dirVec.getRadAngle(nVec)
;  print, !RADEG*(acos(theta)), '  Dg Normalized Normal: ', tempvec
  angle = !RADEG * theta
  
  tHit = closestHit
  rayEpsilon = closestHit * 0.001
  hitTriangleIndex = triangleIndex
  return, 1
  
End


Function trianglemeshclass_sazerac::getUVs

  ; XX: Edit from 23/0114
  ; Modified the UV creation in case of no UV passed by the constructor (if UV exist, we need to check that it is correctly handled)
  ; 
  ; Just for the purpose of being here - need to check how
  ; the UV coordinates are pass from object file to scene...
  
  ; We consider here that the UVs coordinates are vectorarray class (nt,3)
  
  ; AS NO UV COORDINATES ARE FOR THE MOMENT GENERATED WHEN THE ASSET IS GENERATED, WE WILL USE THE DEFAULT.
;  if ptr_valid(self.puvcoor) then begin
;    uv = [$
;      [2 *(*self.puvcoor).x(), 2 *(*self.puvcoor).x() + 1.] ,$
;      [2 *(*self.puvcoor).y(), 2 *(*self.puvcoor).y() + 1.] ,$
;      [2 *(*self.puvcoor).z(), 2 *(*self.puvcoor).z() + 1.]  $
;      ]
;    return, uv
;  endif else begin
    
    ; Here the matrix is defined as follow: [h,i,j]
    ; where h is the number of triangles - any value
    ; i represents the points of the triange - by default 3 - otherwise not a triangle ;)
    ; j represent the u(0) and v(v) coordinates - by default 2 
    uv = fltarr(self.nt, 3, 2)
    uv[*,0,0] = 0.
    uv[*,0,1] = 0.
    uv[*,1,0] = 0.
    uv[*,1,1] = 1.
    uv[*,2,0] = 1.
    uv[*,2,1] = 0.
    return, uv
;  endelse
  
End


Function trianglemeshclass_sazerac::area, triangleIndex

  ; Getting the points of the triangle
  p1 = pointclass_sazerac( (*self.pvcoor)[(*self.pvertInd)[triangleIndex,0],*] )
  p2 = pointclass_sazerac( (*self.pvcoor)[(*self.pvertInd)[triangleIndex,1],*] )
  p3 = pointclass_sazerac( (*self.pvcoor)[(*self.pvertInd)[triangleIndex,2],*] )
  
  e1 = ( p1.makeVector(p2) )
  e2 = ( p1.makeVector(p3) )
  
  temp = vectorclass(e1.det(e2))
  return, 0.5 * temp.length()
  
End


Function trianglemeshclass_sazerac::getShadingGeometry, o2w, dg, triangleIndex

  if ptr_valid(self.pnorcoor) eq 1 and ptr_valid(self.pscoor) eq 1 then begin
    return, (*self.pDiffGeometry) ;dg
  endif else begin
    ; Initialize triangle shading geometry with n and s
    b = fltarr(3)
    uv = self.getUVs()
    A = [ [uv[triangleIndex,0,1]-uv[triangleIndex,0,0], uv[triangleIndex,0,2]-uv[triangleIndex,0,0]], [uv[triangleIndex,1,1]-uv[triangleIndex,1,0], uv[triangleIndex,1,2]-uv[triangleIndex,1,0]] ]
    dg_uv = dg.getUVcoor()
    C = [ dg_uv[0] - uv[triangleIndex,0,0], dg_uv[1] - uv[triangleIndex,1,0] ]
    dum = solveLinearSystem2x2(A, C, x0, x1)
    if dum eq 0 then begin
      b[0] = 1./3.
      b[1] = 1./3.
      b[2] = 1./3.
    endif else begin
      b[1] = x0
      b[2] = x1
      b[0] = 1. - b[1] - b[2]
    endelse
    
    ; Use n and s to compute shading tangents for triangle, ss and ts
    dum = self.getVertIndex(triangleIndex)
    
    ; ***** We consider here that the default transformation object is set to o2w - if not the code need to be change accordingly *****
    if ptr_valid(self.pnorcoor) then begin
      tempNorm = normalclass( (*self.pnorcoor)[dum[0],*] * b[0] + $
                              (*self.pnorcoor)[dum[1],*] * b[1] + $
                              (*self.pnorcoor)[dum[2],*] * b[2] )
      tempNS = (*self.ptransform).normalTransform(tempNorm)
      tempNS.normalizeLength
      ns = tempNS
    endif else ns = dg.getNormalizedNormal()
    
    if ptr_valid(self.pscoor) then begin
      tempTang = normalclass( (*self.pscoor)[dum[0],*] * b[0] + $
                              (*self.pscoor)[dum[1],*] * b[1] + $
                              (*self.pscoor)[dum[2],*] * b[2] )
      tempSS = (*self.ptransform).normalTransform(tempTang)
      tempSS.normalizeLength
      ss = tempSS
    endif else begin
      dumSS = (dg.getSurfacePartialDerivative())[0]
      dumSS.normalizeLength
      ss = dumSS
    endelse
    
    ts = ss.det(ns)
    if ts.squareLength() gt 0.0 then begin
      ts.normalizeLentgh
      ss = ts.det(ns)
    endif else begin
      newCoordSys = ns.localCoordinateSystem()
      ss = newCoordSys[0]
      ts = newCoordSys[1]
    endelse
    
    ; TODO: Need to compute the dndu and dndv
    
    return, diffgeomclass(hpoint, dg_uv[0], dg_uv[1], ss, ts, dndu, dndv, dg.getRefShapeID())
    
  endelse
  
  
End


Function trianglemeshclass_sazerac::setParentShape, shape

if strlowcase(obj_class(shape)) ne 'shapeclass' then begin
  print, "Please provide a shape class object (shapeclass)..."
  return, 0
endif else begin
  self.pParentShape = ptr_new(shape)
  return, 1
endelse

End



Pro trianglemeshclass_sazerac__define

  void = {trianglemeshclass_sazerac, $
    nt            : 0UL ,$          ; number of triangles (required)
    nv            : 0UL ,$          ; number of vertice (required)
    pvertInd      : ptr_new(),$     ; pointer to the array of the triangle's vertice index list (3 per triangle) (required)
    ptriObjArr    : ptr_new(),$     ; pointer to the object array that contains the triangles's object array representing each triangle (required - compute internaly)
    pvcoor        : ptr_new(),$     ; pointer to the array holding vertice's coordinates (object space) (required)
    pnorcoor      : ptr_new(),$     ; pointer to the array holding the vertice's normal vector coordinates (optional)
    pscoor        : ptr_new(),$     ; pointer to the array holding the vertice's tangent vector coordinates (optional)
    puvcoor       : ptr_new(),$     ; pointer to the array (u,v) holding the vertice's texture coordinates (optional)
    pAlphaTexture : ptr_new(),$     ; pointer to the array containing the vertice's alpha value (optional)
    ptransform    : ptr_new(),$     ; pointer to the transformation class object that will manage o2w and w2o transformations
    pParentShape  : ptr_new(),$     ; pointer to the shapeclass properties that is the abstract layer of the mesh (required)
    pDiffGeometry : ptr_new() $     ; Pointer to the diffgeom class generated we an intersection occured (not sure if valid as not in the book)
  }

End