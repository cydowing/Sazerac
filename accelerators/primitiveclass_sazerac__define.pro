Function primitiveclass_sazerac::init

  Compile_opt idl2
  
  return, 1
  
End


Function primitiveclass_sazerac::nextPrimitiveID, previousPrimitiveID

  self.primitiveID = previousPrimitiveID + 1
  return, 1
  
End


Function primitiveclass_sazerac::getPrimitiveID

  return, self.primitiveID
  
End


Function primitiveclass_sazerac::canIntersect

  ; By default the primitive can intersect - might not be true if light source ???
  ; TODO: Need to check on this...
  return, 1
  
End


Function primitiveclass_sazerac::worldBound

  ; Use the mesh worldBound method to get the information
  return, (*self.pMesh).worldBound()
  
End


Function primitiveclass_sazerac::intersect, ray, isect

  return, intersectionclass()
  
End


Function primitiveclass_sazerac::intersectP, ray

  return, 1 ; or 0 is not intersect by the ray

End


Function primitiveclass_sazerac::getAreaLight

  ; Returns a pointer to the Area Light that describes the primitive's emission distribution.
  ; If the primitive is not emissive, then it returns NULL
  return, (*self.pAreaLight)
  
End


Function primitiveclass_sazerac::getBSDF, diffgeom, o2w, memoryArena

  ; return a BSDF class object that describes local light-scattering properties
  ; at the intersection point.
  ; Define in section 9.1 of PBR book.
  ; MemoryArena is describes in section 9.1.1 of PBR book.

End


Function primitiveclass_sazerac::getBSSRDF, diffgeom, o2w, memoryArena

  ; Return a BSSRDF class object that describes subsurfaces scattering inside the primitive.
  ; Define in section 16.5 of PBR book.
  ; MemoryArena is describes in section 9.1.1 of PBR book.

End


Pro primitiveclass_sazerac__define

  void = {primitiveclass_sazerac, $
    primitiveID    : 0UL ,$           ; Unique primitive ID
    pMesh          : Ptr_new(),$      ; pointer to mesh object
    pAreaLight     : Ptr_new() $      ; pointer to the arealight class object if any


  }

End
