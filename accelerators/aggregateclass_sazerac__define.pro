Function aggregateclass_sazerac::init

  Compile_opt idl2
  
  return, 1
  
End


Function aggregateclass_sazerac::nextPrimitiveID, previousPrimitiveID

  self.primitiveID = previousPrimitiveID + 1
  return, 1
  
End


Function aggregateclass_sazerac::getPrimitiveID

  return, self.primitiveID
  
End


Function aggregateclass_sazerac::canIntersect

  ; By default the primitive can intersect - might not be true if light source ???
  ; TODO: Need to check on this...
  return, 1
  
End


Function aggregateclass_sazerac::worldBound

  ; Use the mesh worldBound method to get the information
  return, (*self.pMesh).worldBound()
  
End


Function aggregateclass_sazerac::intersect, ray

  return, intersectionclass
  
End


Function aggregateclass_sazerac::intersectP, ray


End


Function aggregateclass_sazerac::getAreaLight

    print, 'Unimplemented aggregateclass_sazerac::getAreaLight() method caller...'
    return, 0
  
End


Function aggregateclass_sazerac::getBSDF, diffgeom, o2w, memoryArena

    print, 'Unimplemented aggregateclass_sazerac::getBSDF() method caller...'
    return, 0

End


Function aggregateclass_sazerac::getBSSRDF, diffgeom, o2w, memoryArena

    print, 'Unimplemented aggregateclass_sazerac::getBSSRDF() method caller...'
    return, 0

End


Pro aggregateclass_sazerac__define

  void = {aggregateclass_sazerac, $
    primitiveID    : 0UL ,$           ; Unique primitive ID
    pMesh          : Ptr_new(),$      ; pointer to mesh object
    pAreaLight     : Ptr_new() $ ; pointer to the arealight class object if any
  }

End
