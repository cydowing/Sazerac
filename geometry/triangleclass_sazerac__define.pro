Function triangleclass_sazerac::init, vertID, parentMesh;, AssodiffGeom

  ; vertID must be a ptrarr
  self.vertID = vertID
  ; pointer the the parent mesh
  self.pParentMesh = ptr_new(parentMesh)
  
  ; Initializing the object
  return, 1
  
End


Pro triangleclass_sazerac::cleanup

  Compile_opt idl2
  ptr_free, pParentMesh, pAssodiffGeom
  
End


Function triangleclass_sazerac::objectBound

  
  
End 



Pro triangleclass_sazerac__define

  void = {triangleclass_sazerac, $
    vertexID        : ptrarr(3),$            ; pointer array to the vertex list of the triangle
    pParentMesh     : ptr_new() ,$           ; pointer to the parent mesh
    ;    pAssodiffGeom   : ptr_new() ,$           ; pointer to triangle's differentialGeometry class
    inherits diffgeomclass $                 ; inherit of the diffgeomclass
  }

End