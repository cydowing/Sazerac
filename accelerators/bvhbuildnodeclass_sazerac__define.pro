Function bvhbuildnodeclass_sazerac::init
  
  Compile_opt idl2
  
  return, 1

End


Function bvhbuildnodeclass_sazerac::initInterior, axs, pChild1, pChild2

  (*self.pChildNodes)[0] = pChild1
  (*self.pChildNodes)[1] = pChild2
  self.bounds = ( (*(*self.pChildNodes)[0]).worldBound() ).unionWithBox( (*(*self.pChildNodes)[1]).worldBound() )
  self.splitAxis = axs
  ; As it is an interior node, no primitives are store inside the node
  self.nPrimitives = 0
  return, 1
  
End


Function bvhbuildnodeclass_sazerac::initLeaf, first, n, bbox

  (*self.pChildNodes)[0] = ptr_new(!null)
  (*self.pChildNodes)[1] = ptr_new(!null)
  
  self.firstPrimOffset = first
  self.nPrimitives = n
  self.bounds = bbox
  return, 1
  
End


Function bvhbuildnodeclass_sazerac::getbounds

  return, self.bounds
  
End


Function bvhbuildnodeclass_sazerac::getpChildNode

  return, self.pChildNodes
  
End


Function bvhbuildnodeclass_sazerac::getSplitAxis

  return, self.splitAxis
  
End


Function bvhbuildnodeclass_sazerac::getFirstPrimOffset

  return, self.firstPrimOffset
  
End


Function bvhbuildnodeclass_sazerac::getNPrimitives

  return, self.nPrimitives
  
End


Pro bvhbuildnodeclass_sazerac__define

  void = {bvhBuildNodeclass, $
    bounds            : bboxclass ,$            ; Bounding box of all the children(s) primitive(s) bellow
    pChildNodes       : Ptr_new(Ptrarr(2)), $        ; Pointer to childs nodes.
    splitAxis         : 0UL ,$                  ; Splitting axis
    firstPrimOffset   : 0UL ,$                  ;
    nPrimitives       : 0UL  $
  }

End