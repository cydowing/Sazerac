Function bvhaccelclass_sazerac::init, primitiveArray, mp, splitMethod
  
  Compile_opt idl2
  
  ; TODO: we consider here that all primitive are triangle meshes... to be modify if non-triangle mesh are used
  
  if n_elements(primitiveArray) eq 0 then begin
    print, "ERROR - No primitive(s) provided for initialization..."
    return, 0
  endif
  
  ; Initializing the class object
  self.primitiveArr = ptr_new(primitiveArray)
  
  if mp gt 255 then self.mxPrimsInNode = 255 else self.mxPrimsInNode = mp
  
  case 1 of
    strlowcase(splitMethod) eq 'sah' : self.splitMethod = 0
    strlowcase(splitMethod) eq 'middle' : self.splitMethod = 1
    strlowcase(splitMethod) eq 'equal' : self.splitMethod = 2
    else : begin
      print, "Unknown splittin method, use default 'Surface Area Heuristic' method..."
      self.splitMethod = 0
      end
  endcase 
    
  ; Initialize _buildData_ array for primitives
  infoArray = replicate(self.BVHPrimitiveInfo(), n_elements(primitiveArray))
  
  loopFlag = 0UL
  while loopFlag lt n_elements(primitiveArray) do begin
    tempBBox = primitiveArray[loopFlag].worldBound()
    infoArray[loopFlag].bbox = tempBBox
    pMin = (tempBBox.getpMin()).xyz()
    pMax = (tempBBox.getpMax()).xyz()
    infoArray[loopFlag].bboxCentroid = pointclass( 0.5 * pMin + 0.5 * pMax )
    infoArray[loopFlag].primID = loopFlag
    
    loopFlag += 1UL
  endwhile
  
  ; Not sure that we need to store this information in the core data structure
  self.pBhvPrimInfo = ptr_new(infoArray)
  
  ; Recursively build BVH tree for primitives
  totalNodes = 0
  orderedPrims = primitiveArray
  root = self.recursiveBuild(infoArray, 0, n_elements(primitiveArray), totalNodes, orderedPrims)
  
  primitiveArray = primitiveArray[orderedPrims] ; TODO: not sure at all
  
  ; Compute representation of depth-first traversal of BVH tree
  offset = 0;
  dum = self.flattenBVHTree(root[i], offset)
  if offset eq totalNodes then begin
    print, 'Error... aborting."
    return, 0
  endif
  
  ; NOTE: I keep this just for development informations.
  ;  PBRT_BVH_FINISHED_CONSTRUCTION(this);




  return, 1

End


Function bvhaccelclass_sazerac::flattenBVHTree, node, offset

  linearNode = self.linearBVHNode()
  linearNode.bounds = node


End


Function bvhaccelclass_sazerac::linearBVHNode

  void = { linearBVHNode, $
    bounds : bboxclass ,$
    primOffset : 0UL ,$             ; leaf
    secondChildOffset : 0UL ,$      ; interior
    nPrimitives : 0 ,$              ; 0-> interior node
    axs : 0 ,$                      ; interior node: xyz
    pad : intarr(2) $               ; ensure 32 byte total size
    }
    
  return, void

End


Function bvhaccelclass_sazerac::intersect


End


Function bvhaccelclass_sazerac::intersectP


End


Function bvhaccelclass_sazerac::recursiveBuild, buildData, startID, endID, totalNodes, orderedPrim

   orderedPrim = 0
   totalNodes++
   node = bvhbuildnodeclass()
   for i = startID, endID, 1 do begin
    if i eq startID then begin
      tempBbox = (buildData[i].bbox).worldBound()
    endif else begin
      tempBbox = tempBbox.unionWithBox( (buildData[i].bbox).worldBound() )
    endelse
   endfor
   nPrim = endID - startID
   
   if nPrim eq 1 then begin
   
    firstPrimOffset = n_elements(orderedPrim)
    wFlag = 0UL
    while wFlag lt endID do begin
      primNum = buildData[wFlag].primID
      if wFlag eq 0 then orderedPrim = (*self.primitiveArray)[primNum] else orderedPrim = [orderedPrim, (*self.primitiveArray)[primNum]]
      wFlag += 1
    endwhile
    dum = node.initLeaf(firstPrimOffset, nPrim, tempBbox)
   
   endif else begin
     
     for i = startID, endID, 1 do begin
       if i eq startID then begin
         centroidBounds = (buildData[i].bbox).worldBound()
       endif else begin
         centroidBounds = centroidBounds.unionWithBox( (buildData[i].bbox).worldBound() )
       endelse
     endfor
     
     dim = centroidBounds.maximumExtend()
     
     mid = (startId + endID) / 2.
     pMax =  (centroidBounds.getpMax()).xyz()
     pMin =  (centroidBounds.getpMin()).xyz()
     if pMax[dim] eq pMin[dim] then begin
       wFlag = 0UL
       while wFlag lt endID do begin
         primNum = buildData[wFlag].primID
         if wFlag eq 0 then orderedPrim = (*self.primitiveArray)[primNum] else orderedPrim = [orderedPrim, (*self.primitiveArray)[primNum]]
         wFlag += 1
       endwhile
       dum = node.initLeaf(firstPrimOffset, nPrim, tempBbox)
       return, node
     endif
   
   ; Partition primitives based on split method
   case self.splitMethod of
    0: begin
      print, "SAH Method...'
      end
    1: begin
      print, "Middle Method...'
      pmid = 0.5 * (pMin[dim] + pMax[dim])
      midPtr = self.CompareToMid( dim, mid, startID, endID)
      end
    2: begin
      print, "Equal Method..."
      End
   endcase
   
     
   endelse
   
   
   
   
End





Function bvhaccelclass_sazerac::CompareToMid, dim, mid, startID, endID
  
  dum = (*self.pBhvPrimInfo)[startID:endID]
  nDum = n_elements(dum)
  temp = fltarr(nDum, 3)
  dWhile = 0Ul
  while dWhile lt nDum do begin
    temp[dWhile,*] = (dum[dWhile]).xyz()
  endwhile
  tTemp = temp[*,dim]
  dff = where( tTemp lt mid, count) 
  if count gt 0 then return, dff else return, !null
  
End



Pro bvhaccelclass_sazerac__define

  void = {bvhaccelclass_sazerac, $
    primitiveArr    : Ptr_new() ,$       ; Array of primitive from the scene
    pBhvPrimInfo    : Ptr_new()  ,$      ; Pointer to the array of information
    mxPrimsInNode   : 0U ,$              ; Maximum number of primitive(s) per node
    splitMethod     : 0B  $               ; Splitting method used to create the tree
  }

End
