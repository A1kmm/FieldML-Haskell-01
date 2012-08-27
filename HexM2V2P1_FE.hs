module HexM2V2P1_FE (fieldml)
where

fieldml = FieldML domains fields expressions

stringLabel = StringLabels . S.singleton
stringLabels = StringLabel . S.fromList
domainMapCase = foldl' (\dm (l, fs) -> DomainMapIf l (DomainMapConstant fs) dm) . DomainMapConstant

domains = [
  ("SpatialNodeID", Labels (IntegerRange 1 125)),
  ("SpatialElementID", Labels (IntegerRange 1 8)),
  ("SpatialElementNodeID", Labels (IntegerRange 1 27)),
  ("DimensionID", Labels (IntegerRange 1 3)),
  ("SpatialPosition", SignatureSpace (FSetVariable "DimensionID") Reals),
  ("VelocityNodeID", Labels (IntegerRange 1 125)),
  ("VelocityElementID", Labels (IntegerRange 1 8)),
  ("VelocityElementNodeID", Labels (IntegerRange 1 27)),
  ("Velocity", SignatureSpace (FSetVariable "DimensionID") Reals),
  ("PressureNodeID", Labels (IntegerRange 1 27)),
  ("PressureElementID", Labels (IntegerRange 1 8)),
  ("PressureElementNodeID", Labels (IntegerRange 1 27)),
  ("Pressure", Reals),
  ("MaterialPropertyID", Labels (stringLabels ["Rho", "Mu"])),
  ("MaterialPropertySet", SignatureSpace (FSetVariable "MaterialPropertyID") Reals),
  ("FixedInletWallNothing", DisjointUnion fixedInletNothing
                                          (domainMapCase UnitSpace [("FixedWall", FSetVariable "Velocity"),
                                                                    ("FixedInlet", FSetVariable "Velocity")]))
  ]

fixedInletWallNothing = stringLabels ["Nothing", "FixedInlet", "FixedWall"]

fields = ["spatialInterpolation", "velocityInterpolation", "pressureInterpolation",
          "nodalMaterialProperties", "boundaryConditions"]

-- name :: ((globalnode -> quantity) -> SpatialPosition -> quantity) = ex
defineInterpolation name quantity globalNode ex =
  Equal (GeneralVariable name (
            SignatureSpace
              (SignatureSpace (FSetVariable globalNode) (FSetVariable quantity))
              (SignatureSpace (FSetVariable "SpatialPosition") (FSetVariable quantity))
            )
        ) ex

fixedWallNodes = (MultiDimArray (IntegerParameterVector [1,2,3,4,5,7,9,10,11,12,13,14,17,20,24,28,29,30,31,32,33,34,35,37,39,
                                                         41,44,46,47,48,50,51,52,53,54,57,60,64,65,66,67,68,70,72,74,76,77,78,79,80,83,86,
                                                         89,90,91,92,93,94,95,97,99,101,102,103,104,105,106,107,108,111,114,115,116,117,118,
                                                         120,122,123,124,125] (FSetVariable "VelocityNodeID")) (Labels Integers))
fixedInletNodes = (MultiDimArray (IntegerParameterVector [6,15,16,23,36,42,81,82,96] (FSetVariable "VelocityNodeID")) (Labels Integers))

nodalMaterialProperties = undefined -- To be defined.

expressions = [defineInterpolation "spatialInterpolation" "SpatialPosition" "SpatialNodeID" spatialInterp,
               defineInterpolation "velocityInterpolation" "Velocity" "VelocityNodeID" velocityInterp,
               defineInterpolation "pressureInterpolation" "Pressure" "PressureNodeID" pressureInterp,
               Equal (GeneralVariable "nodalMaterialProperties" (SignatureSpace (FSetVariable "SpatialNodeID")
                                                                                (FSetVariable "MaterialPropertySet")))
                     nodalMaterialProperties,
               Equal (GeneralVariable "boundaryConditions" (SignatureSpace (FSetVariable "VelocityNode")
                                                                           (FSetVariable "FixedInletWallNothing"))) $
                 Lambda (GeneralVariable "node" (FSetVariable "VelocityNodeID")) $
                   If (Exists i ((Apply fixedWallNodes i) `Equal` node))
                      (DisjointUnionValue (StringLabel "FixedWall" fixedInletWallNothing)
                                                       (MultiDimArray (RealParameterVector [0,0,0]) (FSetVariable "DimensionID"))) $ 
                     If (Exists i ((Apply fixedInletNodes i) `Equal` node))
                      (DisjointUnionValue (StringLabel "FixedInlet" fixedInletWallNothing)
                                          (MultiDimArray (RealParameterVector [0,1,0]) (FSetVariable "DimensionID")))
                      (DisjointUnionValue (StringLabel "Nothing" fixedInletWallNothing) UnitElement)
                     where i = GeneralVariable "i" (Labels Integers)
              ]

spaceExpression =
  Equal (GeneralVariable "space" (SignatureSpace (FSetVariable "SpatialNodeID") (FSetVariable "SpacePoint")))
        ()
