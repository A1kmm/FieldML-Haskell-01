module HexM2V2P1_FE (fieldml)
where
  
fieldml = FieldML domains fields expressions

domains = [
  ("SpatialNodeID", Labels (IntegerRange 1 125)),
  ("SpatialElementID", Labels (IntegerRange 1 8)),
  ("SpatialElementNodeID", Labels (IntegerRange 1 27)),
  ("SpatialPosition", CartesianProduct [Reals, Reals, Reals]),
  
  ("VelocityNodeID", Labels (IntegerRange 1 125)),
  ("VelocityElementID", Labels (IntegerRange 1 8)),
  ("VelocityElementNodeID", Labels (IntegerRange 1 27)),
  ("Velocity", CartesianProduct [Reals, Reals, Reals]),
  
  ("PressureNodeID", Labels (IntegerRange 1 27)),
  ("PressureElementID", Labels (IntegerRange 1 8)),
  ("PressureElementNodeID", Labels (IntegerRange 1 27)),
  ("Pressure", Reals),
  
  ("MaterialPropertyID", Labels (StringLabels $ S.fromList ["Rho", "Mu"])),
  ("MaterialPropertySet", SignatureSpace (FSetVariable "MaterialPropertyID") Reals)
  ]

fields = ["spatialInterpolation", "velocityInterpolation", "pressureInterpolation",
          "nodalMaterialProperties", "boundaryConditions"]

-- name :: ((globalnode -> quantity) -> SpatialPosition -> quantity) = ex
defineInterpolation name quantity globalNode =
  Equal (GeneralVariable name (
            SignatureSpace
              (SignatureSpace (FSetVariable globalNode) (FSetVariable quantity))
              (SignatureSpace (FSetVariable "SpatialPosition") (FSetVariable quantity))
            )
        ) ex

expressions = [defineInterpolation "spatialInterpolation" "SpatialPosition" "SpatialNodeID" spatialInterp,
               defineInterpolation "velocityInterpolation" "Velocity" "VelocityNodeID" velocityInterp,
               defineInterpolation "pressureInterpolation" "Pressure" "PressureNodeID" pressureInterp,
               Equal (GeneralVariable "nodalMaterialProperties" (SignatureSpace (FSetVariable "SpatialNodeID")
                                                                                (FSetVariable "MaterialPropertySet"))) 
                     nodalMaterialProperties
              ]


spaceExpression = Equal (GeneralVariable "space" (SignatureSpace (FSetVariable "SpatialNodeID") (FSetVariable "SpacePoint")))