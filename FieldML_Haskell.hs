import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Show.Functions

-- By Randall Britten
-- Auckland Bioengineering Institute
-- 2012
-- University of Auckland
-- Permission granted to redistribute in source code or binary form.  No warranty of any kind is given.
--
-- The ideas here were strongly influenced by Andrew Miller's open source "ModML", with some code copied directly from ModML.

-- This is under construction

type Label = String
type SetOfLabels = Set.Set Label

labelsFromIntegerRange :: Int->Int->SetOfLabels
labelsFromIntegerRange a b =
  Set.fromList $ map show [a..b]

data Map = 

  -- A constant true or false.
  BooleanConstant Bool |

  -- Logical and of two expressions.
  And Map Map |

  -- Logical not of an expression.
  Not Map |

  -- Logical or of two expressions.
  Or Map Map |

  LessThan Map Map |

  Equal Map Map |

  -- Any real value, as a constant.
  RealConstant Double |
  
  -- A free variable...
  RealVariable String |  
  
  -- If x {- then -} a {- else -} b, assumes codomain of "a" and "b" are the same, and that codomain of x is Booleans
  If Map Map Map |
  
  -- Assumes codomains of the two maps are the same, and that Plus has meaning on the codomain.  
  Plus Map Map |
  Minus Map Map |
  Times Map Map |
  Divide Map Map |

  -- Compose f g = f(g(x)), assumes f::b->c, g::a->b (i.e. domain/codomain compatibility).
  Compose Map Map |
  
  -- The domain must be the CartesianProduct of n discrete TopologicalSpaces, with a total cardinality equal to the number of parameters, 
  -- and n equal to the dimensionality of the parameter source.
  FromParameterSource [Double] TopologicalSpace |
  
  Project { factor :: Int, source :: Map } |

  Tuple [Map]
  deriving (Show)

domain :: Map ->TopologicalSpace
domain (RealConstant _ ) = Reals
domain (RealVariable _ ) = Reals
domain (If _ a _ ) = domain a -- Should check somewhere that a and b have same domain, here?
domain (Plus a _) = domain a
domain (Minus a _) = domain a
domain (Times a _) = domain a
domain (Divide a _) = domain a
domain (Compose _ g) = domain g
domain (FromParameterSource _ a) = a
domain (Project n f) = factor n (domain f)
domain (Tuple fs) = Product map domain over fs

  
-- Place holder in the design for a point located in a topological space.
data Point = Point
  deriving (Show)

data TopologicalSpace = 
  Reals |
  Booleans |
  Labels SetOfLabels |
  Product [TopologicalSpace] |
  DisjointUnion SetOfLabels (Label->TopologicalSpace) |
  
  -- The Map have codomain = Booleans, the resulting TopologicalSpace is the subset of the BooleanMap's domain where the BooleanMap evaluates to True.
  SimpleSubset Map |
  
  -- Used for creating the quotient TopologicalSpace from the provided TopologicalSpace. The map is required to be a boolean map.
  -- The resulting space is like the original space, but with points where the boolean map evaluates to True treated as a single point.
  Quotient TopologicalSpace TopologicalSpace Map |
  
  -- If the given space is a smooth manifold then this constructs the tangent space at that point.
  TangetSpaceAtPoint TopologicalSpace Point
  deriving (Show)

-- Tests
real2 = Product [Reals, Reals]
real3 = Product [Reals, Reals, Reals]

elementIds = labelsFromIntegerRange 1 4
  
f :: Label->TopologicalSpace
f "1" = Reals
f _ = Reals

m1 = DisjointUnion elementIds f

m2 = Product [real2, Labels elementIds]

x = RealVariable "x"

expression1 :: Map
expression1 =  (x `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` x)

-- Todo: get a chart for a topological space, and name the coordinates in the chart so that they can be mapped to the free variables of a real expression.

unitLineSegment = SimpleSubset expression1

-- As above, but all inline:
unitLineSegment' = 
  SimpleSubset (
    (x `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` x)
  )

unitSquare = 
  SimpleSubset (
    ((Project 1) `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` (Project 1)) 
    `And`
    ((Project 2) `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` (Project 2))
  )


-- Just playing with Haskell Syntax here for convenience.  Will eventually delete everything below this line, and this comment.
