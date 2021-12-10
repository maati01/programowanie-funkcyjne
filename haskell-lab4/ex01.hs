polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r,phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r * cos phi, r * sin phi)

newtype CartesianCoord'' a = MkCartesianCoord'' (a,a)
newtype PolarCoord'' a = MkPolarCoord'' (a,a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r,phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)

--2
newtype CartesianCoord''' a = MkCartesianCoord''' (a,a,a)
newtype CylindricalCoord a = MkCylindricalCoord (a,a,a)
newtype SphericalCoord a = MkSphericalCoord (a,a,a)

polarToCylindrical :: Floating a => CylindricalCoord a -> CartesianCoord''' a
polarToCylindrical (MkCylindricalCoord (r,phi,z)) = MkCartesianCoord''' (r * cos phi, r * sin phi, z)

polarToSpherical :: Floating a => SphericalCoord a -> CartesianCoord''' a
polarToSpherical (MkSphericalCoord (p,phi,psi)) = MkCartesianCoord''' (p * cos phi * cos psi, p * sin phi * cos psi, p * sin psi)

--3
personInfoToString :: (String,String,String) -> String
personInfoToString (nm,snm,addr) =
 "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

 --4
type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String

personInfoToString' :: PersonInfoToStringType'
personInfoToString' (nm, snm, addr) = 
 "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

--5
data Person nm snm addr = Person
     { name :: nm
     , surname :: snm
     , address :: addr
     } deriving (Show)

personInfoToString'' :: (Show a) => Person String String a -> String 
personInfoToString''  (Person {name = nm, surname = snm, address = addr}) =
 "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ show addr

