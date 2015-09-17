type Name = String
type Language = String
type University = String

data Person = Programmer Name Language | Student Name University
                deriving(Show)

programmer = Programmer "Someone" "Haskell"
student = Student "Someone" "UNIP"

is_programmer :: Person -> Bool
is_programmer (Programmer _ _) = True
is_programmer _ = False

is_student :: Person -> Bool
is_student (Student _ _) = True
is_student _ = False
