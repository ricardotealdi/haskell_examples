type Name = String
type Age = Int
type Language = String
type Person = (Name, Age, Language)

person :: Person
person = ("Jonh Doe", 42, "English")

person_name :: Person -> Name
person_name (name, _, _) = name
person_age (_, age, _) = age
person_language (_, _, language) = language
