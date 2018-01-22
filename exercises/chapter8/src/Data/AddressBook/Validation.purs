module Data.AddressBook.Validation where

-- (Difficult, Extended) One problem with this user interface is that the validation errors are not displayed next to the form fields they originated from. Modify the code to fix this problem.
-- Hint: the error type returned by the validator should be extended to indicate which field caused the error. You might want to use the following modified Errors type:


import Prelude

import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), PhoneType(..), address, person, phoneNumber)
import Data.Either (Either(..))
import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)

data Field = FirstNameField
            | LastNameField
            | StreetField
            | CityField
            | StateField
            | PhoneField PhoneType

instance eqField :: Eq Field where
  eq FirstNameField FirstNameField = true
  eq  LastNameField LastNameField = true
  eq  StreetField StreetField = true
  eq  CityField CityField = true
  eq  StateField StateField = true
  eq  (PhoneField _) (PhoneField _) = true
  eq _ _ = false

instance showField :: Show Field where
  show f = "cool field"

data ValidationError = ValidationError String Field

instance eqError :: Eq ValidationError where
  eq (ValidationError _ t1) (ValidationError _ t2) = t1 == t2

type Errors = Array String
type Errors' = Array ValidationError

nonEmpty :: ValidationError -> String -> V Errors' Unit
nonEmpty (ValidationError field field') "" = invalid [ValidationError ("Field '" <> field <> "' cannot be empty") field']
nonEmpty _     _  = pure unit

arrayNonEmpty :: forall a. ValidationError -> Array a -> V Errors' Unit
arrayNonEmpty  (ValidationError field field') [] = invalid [ValidationError ("Field '" <> field <> "' must contain at least one value") field']
arrayNonEmpty _     _  = pure unit

lengthIs :: ValidationError -> Int -> String -> V Errors' Unit
lengthIs (ValidationError field field') len value | length value /= len =
  invalid [ValidationError ("Field '" <> field <> "' must have length " <> show len) field']
lengthIs _ _ _ = pure unit

phoneNumberRegex :: Regex
phoneNumberRegex =
  unsafePartial
    case regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of
      Right r -> r

matches :: ValidationError -> Regex -> String -> V Errors' Unit
matches _     regex value | test regex value = pure unit
matches (ValidationError field field') _     _     = invalid [ValidationError ("Field '" <> field <> "' did not match the required format") field' ]

validateAddress :: Address -> V Errors' Address
validateAddress (Address o) =
  address <$> (nonEmpty (ValidationError "Street" StreetField ) o.street *> pure o.street)
          <*> (nonEmpty (ValidationError "City" CityField )   o.city   *> pure o.city)
          <*> (lengthIs (ValidationError  "State" StateField) 2 o.state *> pure o.state)

validatePhoneNumber :: PhoneNumber -> V Errors' PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches (ValidationError "Number" (PhoneField o."type")) phoneNumberRegex o.number *> pure o.number)

validatePerson :: Person -> V Errors' Person
validatePerson (Person o) =
  person <$> (nonEmpty (ValidationError "First Name" FirstNameField ) o.firstName *> pure o.firstName)
         <*> (nonEmpty (ValidationError "Last Name" LastNameField )  o.lastName  *> pure o.lastName)
         <*> validateAddress o.homeAddress
         <*> (arrayNonEmpty (ValidationError "Phone Numbers" (PhoneField HomePhone) ) o.phones *> traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either Errors' Person
validatePerson' p = unV Left Right $ validatePerson p
