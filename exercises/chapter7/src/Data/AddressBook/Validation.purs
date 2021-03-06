module Data.AddressBook.Validation where

import Prelude

import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), address, person, phoneNumber)
import Data.Either (Either(..))
import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)

type Errors = Array String

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid ["Field '" <> field <> "' cannot be empty"]
nonEmpty _     _  = pure unit

-- (Medium) Using the matches validator, write a validation function which checks that a string is not entirely whitespace. Use it to replace nonEmpty where appropriate.
nonEmpty' :: String -> String -> V Errors Unit
nonEmpty' field value = matches field nonEmptyRegex value

arrayNonEmpty :: forall a. String -> Array a -> V Errors Unit
arrayNonEmpty field [] = invalid ["Field '" <> field <> "' must contain at least one value"]
arrayNonEmpty _     _  = pure unit

lengthIs :: String -> Int -> String -> V Errors Unit
lengthIs field len value | length value /= len = invalid ["Field '" <> field <> "' must have length " <> show len]
lengthIs _     _   _     = pure unit

nonEmptyRegex :: Regex
nonEmptyRegex =
  unsafePartial
    case regex "^\\S+.*$" noFlags of
      Right r -> r

phoneNumberRegex :: Regex
phoneNumberRegex =
  unsafePartial
    case regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of
      Right r -> r

stateRegex :: Regex
stateRegex =
  unsafePartial
    case regex "^[A-z]{2}$" noFlags of
      Right r -> r

matches :: String -> Regex -> String -> V Errors Unit
matches _     regex value | test regex value = pure unit
matches field _     _     = invalid ["Field '" <> field <> "' did not match the required format"]

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (nonEmpty' "Street" o.street *> pure o.street)
          <*> (nonEmpty' "City"   o.city   *> pure o.city)
  -- (Easy) Use a regular expression validator to ensure that the state field of the Address type contains two alphabetic characters. Hint: see the source code for phoneNumberRegex.
          <*> (matches "State" stateRegex o.state *> pure o.state)

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches "Number" phoneNumberRegex o.number *> pure o.number)

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (nonEmpty' "First Name" o.firstName *> pure o.firstName)
         <*> (nonEmpty' "Last Name"  o.lastName  *> pure o.lastName)
         <*> (traverse validateAddress o.homeAddress)
         <*> (arrayNonEmpty "Phone Numbers" o.phones *> traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = unV Left Right $ validatePerson p
