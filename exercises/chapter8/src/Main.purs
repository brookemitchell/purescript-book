module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), PhoneType(..), examplePerson)
import Data.AddressBook.Validation (Errors', Field(..), ValidationError(..), validatePerson')
import Data.Array ((..), length, modifyAt, zipWith, elem)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Foreign (ForeignError, readString, toForeign)
import Data.Foreign.Index (index)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (fromJust, fromMaybe)
import Partial.Unsafe (unsafePartial)
import React (Event, ReactClass, ReactElement, ReactState, ReactThis, ReadWrite, createClass, createFactory, spec, writeState, readState)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)


newtype AppState = AppState
  { person :: Person
  , errors :: Errors'
  }

initialState :: AppState
initialState = AppState
  { person: examplePerson
  , errors: []
  }

valueOf :: Event -> Either (NonEmptyList ForeignError) String
valueOf e = runExcept do
  target <- index (toForeign e) "target"
  value <- index target "value"
  readString value

updateAppState
  :: forall props eff
   . ReactThis props AppState
  -> (String -> Person)
  -> Event
  -> Eff ( console :: CONSOLE
         , state :: ReactState ReadWrite
         | eff
         ) Unit
updateAppState ctx update e =
  for_ (valueOf e) \s -> do
    let newPerson = update s

    log "Running validators"
    case validatePerson' newPerson of
      Left errors -> writeState ctx (AppState { person: newPerson, errors: errors })
      Right _ -> writeState ctx (AppState { person: newPerson, errors: [] })

fieldWithErrors :: Field → Errors' → ReactElement → ReactElement
fieldWithErrors field errors children =
  D.div [ P.className "form-group" ] [
    children
    , if test then D.div [ P.className "alert alert-danger" ] [ D.text "uhuh" ] else D.div [] []
    ]
  where
    dumDum = ValidationError "" field
    test = elem dumDum errors

renderValidationError (ValidationError errTxt _) = D.div [ P.className "alert alert-danger" ] [ D.text errTxt ]

renderValidationErrors [] = []
renderValidationErrors xs = [ D.div [] (map renderValidationError xs)]

addressBook :: forall props. ReactClass props
addressBook = createClass $ spec initialState \ctx -> do
  AppState { person: Person person@{ homeAddress: Address address }, errors } <- readState ctx

  let formField name hint value update =
        D.div []
              [ D.label [ P.className "col-sm-2 control-label" ]
                        [ D.text name ]
              , D.div [ P.className "col-sm-3" ]
                      [ D.input [ P._type "text"
                                , P.className "form-control"
                                , P.placeholder hint
                                , P.value value
                                , P.onChange (updateAppState ctx update)
                                ] []
                      ]
              ]

      renderPhoneNumber (PhoneNumber phone) index =
        fieldWithErrors (PhoneField HomePhone) errors $ formField (show phone."type") "XXX-XXX-XXXX" phone.number \s ->
          Person $ person { phones = fromMaybe person.phones $ modifyAt index (updatePhoneNumber s) person.phones }

      updateFirstName s = Person $ person { firstName = s }
      updateLastName  s = Person $ person { lastName  = s }

      updateStreet s = Person $ person { homeAddress = Address $ address { street = s } }
      updateCity   s = Person $ person { homeAddress = Address $ address { city   = s } }
      updateState  s = Person $ person { homeAddress = Address $ address { state  = s } }

      updatePhoneNumber s (PhoneNumber o) = PhoneNumber $ o { number = s }

  pure $
    D.div [ P.className "container" ]
          [ D.div [ P.className "row" ]
                  [ D.form [ P.className "form-horizontal" ] $
                           [ D.h3' [ D.text "Basic Information" ]

                           , fieldWithErrors FirstNameField errors $ formField "First Name" "First Name" person.firstName updateFirstName
                           , fieldWithErrors LastNameField errors $ formField "Last Name"  "Last Name"  person.lastName  updateLastName

                           , D.h3' [ D.text "Address" ]

                           , fieldWithErrors StreetField errors $ formField "Street" "Street" address.street updateStreet
                           , fieldWithErrors CityField errors $ formField "City"   "City"   address.city   updateCity
                           , fieldWithErrors StateField errors $ formField "State"  "State"  address.state  updateState

                           , D.h3' [ D.text "Contact Information" ]
                           ]
                           <> zipWith renderPhoneNumber person.phones (0 .. length person.phones)
                  ]
          ]

main :: Eff ( console :: CONSOLE
            , dom :: DOM
            ) Unit
main = void do
  log "Rendering address book component"
  let component = D.div [] [ createFactory addressBook unit ]
  doc <- window >>= document
  ctr <- getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
  render component (unsafePartial fromJust ctr)
