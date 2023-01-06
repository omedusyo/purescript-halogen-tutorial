module Component.GithubLookup where

import Prelude

import Data.Maybe ( Maybe(..) )
import Data.Either ( Either(..) )

import Halogen as H
import Halogen.HTML as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Effect.Console as Console

-- import Effect.Class (class MonadEffect)
import Effect.Aff.Class (class MonadAff)

import Web.Event.Event (Event)
import Web.Event.Event as Event


import Affjax as Affjax
import Affjax.ResponseFormat as AffjaxResponseFormat
import Affjax.Web as AffjaxWeb


data EventuallyPossibly e a = 
    NotLoaded
  | Loading
  | Error e
  | Ok a

eitherToEventuallyPossibly :: forall e a . Either e a -> EventuallyPossibly e a
eitherToEventuallyPossibly =
  case _ of
    Left err -> Error err
    Right x -> Ok x

-------------Input-----------------
type Input = Unit

-------------Model-----------------
type Model =
  { username :: String
  , result :: EventuallyPossibly Affjax.Error String
  }

initModel :: Input -> Model
initModel _ =
  { username: "start-username"
  , result: NotLoaded
  }

-------------Msg-----------------
data Msg =
    SubmitButtonClicked Event
  | UsernameChanged String

-------------Update-----------------
update :: forall m output . MonadAff m => Msg -> H.HalogenM Model Msg () output m Unit
update =
  case _ of
    UsernameChanged usernameInput -> do
      H.liftEffect $ Console.log ("msg: " <> usernameInput)
      H.modify_ (\model -> model { username = usernameInput })
      model <- H.get
      H.liftEffect $ Console.log ("model.username: " <> model.username)


    SubmitButtonClicked event -> do
      H.liftEffect $ Event.preventDefault event
      username <- H.gets _.username
      H.modify_ (\model -> model { result = Loading })
      -- This is just amazing. We don't need to split this into two steps, yet it is async anyway.
      httpResponse <- H.liftAff $ Affjax.get AffjaxWeb.driver AffjaxResponseFormat.string ("https://api.github.com/users/" <> username)
      H.modify_ (\model -> model { result = (httpResponse # map _.body # eitherToEventuallyPossibly)  })

-------------VIEWS-----------------
view :: forall m . Model -> H.ComponentHTML Msg () m 
view model =
  H.form
    [ HE.onSubmit \ev -> SubmitButtonClicked ev ]
    [ H.h1 [] [ H.text "Look up GitHub user" ]
    , H.label []
        [ H.div [] [ H.text "Enter username:" ]
        , H.input
            [ HP.value model.username
            , HE.onValueInput UsernameChanged
            ]
        ]
    , H.button
        [ HP.type_ HP.ButtonSubmit ]
        [ H.text "Fetch info"]
    , case model.result of
        NotLoaded ->
          H.text ""

        Loading ->
          H.h1 [] [ H.text "Loading..." ] 

        Error err ->
          H.h1 [] [ H.text "error" ]

        Ok user ->
          H.h1 [] [ H.text user ]
    ]

-------------Component-----------------
component :: forall m query output . MonadAff m =>  H.Component query Input output m
component =
  H.mkComponent
    { initialState: initModel
    , render: view
    , eval: H.mkEval H.defaultEval { handleAction = update }
    }
