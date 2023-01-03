module Component.RNG where

import Prelude

import Data.Maybe ( Maybe(..) )

import Halogen as H
import Halogen.HTML as H
import Halogen.HTML.Events as HE

import Effect.Class (class MonadEffect)
import Effect.Random as Random

-------------Input-----------------
type Input = Unit

-------------Model-----------------
type Model =
  { number :: Maybe Int
  }

initModel :: Input -> Model
initModel _ =
  { number: Nothing
  }

-------------Msg-----------------
data Msg =
  GenerateRandomNumberButtonWasClicked

-------------Update-----------------
update :: forall output m . MonadEffect m => Msg -> H.HalogenM Model Msg () output m Unit
update =
  case _ of
    GenerateRandomNumberButtonWasClicked -> do
      -- H.modify_ \model -> model { number = Just 512 }
        newX <- H.liftEffect $ Random.randomInt 0 10
        -- pure $ model { number = Just newX }
        H.modify_ \model -> model { number = Just newX }

-------------VIEWS-----------------
view :: forall (m :: Type -> Type) . Model -> H.ComponentHTML Msg () m
view model =
  H.div []
    [ case model.number of
        Just x ->
          H.text $ show x
        Nothing ->
          H.text "Generate new number..."
    , H.button [ HE.onClick \_ -> GenerateRandomNumberButtonWasClicked ] [ H.text "generate" ]
    ]

-------------Component-----------------
component :: forall query output m . MonadEffect m => H.Component query Input output m
component =
  H.mkComponent
  { initialState: initModel
  , render: view
  , eval: H.mkEval H.defaultEval { handleAction = update }
  }
