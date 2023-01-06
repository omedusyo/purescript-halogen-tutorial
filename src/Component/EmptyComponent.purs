module Component.EmptyComponent where

import Prelude

import Data.Maybe ( Maybe(..) )

import Halogen as H
import Halogen.HTML as H
import Halogen.HTML.Events as HE

import Effect.Class (class MonadEffect)

-------------Input-----------------
type Input = Unit

-------------Model-----------------
type Model =
  {
  }


initModel :: Input -> Model
initModel _ =
  {
  }

-------------Msg-----------------
data Msg =
  Nada

-------------Update-----------------
update :: forall m output . Msg -> H.HalogenM Model Msg () output m Unit
update =
  case _ of
    Nada ->
      H.modify_ (\model -> model)

-------------VIEWS-----------------
view :: forall m . Model -> H.ComponentHTML Msg () m 
view model =
  H.div []
    []

-------------Component-----------------
component :: forall m query output . H.Component query Input output m
component =
  H.mkComponent
    { initialState: initModel
    , render: view
    , eval: H.mkEval H.defaultEval { handleAction = update }
    }
