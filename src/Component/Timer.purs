module Component.Timer where

import Prelude

import Data.Maybe ( Maybe(..) )

import Halogen as H
import Halogen.HTML as H
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS

import Effect.Ref as Ref

import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Effect.Aff as Aff
import Effect.Aff ( Aff, Fiber )
import Effect ( Effect )

import Control.Monad.Rec.Class ( forever ) as Control

import Data.Time.Duration (Milliseconds(..))


-------------Input-----------------
type Input = Unit

-------------Model-----------------
type Model =
  { count :: Int
  }


initModel :: Input -> Model
initModel _ =
  { count: 0
  }

-------------Msg-----------------
data Msg =
    TickHappened
  | Initialize

-------------Update-----------------
update :: forall m output . MonadAff m => Msg -> H.HalogenM Model Msg () output m Unit
update =
  case _ of
    TickHappened ->
      H.modify_ (\model -> model { count = model.count + 1 })

    Initialize -> do
      tickEmitter :: HS.Emitter Msg <- timer TickHappened
      _ <- H.subscribe tickEmitter -- returns a subscription id
      pure unit

timer :: forall (m :: Type -> Type) msg . MonadAff m => msg -> m (HS.Emitter msg)
timer msg =
  do
    { emitter, listener } <- H.liftEffect HS.create
    let notifier :: Aff Unit
        notifier =
          Control.forever do
            Aff.delay (Milliseconds 1000.0)
            H.liftEffect $ HS.notify listener msg

        notifierFiber :: Aff (Fiber Unit)
        notifierFiber =
          Aff.forkAff notifier
    _ <- H.liftAff notifierFiber
    pure emitter

-------------VIEWS-----------------
view :: forall m . Model -> H.ComponentHTML Msg () m 
view model =
  H.div []
    [ H.h1 [] [ H.text "timer" ]
    , H.div [] [ H.text $ show model.count <> " seconds" ]
    ]

-------------Component-----------------
component :: forall m query output . MonadAff m => H.Component query Input output m
component =
  H.mkComponent
    { initialState: initModel
    , render: view
    , eval: H.mkEval H.defaultEval { handleAction = update, initialize = Just Initialize }
    }
