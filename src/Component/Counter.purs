module Component.Counter where

import Prelude

import Halogen as H
import Halogen.HTML as H
import Halogen.HTML.Events as HE

-------------Input-----------------
-- This is not Msg.
-- Our counter component is self-contained. We don't take inputs from a parent. Wait... Shouldn't then the type be Empty?
-- TODO
-- type Input = Void
-- no... Void doesn't work, because the initial model takes Input as a parameter.
--
-- So what exactly is this input? Is it the initial read-only configuration?
-- Or can I actually resend the input?
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
-- Note that this is internal message. Input is something else - input from the parent component!
-- Since this is internal, maybe a better name would be Event? Or InternalEvent?
data Msg =
    Increment
  | Decrement

-------------Update-----------------
-- update :: Msg -> Model -> Model
-- TODO:
-- * How is the communication with other components done in update?
-- * Can I spawn new components in the update?
-- TODO:
-- * `m`? This is the effect monad
-- * `output` This is for parent communication.
-- * () means no child components that we can talk to. Use `slots` as a convential type var for it.
update :: forall output m . Msg -> H.HalogenM Model Msg () output m Unit
update =
  case _ of
    Increment ->
      H.modify_ \model -> model { count = model.count + 1 }

    Decrement ->
      H.modify_ \model -> model { count = model.count - 1 }

-------------VIEWS-----------------

-- view :: forall w i . Model -> H.HTML w i
view :: forall m . Model -> H.ComponentHTML Msg () m
view model =
  H.div []
    [ H.text $ show model.count
    , H.button [ HE.onClick \_ -> Increment ] [ H.text "inc" ]
    , H.button [ HE.onClick \_ -> Decrement ] [ H.text "dec" ]
    ]

-------------Component-----------------

-- TODO: What is
-- * `query`? A way to communicate TO the parent
-- * `output` a way that the parent can communicate with the component
-- Interesting... there's no Msg, it is internal... This is the public interface of a component.
--
-- Note that there is nothing about subcomponents... internal state, or internal messages
-- Only types that inform about communication between the component and the parent and the Effect monad type
component :: forall query output m . H.Component query Input output m
component =
  H.mkComponent
    { initialState: initModel
    , render: view
    , eval: H.mkEval H.defaultEval { handleAction = update }
    }
