module Main where

import Prelude

import Data.Maybe ( Maybe(..) )

import Effect (Effect)
import Effect.Console (log)

import Halogen.HTML as H
import Halogen.HTML.Properties as HP

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Component.Counter as Counter
import Component.RNG as RNG
import Component.GithubLookup as GithubLookup
import Component.Timer as Timer


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  -- log "Hello, World!"
  _ <- runUI Counter.component unit body
  _ <- runUI Counter.component unit body
  _ <- runUI RNG.component unit body
  _ <- runUI GithubLookup.component unit body
  _ <- runUI Timer.component unit body
  pure unit

-------------VIEWS-----------------

-- `HTML w i` is the type used for html that's not tied to a particular component.
--  `w` ~ widget, describes which sub-components can be used. What is the kind of `w`?
--  `i` ~ input, is this like msg?

element :: forall (w :: Type) (i :: Type) . H.HTML w i
element =
  H.h1 [] [ H.text "Hello, World!" ]


-- element1 :: forall w i . H.HTML w i
element1 :: H.PlainHTML
element1 =
  H.div [ HP.id "root" ]
    [ H.input  [ HP.placeholder "Name" ]
    , H.button [ HP.classes [ H.ClassName "btn-primary" ], HP.type_ HP.ButtonSubmit ]
        [ H.text "Submit" ]
    ]

-- test0 :: forall w msg . H.HTML w msg
-- test0 =
--   H.div [ HP.placeholder "wat" ]
--     []

header :: forall w i . Int -> H.HTML w i
header visits =
  H.h1 []
    [ H.text $ "You've had " <> show visits <> " visitors" ]


maybeElem :: forall w i a . Maybe a -> (a -> H.HTML w i) -> H.HTML w i
maybeElem a f =
  case a of
    Just x -> f x

    Nothing -> H.text ""

whenElem :: forall w i. Boolean -> (Unit -> H.HTML w i) -> H.HTML w i
whenElem cond f =
  if cond then
    f unit
  else
    H.text ""
