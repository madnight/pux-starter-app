module App.View.Homepage where

import App.Events (Event(..))
import App.State (State(..))
import Control.Bind (discard, (>>=))
import Data.Function (($), const)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onClick)
import Text.Smolder.HTML (a, div, h1, button, span)
import Text.Smolder.HTML.Attributes (href, className)
import Text.Smolder.Markup ((!), text, (#!))
import Data.Show (show)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Promise (Promise, toAff)

foreign import data Response :: Type
foreign import data NET :: Effect
foreign import pow :: Number -> Number -> Number

view :: State -> HTML Event
view (State st) =
  div do
    h1 $ text (show (pow 3.0 3.0))
    button #! onClick (const Increment) $ text "Increment"
    span $ text (show st.count)
    button #! onClick (const Decrement) $ text "Decrement"
    h1 $ text (show (pow 3.0 3.0))
    a ! className "guide" ! href "https://www.purescript-pux.org/" $ text "Guide"
    a ! className "github" ! href "https://github.com/alexmingoia/purescript-pux/" $ text "GitHub"
