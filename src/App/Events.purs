module App.Events where

import App.Routes (Route)
import App.State (State(..))
import Data.Function (($))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)
import Data.Ring ((+), (-))
import Control.Applicative ((*>), pure)
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Maybe
import Control.Monad.Eff (Eff, kind Effect)
import DOM.HTML.Types (HISTORY)
import DOM (DOM)

type AppEffects fx = ( history :: HISTORY
                     , dom :: DOM
                     , console :: CONSOLE
                     , ajax :: AJAX
                     | fx)

data Event = PageView Route | Increment | Decrement

type ConsoleEffects fx = (console :: CONSOLE | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) =
  noEffects $ State st { route = route, loaded = true }
foldp Increment (State st) = consolePrint $ State st { count = st.count + 1 }
foldp Decrement (State st) = consolePrint $ State st { count = st.count - 1 }


consolePrint :: ∀ fx. State -> EffModel State Event (AppEffects fx)
consolePrint state = { state: state
                     , effects: [ log "increment" *> pure Nothing ] }
