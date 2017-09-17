module App.Events where

import App.Routes (Route)
import App.State (State(..), Projects)
import Data.Function (($))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)
import Data.Ring ((+), (-))
import Control.Applicative ((*>), pure)
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Maybe
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Aff (Aff)
import DOM.HTML.Types (HISTORY)
import DOM (DOM)
import Control.Promise (Promise, toAff)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Eff.Class (liftEff)

foreign import data NET :: Effect

type AppEffects fx = ( history :: HISTORY
                     , dom :: DOM
                     , console :: CONSOLE
                     , net :: NET
                     , ajax :: AJAX
                     | fx)


data Event = PageView Route | Increment | Decrement | ReceiveTodos Projects

type ConsoleEffects fx = (console :: CONSOLE | fx)

foreign import trends :: forall e. Eff e (Promise Projects)

fetch :: forall eff. Aff (net :: NET | eff) Projects
fetch = liftEff trends >>= toAff


foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) =
  noEffects $ State st { route = route, loaded = true }
foldp Increment (State st) = consolePrint $ State st { count = st.count + 1 }
foldp Decrement (State st) = fetchPrint $ State st { count = st.count - 1 }
foldp (ReceiveTodos p) (State st) = noEffects $ State st { projects = p }

consolePrint :: ∀ fx. State -> EffModel State Event (AppEffects fx)
consolePrint state = { state: state
                     , effects: [ log "increment" *> pure Nothing ] }

fetchPrint :: ∀ fx. State -> EffModel State Event (AppEffects fx)
fetchPrint state = { state: state
                   , effects: [ do
                            result <- fetch
                            pure $ Just $ ReceiveTodos result
                        ]
                   }
