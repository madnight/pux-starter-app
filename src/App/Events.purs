module App.Events where

import App.Routes (Route)
import App.State (State(..), Projects)
import Data.Function (($))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)
import Control.Applicative ((*>), pure)
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Maybe
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Aff (Aff)
import DOM.HTML.Types (HISTORY)
import DOM (DOM)
import Control.Promise (Promise, toAff)
import Control.Bind (bind, (>>=))
import Control.Monad.Eff.Class (liftEff)

foreign import data NET :: Effect
foreign import trends :: forall e. Eff e (Promise Projects)

data Event = PageView Route | ReceiveTodos | SetTodos Projects

type AppEffects fx =
  ( history :: HISTORY
  , dom :: DOM
  , console :: CONSOLE
  , net :: NET
  , ajax :: AJAX | fx
  )

fetch :: ∀ eff. Aff (net :: NET | eff) Projects
fetch = liftEff trends >>= toAff

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) =
  noEffects $ State st { route = route, loaded = true }
foldp (SetTodos p) (State st) =
  noEffects $ State st { projects = p }
foldp ReceiveTodos state =
  fetchPrint $ state

consolePrint :: ∀ fx. State -> EffModel State Event (AppEffects fx)
consolePrint state =
  { state: state
  , effects: [ log "increment" *> pure Nothing ]
  }

fetchPrint :: ∀ fx. State -> EffModel State Event (AppEffects fx)
fetchPrint state =
  { state: state
  , effects: [ do
             result <- fetch
             pure $ Just $ SetTodos result ]
  }
