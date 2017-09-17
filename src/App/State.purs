module App.State where

import App.Config (config)
import App.Routes (Route, match)
import Data.Newtype (class Newtype)

type Projects = Array Project

newtype Project = Project
  { owner :: String
  , name :: String }

newtype State = State
  { title :: String
  , route :: Route
  , count :: Int
  , loaded :: Boolean
  , projects :: Projects
  }

derive instance newtypeState :: Newtype State _

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , projects: []
  , loaded: false
  , count: 0
  }
