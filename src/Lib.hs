module Lib
  ( run
  ) where

import qualified Data.Aeson   as Aeson
import           Data.List    (sort)
import qualified Debug.Trace  as Debug
import           GHC.Generics (Generic)
import           Text.Regex   (mkRegex, subRegex)
import qualified Zero.Server  as Server

data Person =
  Person
    { name :: String
    , age  :: Int
    }
  deriving (Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

sum' :: Int -> Int -> Int
sum' a b = Debug.traceShowId a + Debug.traceShowId b

sumItems :: [Int] -> Int
sumItems items = Debug.trace ("the sum is " ++ show sum) sum
  -- trace :: String -> a -> a
  -- print the first argument (String) and return the second argument (a)
  where
    reducer item acc = acc + Debug.traceShowId item
    sum = foldl reducer 0 items

spellOut :: String -> String
spellOut "1" = "one"
spellOut "2" = "two"
spellOut "3" = "three"
spellOut _   = "something else"

augmentSentence :: String -> String
augmentSentence s = subRegex (mkRegex "^I'm positive") s "I think"

data SwitchState
  = On
  | Off

flipSwitch :: SwitchState -> SwitchState
flipSwitch On  = Off
flipSwitch Off = On

instance Show SwitchState where
  show On  = "On"
  show Off = "Off"

data Item =
  Item
    { model    :: String
    , quantity :: Int
    }
  deriving (Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

instance Ord Item where
  Item {model = _, quantity = q1} `compare` Item {model = _, quantity = q2} = q2 `compare` q1

helloHandler :: Server.Request -> Server.Response
helloHandler _ = Server.stringResponse "hello"

jsonHandler :: Server.Request -> Server.Response
jsonHandler req = Server.stringResponse result
  where
    body = Server.requestBody req
    result =
      case Server.decodeJson body of
        Left _err -> "Failed to decode request body as a Person. It must be something else"
        Right p -> "Yay! We have a person named: " <> name p

cartUpdate :: [Item] -> Server.Request -> [Item]
cartUpdate items req = items'
  where
    body = Server.requestBody req
    items' =
      case Server.decodeJson body of
        Left _err  -> items
        Right item -> combineItems item items

combineItems :: Item -> [Item] -> [Item]
combineItems item [] = [item]
combineItems item (h:rest)
  | model item == model h = item {quantity = quantity item + quantity h} : rest
  | otherwise = h : combineItems item rest

run :: IO ()
run =
  Server.startServer
    [ Server.simpleHandler Server.GET "/hello" helloHandler
    , Server.simpleHandler Server.GET "/sum" (\r -> Server.stringResponse (show $ sum' 3 2))
    , Server.simpleHandler Server.GET "/sum-items" (\r -> Server.stringResponse (show $ sumItems [1 .. 10]))
    , Server.simpleHandler Server.POST "/person" jsonHandler
    , Server.handlersWithState
        0
        [ Server.statefulHandler Server.GET "/count-up" (\s _r -> (s + 1, Server.stringResponse $ show $ s + 1))
        , Server.statefulHandler Server.GET "/count-down" (\s _r -> (s - 1, Server.stringResponse $ show $ s - 1))
        ]
    , Server.simpleHandler Server.POST "/echo" (Server.stringResponse . Server.requestBody)
    , Server.simpleHandler Server.POST "/case" (Server.stringResponse . spellOut . Server.requestBody)
    , Server.simpleHandler
        Server.POST
        "/string-manipulation"
        (Server.stringResponse . augmentSentence . Server.requestBody)
    , Server.handlersWithState
        Off
        [ Server.statefulHandler
            Server.POST
            "/onoff-switch"
            (\s _r -> (flipSwitch s, Server.stringResponse $ show $ flipSwitch s))
        ]
    , Server.handlersWithState
        0
        [ Server.statefulHandler Server.GET "/current-count" (\s _r -> (s, Server.stringResponse $ show s))
        , Server.statefulHandler Server.POST "/increase" (\s _r -> (s + 1, Server.stringResponse ""))
        ]
    , Server.handlersWithState
        ([] :: [Item])
        [ Server.statefulHandler Server.GET "/cart" (\s _r -> (s, Server.jsonResponse $ sort s))
        , Server.statefulHandler Server.POST "/cart" (\s r -> (cartUpdate s r, Server.stringResponse ""))
        ]
    ]
