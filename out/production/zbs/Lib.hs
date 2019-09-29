module Lib
  ( run
  ) where

import qualified Data.Aeson   as Aeson
import qualified Debug.Trace  as Debug
import           GHC.Generics (Generic)
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

helloHandler :: Server.Request -> Server.Response
helloHandler _ = Server.stringResponse "hello"

jsonHandler :: Server.Request -> Server.Response
jsonHandler req = Server.stringResponse result
  where
    body = Server.requestBody req
    result =
      case Server.decodeJson body of
        Left err -> "Failed to decode request body as a Person. It must be something else"
        Right p -> "Yay! We have a person named: " <> name p

run :: IO ()
run =
  Server.startServer
    [ Server.simpleHandler Server.GET "/hello" helloHandler
    , Server.simpleHandler Server.GET "/sum" (\r -> Server.stringResponse (show $ sum' 3 2))
    , Server.simpleHandler Server.GET "/sum-items" (\r -> Server.stringResponse (show $ sumItems [1 .. 10]))
    , Server.simpleHandler Server.POST "/person" jsonHandler
    , Server.handlersWithState
        0
        [ Server.statefulHandler Server.GET "/count-up" (\s r -> (s + 1, Server.stringResponse $ show s + 1))
        , Server.statefulHandler Server.GET "/count-down" (\s r -> (s - 1, Server.stringResponse $ show s - 1))
        ]
    ]
