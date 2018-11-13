import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Data.Map                  as Map


type Config = Map String (String, String)

-- incredibly useful in mainitaining purity of functions when there is an IO CONFIG(reading from files,databases) involved.
-- since config is an IO it cannot be direcly used inside the helper functions and only sensible option is the pass it as argument.
-- and since same config is used across these functions we can use Reader Monad

-- Reading a Map config using Reader Monad within the context of success and failure.
-- use first name to display last name and email from the config as a Maybe type. Nothing if user is not present.
-- multiple functions using the same config to run operation i.e getLastName and getEmail

getLastName :: String -> ReaderT Config Maybe String
getLastName fname = ReaderT $ \r -> fmap fst (Map.lookup fname r)

getEmail :: String -> ReaderT Config Maybe String
getEmail fname = ReaderT $ \r -> fmap snd (Map.lookup fname r)

-- note we do not need to pass configs explicitly to the two helper functions. This could many layers deep.
-- run the readerT to execute the helper functions within this environment

fetchDetails :: String -> Config -> Maybe String
fetchDetails user conf =
  let r = runReaderT $ do
        lastName <- getLastName user
        email <- getEmail user
        return (lastName ++ " " ++ email)
  in r conf

-- IO test config. Randomly picked few combinations.
-- this could be coming from a file or from a database.

testConfig :: IO (Map String (String,String))
testConfig =
  return $
  Map.fromList
    [ ("darell", ("phillip", "dphillip@gmail.com"))
    , ("james", ("smith", "jsmith@yahoo.com"))
    , ("foo", ("baz", "fbaz@mail.com"))
    , ("bob", ([], []))
    ]

main :: IO()
main = do
  putStrLn $ "Enter First Name"
  fname <- getLine
  testConf <- testConfig
  putStrLn $ show $ fetchDetails fname testConf
