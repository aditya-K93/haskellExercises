import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Data.Map                  as Map


type Config = Map String (String, String)

-- demonstrating usage of haskell Reader Monad.
-- Reading a Map config using Reader Monad within the context of success and failure
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

testConfig :: Map String (String,String)
testConfig =
  Map.fromList
    [ ("darell", ("phillip", "dphillip@gmail.com"))
    , ("james", ("smith", "jsmith@yahoo.com"))
    , ("foo", ("baz", "fbaz@mail.com"))
    ]

main :: IO()
main = do
  putStrLn $ "Enter First Name"
  fname <- getLine
  putStrLn $ show $ fetchDetails fname testConfig

