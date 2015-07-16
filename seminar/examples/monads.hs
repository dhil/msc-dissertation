import System.Random
import Data.Char
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

-- | We can interpret monads as descriptions for computational effects
-- Phonebook example using "regular" monads.
askNameAndNumber :: IO (Maybe (String,String))
askNameAndNumber  = do
  putStr "Enter your name> "
  name    <- getLine
  putStr "Enter your number> "
  number  <- getLine
  if isValid number
    then return $ Just (name,number)
    else return $ Nothing

-- | Determines whether a given string consists only of integer literals.
isValid :: String -> Bool
isValid = all isDigit


--  | Phonebook example with Monad Transformers
askNameAndNumberT :: MaybeT IO (String,String)
askNameAndNumberT = do
  lift $ putStr "Enter your name> "
  name   <- lift getLine
  lift $ putStr "Enter your number> "
  number <- getValidNumber
  return (name,number)
  

getValidNumber :: MaybeT IO String
getValidNumber = do num <- lift getLine
                    guard (isValid num)  -- Returns Nothing when isValid returns false
                    return num
