-- Coffee dispenser model using Monad Transformers
-- ghci> dispense1 1;
-- ghci> dispense2 2;
-- ghci> dispense3 3;
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans
import Control.Monad.Random

-- Sum type describing dispensable drinks
data Dispensable = Coffee | Tea
                  deriving Show

-- Models the coffee dispenser's interface.
-- Remarkably, it has infinitely many buttons.
type ItemCode  = Integer
type Inventory = [(ItemCode,Dispensable)]

-- A fixed inventory
inventory = [(1,Coffee),(2,Tea)]

-- The basic dispenser reproduced using Monad Transformers
dispenser1 :: ItemCode -> ReaderT Inventory Maybe Dispensable
dispenser1 n = do inv <- ask
                  item <- lift $ lookup n inv
                  return item

dispense1 :: ItemCode -> IO ()
dispense1 n = display . runReaderWithInv $ dispenser1 n

-- Extended coffee dispenser
dispenser2 :: ItemCode -> WriterT String (ReaderT Inventory Maybe) Dispensable
dispenser2 n = do inv <- lift ask
                  item <- lift . lift $ lookup n inv
                  tell $ "Dispensing " ++ show item ++ "..."
                  return item

dispense2 :: ItemCode -> IO ()
dispense2 n = display . runReaderWithInv . execWriterT $ dispenser2 n

-- The dodgy coffee dispenser
dispenser3 :: ItemCode -> RandT StdGen (WriterT String (ReaderT Inventory Maybe)) Dispensable
dispenser3 n = do val <- getRandomR (1,20)
                  inv <- lift . lift $ ask
                  item <- lift . lift . lift $ lookup' val n inv
                  lift . tell $ "Dispensing " ++ show item ++ "..."
                  return item
                    where
                      lookup' :: Int -> ItemCode -> Inventory -> Maybe Dispensable
                      lookup' v n inv = if v > 10 then lookup n inv else Nothing

dispense3 :: ItemCode -> IO ()
dispense3 n =
  do r <- newStdGen
     display . runReaderWithInv . execWriterT . (flip evalRandT r) $ dispenser3 n


-- Convenient auxiliary functions
display :: Show a => a -> IO ()
display = putStrLn . show

runReaderWithInv :: ReaderT Inventory m a -> m a
runReaderWithInv = flip runReaderT inventory
