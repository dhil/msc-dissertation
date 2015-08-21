-- Basic Coffee dispenser model
-- ghci> dispense 1
import Control.Monad

-- Sum type describing dispensable drinks
data Dispensable = Coffee | Tea
                  deriving Show

-- Models the coffee dispenser's interface.
-- Remarkably, it has infinitely many buttons.
type ItemCode  = Integer
type Inventory = [(ItemCode,Dispensable)]

-- A fixed inventory
inventory = [(1,Coffee),(2,Tea)]

-- An implementation of the Reader monad
data Reader e a = Reader (e -> a)

instance Functor (Reader e) where
  fmap f (Reader x) = Reader $ \e -> (f . x) e

instance Applicative (Reader e) where
  pure x          = Reader $ \e -> x
  (Reader f) <*> (Reader x) = Reader $ \e -> (f e) (x e)

instance Monad (Reader e) where
  return x = Reader $ \e -> x
  x >>= f  = Reader $ \e -> runReader (f (runReader x e)) e

runReader :: Reader e a -> e -> a
runReader (Reader f) e = f e

-- The monadic ask operation
ask :: Reader a a
ask = Reader $ \e -> e

-- The dispenser model
dispenser :: ItemCode -> Reader Inventory (Maybe Dispensable)
dispenser n = do env <- ask
                 let item = lookup n env
                 return item

-- Convenient driver function for dispenser.
dispense :: ItemCode -> IO ()
dispense n = putStrLn . show . (flip runReader inventory) $ dispenser n
