import qualified Data.Map as M (empty, insertWith)

data Prob a = Prob [(a, Float)] deriving Show

instance Functor Prob  where 
    fmap f (Prob ((a, p) : xs)) = Prob $ (f a, p) : rest where Prob rest = fmap f (Prob xs)
    fmap f (Prob []) = Prob []

instance Monad Prob where
    Prob d >>= f = Prob [(b, q*p) | (a, p) <- d, (b, q) <- d' a] where d' a = d'' where Prob d'' = f a
    return a = Prob [(a, 1)]
    
data Coin = Head | Tails deriving (Show, Eq, Ord)
coin = Prob [(Head, 0.5), (Tails, 0.5)]
loadedCoin = Prob [(Head, 0.1), (Tails, 0.9)]


{-
o1 = coin >>= f1 coin >>= f1 loadedCoin >>= f1 loadedCoin
f1 d x = fmap (\y -> (x,y)) d
-}


hasTails = any (== Tails)
arr d = fmap (\x -> [x]) d
f2 d x = fmap (\y -> x ++ y) (arr d)
--o2 = (arr coin) >>= f2 coin >>= f2 loadedCoin >>= f2 loadedCoin
o2 = (arr loadedCoin) >>= (f2 loadedCoin)
Prob o3 = (fmap hasTails o2)
o4 = filter ((== True) . fst) o3
o5 = sum $ map snd o4


{-
Для него определить Functor, Monad

и с помощью этих инстансов, попробовать решить задачу:

У нас есть две монеты

Coin = Head | Tails
coin = Prob [(Head, 0.5), (Tails, 0.5)]

loadedCoin = Prob [(Head, 0.1), (Tails, 0.9)]

посчитатаь вероятность выпадения решки, когда мы бросаем четыре монеты: две обычных и две loaded
-}
