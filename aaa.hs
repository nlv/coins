import qualified Data.Map as M (empty, insertWith)

{-
 - решение тестовой задачи:
 - моделирование подпрасывание монет.
 - реализация "вероятностной" монады
 - нахождение вероятности
 -}

data Prob a = Prob { unProb :: [(a, Float)] } deriving Show

instance Functor Prob  where
    fmap f d = Prob [(f a, p) | (a, p) <- unProb d]

instance Monad Prob where
    d >>= f = Prob [(b, q*p) | (a, p) <- unProb d, (b, q) <- unProb (f a)]
    return a = Prob [(a, 1)]

data Coin = Head | Tails deriving (Show, Eq, Ord)
coin = Prob [(Head, 0.5), (Tails, 0.5)]
loadedCoin = Prob [(Head, 0.1), (Tails, 0.9)]

{- Не очень по оптимизации -}
list :: Prob a -> Prob [a]
list d = fmap (\a -> [a]) d

{- Не очень по оптимизации -}
join d a = fmap ((a ++)) (list d)

test :: Prob [Coin]
test = do
  one <- list coin
  two <- join coin one
  three <- join loadedCoin two
  join loadedCoin three

tailsProb = sum $ map snd $ filter ((== True). fst) $ unProb $ fmap hasTails test
  where hasTails = any (== Tails)
