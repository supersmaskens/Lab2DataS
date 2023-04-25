import Control.Applicative hiding (empty)
import System.Environment
import System.IO
import Data.Maybe (fromJust)
import PrioQueueLab2


-- | Bids.

data Bid
  = Buy Person Price           -- Person offers to buy share
  | Sell Person Price          -- Person offers to sell share
  | NewBuy Person Price Price  -- Person changes buy bid
  | NewSell Person Price Price -- Person changes sell bid
  deriving Show

type Person = String
type Price = Integer

-- | Parses a bid. Incorrectly formatted bids are returned verbatim
-- (tagged with 'Left').

parseBid :: String -> Either String Bid
parseBid s = case words s of
  name : kind : prices ->
    case (kind, mapM readInteger prices) of
      ("K",  Just [price])              -> Right (Buy name price)
      ("S",  Just [price])              -> Right (Sell name price)
      ("NK", Just [oldPrice, newPrice]) -> Right (NewBuy name oldPrice newPrice)
      ("NS", Just [oldPrice, newPrice]) -> Right (NewSell name oldPrice newPrice)
      _ -> Left s
  _ -> Left s
  where
  readInteger :: String -> Maybe Integer
  readInteger s = case filter (null . snd) $ reads s of
    [(x, _)] -> Just x
    _        -> Nothing

-- | Parses a sequence of bids. Correctly formatted bids are returned
-- (in the order encountered), and an error message is printed for
-- each incorrectly formatted bid.

parseBids :: String -> IO [Bid]
parseBids s = concat <$> mapM (check . parseBid) (lines s)
  where
  check (Left bid)  = do
    hPutStrLn stderr $ "Malformed bid: " ++ bid
    return []
  check (Right bid) = return [bid]

-- | The main function of the program.

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> process stdin
    [f] -> process =<< openFile f ReadMode
    _   -> hPutStr stderr $ unlines
      [ "Usage: ./Lab2 [<file>]"
      , "If no file is given, then input is read from standard input."
      ]
  where
  process h = trade =<< parseBids =<< hGetContents h

-- | The core of the program. Takes a list of bids and executes them.

data BookedBid = BuyBid Person Price | SellBid Person Price


type BuyBid  = BookedBid
type SellBid = BookedBid


instance Eq BookedBid where
  BuyBid  _ x == BuyBid  _ y = x == y
  SellBid _ x == SellBid _ y = x == y
  BuyBid  _ x == SellBid _ y = x == y


instance Ord BookedBid where
  BuyBid  _ x <= BuyBid  _ y = x <= y
  SellBid _ x <= SellBid _ y = x >= y
  BuyBid  _ x <= SellBid _ y = x <= y
  SellBid _ x <= BuyBid  _ y = x <= y
 

type OrderBook = (SkewHeap BuyBid, SkewHeap SellBid)


trade :: [Bid] -> IO ()
trade = transactions (empty, empty) 


transactions :: OrderBook -> [Bid] -> IO ()
transactions ob []       = printBooks ob
transactions ob (b:bids) = handleBid ob b >>= (`transactions` bids)
 

handleBid :: OrderBook -> Bid -> IO OrderBook
handleBid ob@(buys, sells) b = do
  case b of
    (Buy n p)       -> buy ob (BuyBid n p) 

    (Sell n p)      -> sell ob (SellBid n p) 
    
    (NewBuy n _ p)  -> let newBuys = findAndDelete (\(BuyBid on _) -> on == n) buys 
                       in handleBid (newBuys, sells) (Buy n p)

    (NewSell n _ p) -> let newSells = findAndDelete (\(SellBid on _) -> on == n) sells
                       in handleBid (buys, newSells) (Sell n p)


-- Comparing bids and adding/removing from books.
buy :: OrderBook -> BuyBid -> IO OrderBook
buy (buys, sells) bid =                  -- Note that the if statement will be False when sells is Empty
  if compareToPeek (>=) bid sells then   -- so no need to worry about peeking an empty heap.
    printPurchase bid (fromJust $ peek sells) >> return (buys, remove sells)

  else return (add bid buys, sells)


sell :: OrderBook -> SellBid -> IO OrderBook
sell (buys, sells) bid =
  if compareToPeek (<=) bid buys then    -- Same as above.
    printPurchase (fromJust $ peek buys) bid >> return (remove buys, sells)

  else return (buys, add bid sells) 


-- Printing
printPurchase :: BuyBid -> SellBid -> IO()
printPurchase (BuyBid n1 p1) (SellBid n2 _) = 
  putStrLn $ n1 ++ " buys a share from " ++ n2 ++ " for " ++ show p1 ++ "kr"


printBooks :: OrderBook -> IO() 
printBooks (buys, sells) = do
  putStrLn "OrderBok:"
  putStr "Säljare: " 
  applyIO (\(SellBid n p) -> printer n p) sells

  putStrLn ""
  putStr "Köpare: "
  applyIO (\(BuyBid n p) -> printer n p) buys
    where 
      printer n p = putStr $ n ++ " " ++ show p ++ ", " 



--intercalate


{- transactions :: OrderBook -> [Bid] -> IO ()
transactions ob [] = printBooks ob

transactions ob (b:bids) = do
  x <- handlePurchase ob b
  y <- makePurchase ob
  transactions y bids

makePurchase :: OrderBook -> IO OrderBook
makePurchase ob@(buys, sells) = do
  if comparePeeks (>=) buys sells then do 
    let b = peek buys
    let s = peek sells

    purchase b s
    return (removeMinT buys, removeMin sells)

  else return ob 

handleBid :: OrderBook -> Bid -> IO OrderBook
handleBid ob@(buys, sells) b = do
  case b of
    (Buy n p)       -> return (addT (BuyBid n p) buys, sells)

    (Sell n p)      -> return (buys, add (SellBid n p) sells)

    (NewBuy n _ p)  -> return (findAndReplaceT (\(BuyBid on _) -> on == n) (BuyBid n p) buys, sells)

    (NewSell n _ p) -> return (buys, findAndReplace (\(SellBid on _) -> on == n) (SellBid n p) sells) -}







{- instance Show BookedBid where -- was used for testing/troubleshooting
  show (BuyBid n _)  = n
  show (SellBid n _) = n
 -}