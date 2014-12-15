import Network
import System.IO
import System.Exit
import Text.Printf
import Data.List
import Control.Exception
import Control.Monad (replicateM)
import Control.Monad.State
import Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as Map
import Data.Random.Extras (shuffle)
import Data.Random.RVar (runRVar)
import Data.Random.Source.DevRandom (DevRandom( DevURandom ))
import Data.Char (toLower)

server :: String
server = "irc.rezel.net"

port :: Int
port = 6667

chan :: String
chan = "#skull"

nick :: String
nick = "skullbot"

type Player = String
data Head = Skull | Flower

instance Show Head where
  show Skull = "C"
  show Flower = "F"

type Card = Either Head Head
type Hand = [Head]
type Stack = [Card]
data Phase = Preparation | StackConstruction | Bidding | Resolution
           deriving Eq

instance Show Phase where
  show Preparation = "Préparation"
  show StackConstruction = "Construction des piles"
  show Bidding = "Enchères"
  show Resolution = "Résolution"

data Status = Status { hand :: Hand, stack :: Stack, points :: Int }
data SkullBot = SkullBot { socket :: Handle
                     , phase :: Phase
                     , players :: [Player]
                     , statuses :: Map.Map Player Status
                     , activePlayer :: Maybe Player
                     , lastBid :: Maybe Player
                     , currentBid :: Int
                     }

initSkullBot :: Handle -> SkullBot
initSkullBot s = SkullBot s Preparation [] Map.empty Nothing Nothing 0

initStatus :: Status
initStatus = Status [Skull, Flower, Flower, Flower] [] 0

updateStatus :: Player -> (Status -> Status) -> Net ()
updateStatus p f = modify (\sk -> sk { statuses = Map.adjust f p $ statuses sk})

addPlayer :: Player -> Net ()
addPlayer p = do
  modify (\s -> s { players = players s ++ [p]
                  , statuses = Map.insert p (initStatus) (statuses s) })
  privmsg $ p ++ " a rejoint la partie !"
  where insert x xs = if x `elem` xs then xs else x:xs

removePlayer :: Player -> Net ()
removePlayer p = do
  modify (\s -> s { players = delete p (players s), statuses = Map.delete p (statuses s) })
  ap <- gets activePlayer
  when (Just p == ap) $ nextPlayer

command :: Message -> Net ()
command m | c == "!play" = play
          | c == "!join" = joinPlayer (author m)
          | any (c ==) ["!nbcards", "!nbcard","!nb"] = do
              nbcards <- countStacks
              privmsg $ "Il reste " ++show nbcards ++" cartes."
          | c == "!recap" = recapAll
          | c == "!quit" = quit >> privmsg "Partie annulée."
          | c == "!suiv" = do
              active <- gets activePlayer
              case active of
               Just p -> privmsg p
               Nothing -> return ()
          | "!bid" `isPrefixOf` c = bid m
          | c == "!pass" = pass m
          | "!pop" `isPrefixOf` c = pop m
          | "!push" `isPrefixOf` c = push m
          | "!rm " `isPrefixOf` c = removePlayer $ drop 4 c
          | c == "!phase" = do
              phase <- gets phase
              privmsg $ show phase
          | otherwise = return ()
  where c = content m

play :: Net ()
play = do
  phase <- gets phase
  players <- gets players
  when (phase == Preparation) $ do
    changePhase StackConstruction
    privmsg $ "Ordre: " ++pprint players++"."
    nextPlayer
    newTurn
  where pprint = concat . intersperse ", "

changePhase :: Phase -> Net ()
changePhase p = do
  modify (\s -> s {phase = p})
  privmsg $ "On passe en phase de "++(map toLower $ show p)++" !"
  when (p == Resolution) $ do
    ap <- gets activePlayer
    case ap of
     Just p -> popSelf p >> recapAll
     Nothing -> return ()

quit :: Net ()
quit = modify $ initSkullBot . socket

joinPlayer :: Player -> Net ()
joinPlayer p = do
  players <- gets players
  phase <- gets phase
  when (phase == Preparation && (not $ p `elem` players)) $ addPlayer p

countStacks :: Net Int
countStacks = gets $ Map.foldl' (flip $ (+) . length . stack) 0 . statuses


recapPlayer :: Player -> Net ()
recapPlayer p = do
  hs <- gets $ Map.lookup p . statuses
  case hs of
   Just s -> privmsg $
             p ++ ": " ++ show (length $ hand s) ++ " cartes, " ++
             show (points s) ++ " point, " ++
             showStack (stack s) ++ "."
   Nothing -> return ()

showStack :: Stack -> String
showStack [] = "[]"
showStack s = concat . map (either (const "*") show) $ s

recapAll :: Net ()
recapAll = do
  skull <- get
  mapM_ recapPlayer $ players skull
  case lastBid skull of
   Just p -> when (currentBid skull > 0) $
             if (phase skull == Resolution)
             then privmsg $ p ++ " doit encore retourner " ++ show (currentBid skull) ++ " fleurs."
             else privmsg $ p ++ " pense pouvoir retourner " ++ show (currentBid skull) ++ " fleurs."
   Nothing -> return ()

nextPlayer :: Net ()
nextPlayer = do
  sk <- get
  case players sk of
   [] -> modify (\s -> s { activePlayer = Nothing })
   (x:xs) -> do
     modify (\s -> s { activePlayer = Just x, players = xs ++ [x] })
     privmsg $ "C'est au tour de "++x++" !"
     when (Just x == lastBid sk && phase sk == Bidding) $ changePhase Resolution


bid :: Message -> Net ()
bid m = do
  skull <- get
  max_bid <- countStacks
  rdy <- allPlayed
  let bidTry = read $ drop 5 $ content m
  when ((phase skull == Bidding ||
         (phase skull == StackConstruction && rdy)) &&
        (Just $ author m) == activePlayer skull &&
        bidTry <= max_bid &&
        bidTry > currentBid skull) $ do
    modify (\s -> s { lastBid = Just $ author m
                    , currentBid = bidTry})
    if (bidTry == max_bid)
      then changePhase Resolution
      else (changePhase Bidding) >> nextPlayer
  where
    allPlayed :: Net Bool
    allPlayed = do
      st <- gets statuses
      return $ Map.foldl' (\acc s -> acc && (length $ stack s) >= 1) True st

pass :: Message -> Net ()
pass m = do
  skull <- get
  when ((Just $ author m) == activePlayer skull &&
        phase skull == Bidding) $ do
    nextPlayer
    when (activePlayer skull == lastBid skull) $ changePhase Resolution

popSelf :: Player -> Net ()
popSelf p = do
  go <- popOthers p p
  currentBid <- gets currentBid
  when (currentBid > 0 && go) $ popSelf p

pop :: Message -> Net ()
pop m = do
  sk <- get
  when ((Just $ author m) == activePlayer sk &&
        phase sk == Resolution) $ do
    let target = drop 5 $ content m
    popOthers (author m) target
    recapAll

popOthers :: Player -> Player -> Net Bool
popOthers p target = do
  h <- revealCard target
  case h of
   Nothing -> return False
   Just Flower -> do
     modify (\sk -> sk { currentBid = currentBid sk - 1 })
     skull <- get
     if (currentBid skull <= 0)
       then (updatePoints p) >> return False
       else return True
   Just Skull -> do
     privmsg $ p ++" a pioché un crâne !"
     removeCard p
     newTurn
     return False

updatePoints :: Player -> Net ()
updatePoints p = do
  st <- gets statuses
  case Map.lookup p st of
   Just s -> if points s > 0
             then quit >> (privmsg $ "Félicitations "++p++", tu as gagné !")
             else do
               updateStatus p (\s -> s {points = points s + 1})
               privmsg $ "Bien joué "++p++", tu marques un point !"
               newTurn

newTurn :: Net ()
newTurn = do
  collectAllStacks
  modify (\sk -> sk { phase = StackConstruction
                    , lastBid = Nothing
                    , currentBid = 0
                    })
  gets players >>= mapM_ notifHand

notifHand :: Player -> Net ()
notifHand p = do
  sk <- get
  case Map.lookup p (statuses sk) of
   Just s -> printHand p $ hand s
   Nothing -> return ()
  where
    printHand :: Player -> Hand -> Net ()
    printHand p h = notify p $ concat $ intersperse " " $ zipWith (\i c -> show i ++ ":" ++ show c) [0..] h

collectStack :: Status -> Status
collectStack s = s { hand = hand s ++ map (either id id) (stack s), stack = [] }

collectAllStacks :: Net ()
collectAllStacks = do
  sk <- get
  forM_ (players sk) $ \p -> do
    case Map.lookup p $ statuses sk of
     Nothing -> return ()
     Just s -> do
       let s' = collectStack s
       h <- listShuffle $ hand s'
       updateStatus p (\_ -> s' {hand = h})

removeCard :: Player -> Net ()
removeCard p = updateStatus p (\s -> s {hand = tail $ hand s})

popStack :: Stack -> (Maybe Head, Stack)
popStack s = aux [] s
  where
    aux acc [] = (Nothing, acc)
    aux acc (c@(Right _):cs) = aux (acc ++ [c]) cs
    aux acc ((Left h):cs) = (Just h, acc ++ [Right h] ++ cs)

push :: Message -> Net ()
push m = do
  let p = author m
      n = read $ drop 6 $ content m
  ap <- gets activePlayer
  phase <- gets phase
  when (Just p == ap && phase == StackConstruction) $ do
    playCard p n
    notifHand p

playCard :: Player -> Int -> Net ()
playCard p i = do
  st <- gets statuses
  case Map.lookup p st of
   Nothing -> return ()
   Just s -> when (i>=0 && i < (length $ hand s)) $ do
     updateStatus p (pour i)
     nextPlayer
  where
       pour i s = s { stack = (Left $ (hand s) !! i) : stack s
                    , hand = removeNth i $ hand s}
       removeNth _ [] = []
       removeNth 0 (_:l) = l
       removeNth n (x:xs) = x : removeNth (n-1) xs

revealCard :: Player -> Net (Maybe Head)
revealCard p = do
  st <- gets statuses
  case Map.lookup p st of
   Nothing -> return Nothing
   Just s ->
     let (h, s') = popStack $ stack s in
     case h of
      Nothing -> return Nothing
      Just _ -> do
        updateStatus p (\st -> st {stack = s'})
        return h

type Net = StateT SkullBot IO

io :: IO a -> Net a
io = liftIO

main :: IO ((), SkullBot)
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop = runStateT run

connect :: IO SkullBot
connect = notify $ do
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  return (initSkullBot h)
  where
    notify a = bracket_
               (printf "Connecting to %s ... " server >> hFlush stdout)
               (putStrLn "done.")
               a

run :: Net ()
run = do
  write "NICK" nick
  write "USER" (nick++" 0 * :skull'n'roses bot")
  h <- gets socket
  waitForPing h
  io $ threadDelay 1000000
  write "JOIN" chan
  forever $ do
    listen eval h

waitForPing :: Handle -> Net ()
waitForPing h = listen (const $ waitForPing h) h

listen :: (Message -> Net ()) -> Handle -> Net ()
listen eval h = do
  s <- init `fmap` io (hGetLine h)
  io (putStrLn s)
  if ping s then pong s else eval (parse s)
  where
    parse s = Message header author (drop 1 $ content) mType
      where
        (header, content) = span (/= ':') $ drop 1 s
        author = takeWhile (/= '!') header
        mType | "JOIN " `isSuffixOf` header = JOIN
              | "PRIVMSG" `isInfixOf` header = PRIVMSG
              | "PART " `isSuffixOf` header = PART
              | otherwise = Unknown

data Message = Message { header :: String,
                         author :: String,
                         content :: String,
                         mType :: MessageType }

data MessageType = JOIN |
                   PRIVMSG |
                   PART |
                   Unknown
                 deriving Eq

ping :: String -> Bool
ping x = "PING :" `isPrefixOf` x

pong :: String -> Net ()
pong x = write "PONG" (':' : drop 6 x)

write :: String -> String -> Net ()
write s t = do
  h <- gets socket
  io $ hPrintf h "%s %s\r\n" s t
  io $ printf "> %s %s\n" s t

welcome :: String -> Net ()
welcome s | s == "kinnian" || s == "coin" || s == "traklon" || s == "Fraulol" = privmsg (s ++ " ! <3")
          | otherwise = return ()

killBot :: Net ()
killBot = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)

help :: String -> Net ()
help user = mapM_ (notify user) $ lines helpMsg
  where
    helpMsg = "\
\    !help: Afficher cette aide.\n\
\    !play: Lancer une partie.\n\
\    !quit: Annule la partie courante.\n\
\    !join: Rejoindre une partie en cours de création.\n\
\    !rm nick: Retirer un joueur de la partie.\n\
\    !suiv: Afficher le nom du joueur courant.\n\
\    !nbcards, !nbcard!, !nb: Afficher le total des cartes dans toutes les piles.\n\
\    !recap: Afficher un récapitulatif de la partie en cours.\n\
\    !pass: Passer son tour (si les règles le permettent).\n\
\    !phase: Afficher la phase de jeu en cours.\n\
\    !push cardnb: Poser une carte face cachée devant soi.\n\
\                  La correspondance carte/numero est notifiée régulièrement.\n\
\    !pop nick: Retourner la première carte cachée d'un joueur.\n\
\    !bid nb: Faire une annonce.\n\
\ "

eval :: Message -> Net ()
eval m | mType m == PRIVMSG = command m
       | otherwise = return ()

privmsg :: String -> Net ()
privmsg msg = write "PRIVMSG" (chan ++ " :" ++ msg)

notify :: String -> String -> Net ()
notify user msg = write "NOTICE" (user ++ " :" ++ msg)

listShuffle :: [a] -> Net [a]
listShuffle l = io $ runRVar (shuffle l) DevURandom
