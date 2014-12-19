module Skull ( SkullGame
             , SkullInfo(..)
             , Log(..)
             , initSkullInfo
             , command
             ) where

import Message
import Data.List

import Control.Monad.State

import qualified Data.Map.Strict as Map
import Data.Random.Extras (shuffle)
import Data.Random.RVar (runRVar)
import Data.Random.Source.DevRandom (DevRandom( DevURandom ))
import Data.Char (toLower)


type Player = String
data Head = Skull | Flower

instance Show Head where
  show Skull = "C"
  show Flower = "F"

data Log = Public String | Private String String

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
data SkullInfo = SkullInfo { phase :: Phase
                           , players :: [Player]
                           , statuses :: Map.Map Player Status
                           , activePlayer :: Maybe Player
                           , lastBid :: Maybe Player
                           , currentBid :: Int
                           , logs :: [Log]
                           }

type SkullGame m = StateT SkullInfo m

initSkullInfo :: SkullInfo
initSkullInfo = SkullInfo Preparation [] Map.empty Nothing Nothing 0 []

initStatus :: Status
initStatus = Status [Skull, Flower, Flower, Flower] [] 0

updateStatus :: (Monad m) => Player -> (Status -> Status) -> SkullGame m ()
updateStatus p f = modify (\sk -> sk { statuses = Map.adjust f p $ statuses sk})

addPlayer :: (Monad m) => Player -> SkullGame m ()
addPlayer p = do
  modify (\s -> s { players = players s ++ [p]
                  , statuses = Map.insert p (initStatus) (statuses s) })
  publicLog $ p ++ " a rejoint la partie !"

removePlayer :: (Monad m) => Player -> SkullGame m ()
removePlayer p = do
  modify (\s -> s { players = delete p (players s), statuses = Map.delete p (statuses s) })
  ap' <- gets activePlayer
  when (Just p == ap') $ nextPlayer

command :: (Monad m) => Message -> SkullGame m ()
command m | c == "!play" = play
          | c == "!join" = joinPlayer (author m)
          | any (c ==) ["!nbcards", "!nbcard","!nb"] = do
              nbcards <- countStacks
              publicLog $ "Il reste " ++show nbcards ++" cartes."
          | c == "!recap" = recapAll
          | c == "!quit" = quit >> publicLog "Partie annulée."
          | c == "!suiv" = do
              active <- gets activePlayer
              case active of
               Just p -> publicLog p
               Nothing -> return ()
          | "!bid" `isPrefixOf` c = bid m
          | c == "!pass" = pass m
          | "!pop" `isPrefixOf` c = pop m
          | "!push" `isPrefixOf` c = push m
          | "!rm " `isPrefixOf` c = removePlayer $ drop 4 c
          | c == "!phase" = gets phase >>= (publicLog . show)
          | otherwise = return ()
  where c = content m

play :: (Monad m) => SkullGame m ()
play = do
  phase' <- gets phase
  players' <- gets players
  when (phase' == Preparation) $ do
    changePhase StackConstruction
    publicLog $ "Ordre: " ++pprint players'++"."
    nextPlayer
    newTurn
  where pprint = concat . intersperse ", "

changePhase :: (Monad m) => Phase -> SkullGame m ()
changePhase p = do
  modify (\s -> s {phase = p})
  publicLog $ "On passe en phase de "++(map toLower $ show p)++" !"
  when (p == Resolution) $ do
    ap' <- gets activePlayer
    case ap' of
     Just p' -> popSelf p' >> recapAll
     Nothing -> return ()

quit :: (Monad m) => SkullGame m ()
quit = modify $ const initSkullInfo

joinPlayer :: (Monad m) => Player -> SkullGame m ()
joinPlayer p = do
  players' <- gets players
  phase' <- gets phase
  when (phase' == Preparation && (not $ p `elem` players')) $ addPlayer p

countStacks :: (Monad m) => SkullGame m Int
countStacks = gets $ Map.foldl' (flip $ (+) . length . stack) 0 . statuses


recapPlayer :: (Monad m) => Player -> SkullGame m ()
recapPlayer p = do
  hs <- gets $ Map.lookup p . statuses
  case hs of
   Just s -> publicLog $
             p ++ ": " ++ show (length $ hand s) ++ " cartes, " ++
             show (points s) ++ " point, " ++
             showStack (stack s) ++ "."
   Nothing -> return ()

showStack :: Stack -> String
showStack [] = "[]"
showStack s = concat . map (either (const "*") show) $ s

recapAll :: (Monad m) => SkullGame m ()
recapAll = do
  skull <- get
  mapM_ recapPlayer $ players skull
  case lastBid skull of
   Just p -> when (currentBid skull > 0) $
             if (phase skull == Resolution)
             then publicLog $ p ++ " doit encore retourner " ++ show (currentBid skull) ++ " fleurs."
             else publicLog $ p ++ " pense pouvoir retourner " ++ show (currentBid skull) ++ " fleurs."
   Nothing -> return ()

nextPlayer :: (Monad m) => SkullGame m ()
nextPlayer = do
  sk <- get
  case players sk of
   [] -> modify (\s -> s { activePlayer = Nothing })
   (x:xs) -> do
     modify (\s -> s { activePlayer = Just x, players = xs ++ [x] })
     publicLog $ "C'est au tour de "++x++" !"
     when (Just x == lastBid sk && phase sk == Bidding) $ changePhase Resolution


bid :: (Monad m) => Message -> SkullGame m ()
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
    allPlayed = do
      st <- gets statuses
      return $ Map.foldl' (\acc s -> acc && (length $ stack s) >= 1) True st

pass :: (Monad m) => Message -> SkullGame m ()
pass m = do
  skull <- get
  when ((Just $ author m) == activePlayer skull &&
        phase skull == Bidding) $ do
    nextPlayer
    when (activePlayer skull == lastBid skull) $ changePhase Resolution

popSelf :: (Monad m) => Player -> SkullGame m ()
popSelf p = do
  go <- popOthers p p
  currentBid' <- gets currentBid
  when (currentBid' > 0 && go) $ popSelf p

pop :: (Monad m) => Message -> SkullGame m ()
pop m = do
  sk <- get
  when ((Just $ author m) == activePlayer sk &&
        phase sk == Resolution) $ do
    let target = drop 5 $ content m
    _ <- popOthers (author m) target
    recapAll

popOthers :: (Monad m) => Player -> Player -> SkullGame m Bool
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
     publicLog $ p ++" a pioché un crâne !"
     removeCard p
     newTurn
     return False

updatePoints :: (Monad m) => Player -> SkullGame m ()
updatePoints p' = do
  st <- gets statuses
  case Map.lookup p' st of
   Just s -> if points s > 0
             then quit >> (publicLog $ "Félicitations "++p'++", tu as gagné !")
             else do
               updateStatus p' (\s' -> s' {points = points s + 1})
               publicLog $ "Bien joué "++p'++", tu marques un point !"
               newTurn
   Nothing -> return ()

newTurn :: (Monad m) => SkullGame m ()
newTurn = do
  collectAllStacks
  modify (\sk -> sk { phase = StackConstruction
                    , lastBid = Nothing
                    , currentBid = 0
                    })
  gets players >>= mapM_ notifHand

notifHand :: (Monad m) => Player -> SkullGame m ()
notifHand p = do
  sk <- get
  case Map.lookup p (statuses sk) of
   Just s -> printHand p $ hand s
   Nothing -> return ()
  where
    printHand p' h =
      privateLog p' $ concat $ intersperse " " $
      zipWith (\i c -> show i ++ ":" ++ show c) [(0 :: Integer)..] h

collectStack :: Status -> Status
collectStack s = s { hand = hand s ++ map (either id id) (stack s), stack = [] }

collectAllStacks :: (Monad m) => SkullGame m ()
collectAllStacks = do
  sk <- get
  forM_ (players sk) $ \p -> do
    case Map.lookup p $ statuses sk of
     Nothing -> return ()
     Just s -> do
       let s' = collectStack s
       h <- return $  listShuffle $ hand s'
       updateStatus p (\_ -> s' {hand = h})

removeCard :: (Monad m) => Player -> SkullGame m ()
removeCard p = updateStatus p (\s -> s {hand = tail $ hand s})

popStack :: Stack -> (Maybe Head, Stack)
popStack s = aux [] s
  where
    aux acc [] = (Nothing, acc)
    aux acc (c@(Right _):cs) = aux (acc ++ [c]) cs
    aux acc ((Left h):cs) = (Just h, acc ++ [Right h] ++ cs)

push :: (Monad m) => Message -> SkullGame m ()
push m = do
  let p = author m
      n = read $ drop 6 $ content m
  ap' <- gets activePlayer
  phase' <- gets phase
  when (Just p == ap' && phase' == StackConstruction) $ do
    playCard p n
    notifHand p

playCard :: (Monad m) => Player -> Int -> SkullGame m ()
playCard p i = do
  st <- gets statuses
  case Map.lookup p st of
   Nothing -> return ()
   Just s -> when (i>=0 && i < (length $ hand s)) $ do
     updateStatus p (pour i)
     nextPlayer
  where
       pour i' s = s { stack = (Left $ (hand s) !! i') : stack s
                     , hand = removeNth i' $ hand s}
       removeNth _ [] = []
       removeNth 0 (_:l) = l
       removeNth n (x:xs) = x : removeNth (n-1) xs

revealCard :: (Monad m) => Player -> SkullGame m (Maybe Head)
revealCard p = do
  st <- gets statuses
  case Map.lookup p st of
   Nothing -> return Nothing
   Just s ->
     let (h, s') = popStack $ stack s in
     case h of
      Nothing -> return Nothing
      Just _ -> do
        updateStatus p (\st' -> st' {stack = s'})
        return h


publicLog :: (Monad m) => String -> SkullGame m ()
publicLog s = modify (\sk -> sk { logs = (logs sk) ++ [Public s] })

privateLog :: (Monad m) => String -> String -> SkullGame m ()
privateLog p s = modify (\sk -> sk { logs = (logs sk) ++ [Private p s] })



listShuffle :: [a] -> [a]
listShuffle = id
--listShuffle l = runRVar (shuffle l) DevURandom

help :: (Monad m) => String -> SkullGame m ()
help user = mapM_ (privateLog user) $ lines helpMsg
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
