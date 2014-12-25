module Bot (main) where

import Data.List
import Skull
import Message
import Text.Printf
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent (threadDelay)
import Network
import System.IO
import System.Exit

data IRC = IRC { socket :: Handle
               , server :: String
               , port :: Int
               , chan :: String
               , nick :: String
               }
type Net = SkullGame (ReaderT IRC IO)

io :: IO a -> Net a
io = liftIO

main :: IO ()
main = bracket
       (connect "irc.rezel.net" 6667 "#skull" "skullbot")
       disconnect
       loop
  where
    disconnect = hClose . socket
    loop h = runReaderT  (evalStateT run initSkullInfo) h

connect :: String -> Int -> String -> String -> IO IRC
connect s p c n = notify' $ do
  h <- connectTo s (PortNumber (fromIntegral p))
  hSetBuffering h NoBuffering
  return $ IRC h s p c n
  where
    notify' a = bracket_
                (printf "Connecting to %s ... " s >> hFlush stdout)
                (putStrLn "done.")
                a

run :: Net ()
run = do
  irc <- ask
  let nick' = nick irc
      h = socket irc
      chan' = chan irc
  write "NICK" nick'
  write "USER" (nick'++" 0 * :skull'n'roses bot")
  waitForPing h
  io $ threadDelay 1000000
  write "JOIN" chan'
  forever $ do
    listenMessage eval h

waitForPing :: Handle -> Net ()
waitForPing h = listenMessage (const $ waitForPing h) h

listenMessage :: (Message -> Net ()) -> Handle -> Net ()
listenMessage eval' h = do
  s <- init `fmap` io (hGetLine h)
  io (putStrLn s)
  if ping s then pong s else eval' (parse s)
  where
    parse s = Message header' author' (drop 1 $ content') mType'
      where
        (header', content') = span (/= ':') $ drop 1 s
        author' = takeWhile (/= '!') header'
        mType' | "JOIN " `isSuffixOf` header' = JOIN
               | "PRIVMSG" `isInfixOf` header' = PRIVMSG
               | "PART " `isSuffixOf` header' = PART
               | otherwise = Unknown

ping :: String -> Bool
ping x = "PING :" `isPrefixOf` x

pong :: String -> Net ()
pong x = write "PONG" (':' : drop 6 x)

write :: String -> String -> Net ()
write s t = do
  h <- asks socket
  _ <- io $ hPrintf h "%s %s\r\n" s t
  io $ printf "> %s %s\n" s t

welcome :: String -> Net ()
welcome s | s == "kinnian" || s == "coin" || s == "traklon" || s == "Fraulol" = privmsg (s ++ " ! <3")
          | otherwise = return ()

killBot :: Net ()
killBot = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)


privmsg :: String -> Net ()
privmsg msg = do
  chan' <- asks chan
  write "PRIVMSG" (chan' ++ " :" ++ msg)

notify :: String -> String -> Net ()
notify user msg = write "NOTICE" (user ++ " :" ++ msg)

eval :: Message -> Net ()
eval m | mType m == PRIVMSG = do
           command m
           gets logs >>= mapM_ say
           modify (\sk -> sk {logs = []})
       | otherwise = return ()

say :: Log -> Net ()
say (Public s) = privmsg s
say (Private p s) = notify p s
