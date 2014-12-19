module Message where

data Message = Message { header :: String,
                         author :: String,
                         content :: String,
                         mType :: MessageType }

data MessageType = JOIN |
                   PRIVMSG |
                   PART |
                   Unknown
                 deriving Eq
