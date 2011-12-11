
module Message
  ( E
  , Message(..)
  , Level(..)
  , report
  , info
  , warn
  , err
  , runE
  , (>>?)
  , (?>>?)
  ) where

import Control.Monad.Writer

type E = Writer [Message]

data Message = Message { level :: Level, text :: String }

instance Show Message where
  showsPrec p msg =
    (showsPrec p $ level msg)
    . ("  " ++)
    . (text msg ++)

data Level = Info | Warning | Error deriving (Bounded, Enum, Eq, Ord, Read)

levelIndicators =
  [ (Info,    " I")
  , (Warning, " W")
  , (Error,   "*E")
  ]

instance Show Level where
  showsPrec _ level = (maybe "??" id (lookup level levelIndicators) ++)

report :: Message -> E ()
report = tell . (: [])

info, warn, err :: String -> E ()

info = report . Message Info

warn = report . Message Warning

err = report . Message Error

filterMessages minLevel = filter $ (>= minLevel) . level

runE :: (MonadIO m) => Level -> E a -> m a
runE minLevel e =
  let (x, w) = runWriter $ censor (filterMessages minLevel) e in
  liftIO (mapM_ print w) >> return x

infixl 1 >>?, ?>>?

check :: [Message] -> (a -> E b) -> a -> E (Maybe b)
check w f a =
  if   null $ filterMessages Error w
  then f a >>= return . Just
  else return Nothing

(>>?) :: E a -> (a -> E b) -> E (Maybe b)
(>>?) m f = do
  (a, w) <- listen m
  check w f a

(?>>?) :: E (Maybe a) -> (a -> E b) -> E (Maybe b)
(?>>?) m f = do
  (a, w) <- listen m
  maybe (return Nothing) (check w f) a

