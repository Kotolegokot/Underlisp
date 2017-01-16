module Fail (Fail (..)
                 , report
                 , reportUndef
                 , catchError
                 , throwError
                 , liftIO
                 , rethrow) where

import Control.Monad.Except

import Point

data Fail = Fail { lePoint :: Point
                 , leMsg   :: String }

report :: MonadError Fail m => Point -> String -> m a
report point msg = throwError $ Fail point msg

reportUndef :: MonadError Fail m => String -> m a
reportUndef = report Undefined

rethrow :: MonadError e m => (e -> e) -> m a -> m a
rethrow f m = catchError m (\e -> throwError $ f e)
