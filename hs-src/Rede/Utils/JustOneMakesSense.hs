module Rede.Utils.JustOneMakesSense(
    newJOMSAtZero
    ,doJOMS

    ,JustOneMakesSense
    ) where 


import           Control.Concurrent.MVar
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Catch


newtype JustOneMakesSense = JustOneMakesSense (MVar Int)


newJOMSAtZero :: IO JustOneMakesSense
newJOMSAtZero = fmap JustOneMakesSense (newMVar 0)


doJOMS ::  (MonadIO m, MonadMask m) => JustOneMakesSense -> m a -> m a -> m a 
doJOMS (JustOneMakesSense mvar) actionIfZero actionIfMore = do 
    bracket 
        (liftIO $ modifyMVar mvar (\ i -> return $ if i == 0 then (1,True) else (i,False) ) )
        (\is_zero -> if is_zero then liftIO $ modifyMVar_ mvar (\ _ -> return 0) else return () )    
        (\is_zero -> if is_zero then actionIfZero else actionIfMore )
