{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Web (module Web.Messages) where

import Ef.Ma
import qualified Web.Methods as Web
import Web.Messages

instance Ma Web.Web Web where
    ma use Web.Web {..} (GetScreen sk) = ma use screen sk
    ma use Web.Web {..} (GetWindow wk) = ma use window wk
    ma use Web.Web {..} (GetDocument dk) = ma use document dk
    ma use Web.Web {..} (GetDrawer dk) = ma use drawer dk
    ma use Web.Web {..} (GetSignaled sk) = ma use signaled sk
