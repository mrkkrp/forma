{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TypeApplications           #-}

module Web.Formaldehyde
  ( FormParser
  , FormResult )
where

import Control.Monad.Except
import Control.Monad.Writer
import Data.Aeson
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Scientific
import Data.Text (Text)
import GHC.TypeLits
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

newtype FormParser m a = FormParser (WriterT FieldError m a)
  deriving (Functor, Applicative, Monad)

-- TODO define alternative so optional, many, some, etc work nicely

data FieldError = FieldError (Map Text Value)

instance Monoid FieldError -- TODO and also Semigroup

mkFieldError :: forall name e. ToJSON e => e -> FieldError
mkFieldError = undefined

text :: forall (name :: Symbol) m e a. (Monad m, ToJSON e)
  => (Text -> ExceptT e m a)
  -> FormParser m a
text = undefined

scientific :: forall name m e a. (Monad m, ToJSON e)
  => (Scientific -> ExceptT e m a)
  -> FormParser m a
scientific = undefined

bool :: forall name m e a. (Monad m, ToJSON e)
  => (Bool -> ExceptT e m a)
bool = undefined

data FormResult
  = FormResultErrors (NonEmpty FieldError)
  | FormResultRedirect ByteString
  | FormResultSuccess

runForm :: Monad m => FormParser m a -> (a -> m FormResult) -> m Value
runForm = undefined

rere :: Monad m => FormParser m Text
rere = text @"my_field" f
  where
    f :: Monad m => Text -> ExceptT Text m Text
    f = return
