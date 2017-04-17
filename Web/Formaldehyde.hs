{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Web.Formaldehyde
  ( -- * Constructing a form
    text
  , scientific
  -- TODO something for integral types
  -- TODO something for real numbers
  , bool
  -- TODO something for enumeration, maybe different approaches?
  , optional
    -- * Running a form
  , runForm
    -- * Some types
  , FormParser
  , FormResult )
where

import Control.Monad.Except
import Control.Monad.Writer (WriterT (..))
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Functor.Alt
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Proxy
import Data.Scientific (Scientific)
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)
import GHC.TypeLits
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import qualified Data.Scientific    as C
import qualified Data.Semigroup     as S
import qualified Data.Text          as T

----------------------------------------------------------------------------
-- Types

newtype FormParser m a = FormParser (WriterT FieldError m a)
  deriving (Functor, Applicative, Monad)

instance Monad m => Alt (FormParser m) where
  (FormParser x) <!> (FormParser y) = FormParser . WriterT $ do
    (x0, e0) <- runWriterT x
    (y1, e1) <- runWriterT y
    let nullError (FieldError m) = M.null m
    return $
      if nullError e0
        then (x0, e0)
        else (y1, e1)

data FieldError = FieldError (Map Text Value)

instance Semigroup FieldError where
  (FieldError x) <> (FieldError y) = FieldError (M.union x y)

instance Monoid FieldError where
  mempty  = FieldError M.empty
  mappend = (<>)

data FormResult
  = FormResultErrors (NonEmpty FieldError)
  | FormResultRedirect ByteString
  | FormResultSuccess

----------------------------------------------------------------------------
-- Constructing a form

mkFieldError :: forall (name :: Symbol) e. (KnownSymbol name, ToJSON e)
  => e                 -- ^ Data that represents error
  -> FieldError
mkFieldError x = FieldError (M.singleton name (toJSON x))
  where
    name = T.pack (symbolVal (Proxy :: Proxy name))

text :: forall (name :: Symbol) m e a. (KnownSymbol name, Monad m, ToJSON e)
  => (Text -> ExceptT e m a)
  -> FormParser m a
text = undefined

scientific :: forall (name :: Symbol) m e a. (KnownSymbol name, Monad m, ToJSON e)
  => (Scientific -> ExceptT e m a)
  -> FormParser m a
scientific = undefined

bool :: forall (name :: Symbol) m e a. (KnownSymbol name, Monad m, ToJSON e)
  => (Bool -> ExceptT e m a)
bool = undefined

-- | One or none.

optional :: (Alt f, Applicative f) => f a -> f (Maybe a)
optional x = Just <$> x <!> pure Nothing

----------------------------------------------------------------------------
-- Running a form

runForm :: Monad m => FormParser m a -> (a -> m FormResult) -> m Value
runForm = undefined

rere :: Monad m => FormParser m Text
rere = text @"my_field" f
  where
    f :: Monad m => Text -> ExceptT Text m Text
    f = return
