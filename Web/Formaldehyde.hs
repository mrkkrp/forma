{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# language AllowAmbiguousTypes        #-}

module Web.Formaldehyde
  ( -- * Constructing a form
    field
  , optional
    -- * Running a form
  , runForm
  , pick
    -- * Some types
  , FormParser
  , FormResult )
where

import Control.Applicative hiding (optional)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer (WriterT (..))
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Functor.Alt
import Data.Kind
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe (isJust)
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

newtype FormParser (names :: [Symbol]) m a
  = FormParser (WriterT FieldError (ReaderT Value m) a)
  deriving (Functor, Applicative, Monad)

instance Monad m => Alt (FormParser names m) where
  (FormParser x) <!> (FormParser y) = FormParser . WriterT $ do
    (x0, e0) <- runWriterT x
    (y1, e1) <- runWriterT y
    let nullError (FieldError m _) = isJust m
    return $
      if nullError e0
        then (x0, e0)
        else (y1, e1)

data FieldError = FieldError (Maybe String) (Map Text Value)

instance Semigroup FieldError where
  (FieldError m0 x) <> (FieldError m1 y) = FieldError (m0 <|> m1) (M.union x y)

instance Monoid FieldError where
  mempty  = FieldError Nothing M.empty
  mappend = (<>)

data FormResult
  = FormResultErrors (NonEmpty FieldError)
  | FormResultRedirect ByteString
  | FormResultSuccess

type family InSet (n :: Symbol) (ns :: [Symbol]) :: Constraint where
  InSet n '[]    = TypeError
    ('Text "The name " ':<>: 'ShowType n ':<>: 'Text " is not in the given set." ':$$:
     'Text "Either it's a typo or you need to add it to the set first.")
  InSet n (n:ns) = ()
  InSet n (m:ns) = InSet n ns

newtype SelectedName (names :: [Symbol]) = SelectedName Text

pick :: forall (name :: Symbol) (names :: [Symbol]).
  ( KnownSymbol name
  , InSet name names )
  => SelectedName names
pick = (SelectedName . T.pack . symbolVal) (Proxy :: Proxy name)

unSelectedName :: SelectedName names -> Text
unSelectedName (SelectedName txt) = txt

----------------------------------------------------------------------------
-- Constructing a form

mkValidationError :: ToJSON e
  => Text              -- ^ The field name
  -> e                 -- ^ Data that represents error
  -> FieldError
mkValidationError name x = FieldError Nothing (M.singleton name (toJSON x))

mkParseError
  :: String            -- ^ Parse error as returned by Aeson
  -> FieldError
mkParseError err = FieldError (pure err) M.empty

-- | Make a field that parses a 'Text' value.

field :: forall (name :: Symbol) (names :: [Symbol]) m e s a.
  ( KnownSymbol name
  , InSet name names
  , Monad m
  , ToJSON e
  , FromJSON s )
  => (s -> ExceptT e m a)
  -> FormParser names m a
field check = FormParser $ do
  value <- ask
  undefined

-- | One or none.

optional :: (Alt f, Applicative f) => f a -> f (Maybe a)
optional x = Just <$> x <!> pure Nothing

----------------------------------------------------------------------------
-- Running a form

runForm :: Monad m => FormParser names m a -> (a -> m FormResult) -> m Value
runForm = undefined

-- {
--   "parse_error": "here goes aeson parse error",
--   "redirect_to": null,
--   "validation_errors":
--     [
--       {
--         "field": "my_field",
--         "message": "Here we go, it's all wrong."
--       }
--     ],
--   "result": 5
-- }
