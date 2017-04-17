{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# language AllowAmbiguousTypes        #-}

module Web.Formaldehyde
  ( -- * Constructing a form
    field
    -- * Running a form
  , runForm
  , pick
  , mkFieldError
    -- * Types
  , FormParser
  , FormResult
  , FieldError )
where

import Control.Applicative
import Control.Monad.Except
import Data.Aeson
import Data.Bifunctor (first)
import Data.Default.Class
import Data.Either (isRight)
import Data.Kind
import Data.Map.Strict (Map)
import Data.Proxy
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)
import GHC.TypeLits
import qualified Control.Monad.Fail as Fail
import qualified Data.Aeson.Types   as A
import qualified Data.Map.Strict    as M
import qualified Data.Text          as T

----------------------------------------------------------------------------
-- Types

newtype FormParser (names :: [Symbol]) m a
  = FormParser (Value -> m (Either String a, FieldError names))

instance Functor m => Functor (FormParser names m) where
  fmap f (FormParser x) = FormParser $
    fmap (first (fmap f)) . x

instance Applicative m => Applicative (FormParser names m) where
  pure x = (FormParser . const . pure) (Right x, mempty)
  (FormParser f) <*> (FormParser x) = FormParser $ \v ->
    let g (ef, e0) (ex, e1) = (ef <*> ex, e0 <> e1)
    in pure g <*> f v <*> x v

instance Applicative m => Alternative (FormParser names m) where
  empty = (FormParser . const . pure) (Left "empty", mempty)
  (FormParser x) <|> (FormParser y) = FormParser $ \v ->
    let g (ex, e0) ~(ey, e1) =
          if isRight ex
            then (ex, e0)
            else (ey, e1)
    in pure g <*> x v <*> y v

instance Monad m => Monad (FormParser names m) where
  return = pure
  FormParser m >>= f = FormParser $ \v -> do
    (ex, e0) <- m v
    case ex of
      Left err -> return (Left err, e0)
      Right x -> do
        let (FormParser n) = f x
        (ey, e1) <- n v
        return (ey, e0 <> e1)
  fail = Fail.fail

instance Monad m => Fail.MonadFail (FormParser names m) where
  fail msg = (FormParser . const . return) (Left msg, mempty)

instance Monad m => MonadPlus (FormParser names m) where
  mzero = empty
  mplus = (<|>)

instance Monad m => MonadTrans (FormParser names) where
  lift m = FormParser . const $
    let f x = (Right x, mempty)
    in fmap f m

data FieldError (names :: [Symbol]) =
  FieldError (Map Text Value)

instance Semigroup (FieldError names) where
  (FieldError x) <> (FieldError y) = FieldError (M.union x y)

instance Monoid (FieldError names) where
  mempty  = FieldError M.empty
  mappend = (<>)

instance ToJSON (FieldError names) where
  toJSON (FieldError m) = (object . fmap f . M.toAscList) m
    where
      f (name, err) = name .= err

mkFieldError :: ToJSON e
  => SelectedName names -- ^ The field name
  -> e                 -- ^ Data that represents error
  -> FieldError names
mkFieldError name x =
  FieldError (M.singleton (unSelectedName name) (toJSON x))

data FormResult (names :: [Symbol]) a
  = FormResultErrors (FieldError names)
  | FormResultSuccess a (Maybe Text)

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

data Response (names :: [Symbol]) = Response
  { responseParseError :: Maybe String
  , responseRedirectTo :: Maybe Text
  , responseFieldError :: FieldError names
  , responseResult     :: Value }

instance Default (Response names) where
  def = Response
    { responseParseError = Nothing
    , responseRedirectTo = Nothing
    , responseFieldError = mempty
    , responseResult     = Null }

instance ToJSON (Response names) where
  toJSON Response {..} = object
    [ "parse_error"  .= responseParseError
    , "redirect_to"  .= responseRedirectTo
    , "field_errors" .= responseFieldError
    , "result"       .= responseResult ]

----------------------------------------------------------------------------
-- Constructing a form

-- | Make a field that parses a 'Text' value.

field :: forall (name :: Symbol) (names :: [Symbol]) m e s a.
  ( KnownSymbol name
  , InSet name names
  , Monad m
  , ToJSON e
  , FromJSON s )
  => (s -> ExceptT e m a)
  -> FormParser names m a
field check = FormParser $ \v -> do
  let name = pick @name @names
      f :: Value -> A.Parser s
      f = withObject "form field" (.: unSelectedName name)
      r = A.parseEither f v
  case r of
    Left parseError -> return (Left parseError, mempty)
    Right r' -> do
      e <- runExceptT (check r')
      return $ case e of
        Left verr ->
          (Right undefined, mkFieldError name (toJSON verr))
        Right x ->
          (Right x, mempty)

----------------------------------------------------------------------------
-- Running a form

runForm :: (Monad m, ToJSON b)
  => FormParser names m a
  -> Value
  -> (a -> m (FormResult names b))
  -> m Value
runForm (FormParser p) v f = do
  (ex, es) <- p v
  case ex of
    Left parseError -> return . toJSON $
      def { responseParseError = pure parseError }
    Right result -> do
      r <- f result
      return . toJSON $ case r of
        FormResultErrors es' ->
          def { responseFieldError = es <> es' }
        FormResultSuccess x murl ->
          def { responseRedirectTo = murl
              , responseResult     = toJSON x }
