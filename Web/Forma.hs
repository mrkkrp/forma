{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
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

module Web.Forma
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
import Data.Default.Class
import Data.Kind
import Data.Map.Strict (Map)
import Data.Proxy
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)
import GHC.TypeLits
import qualified Data.Aeson.Types    as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as M
import qualified Data.Text           as T

----------------------------------------------------------------------------
-- Types

data BranchState names a
  = BranchParsingFailed String
  | BranchValidationFailed (FieldError names)
  | BranchSucceeded a
  deriving Functor

instance Applicative (BranchState names) where
  pure = BranchSucceeded
  (BranchParsingFailed    msg) <*> _ =
    BranchParsingFailed msg
  (BranchValidationFailed _) <*> (BranchParsingFailed msg) =
    BranchParsingFailed msg
  (BranchValidationFailed err0) <*> (BranchValidationFailed err1) =
    BranchValidationFailed (err0 <> err1)
  (BranchValidationFailed err) <*> BranchSucceeded _ =
    BranchValidationFailed err
  BranchSucceeded _ <*> (BranchParsingFailed msg) =
    BranchParsingFailed msg
  BranchSucceeded _ <*> (BranchValidationFailed err) =
    BranchValidationFailed err
  BranchSucceeded f <*> BranchSucceeded x =
    BranchSucceeded (f x)

newtype FormParser (names :: [Symbol]) m a
  = FormParser { runFormParser :: Value -> m (BranchState names a) }

instance Functor m => Functor (FormParser names m) where
  fmap f (FormParser x) = FormParser $
    fmap (fmap f) . x

instance Applicative m => Applicative (FormParser names m) where
  pure x = (FormParser . const . pure) (BranchSucceeded x)
  (FormParser f) <*> (FormParser x) = FormParser $ \v ->
    pure (<*>) <*> f v <*> x v

instance Applicative m => Alternative (FormParser names m) where
  empty = (FormParser . const . pure) (BranchParsingFailed "empty")
  (FormParser x) <|> (FormParser y) = FormParser $ \v ->
    let g x' y' =
          case x' of
            BranchParsingFailed    _ -> y'
            BranchValidationFailed _ -> x'
            BranchSucceeded        _ -> x'
    in pure g <*> x v <*> y v

data FieldError (names :: [Symbol]) =
  FieldError (Map Text Value)

instance Semigroup (FieldError names) where
  (FieldError x) <> (FieldError y) = FieldError (M.union x y)

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
  , responseFieldError :: Maybe (FieldError names)
  , responseResult     :: Value }

instance Default (Response names) where
  def = Response
    { responseParseError = Nothing
    , responseRedirectTo = Nothing
    , responseFieldError = Nothing
    , responseResult     = Null }

instance ToJSON (Response names) where
  toJSON Response {..} = object
    [ "parse_error"  .= responseParseError
    , "redirect_to"  .= responseRedirectTo
    , "field_errors" .= maybe (Object HM.empty) toJSON responseFieldError
    , "result"       .= responseResult ]

----------------------------------------------------------------------------
-- Constructing a form

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
    Left parseError -> pure (BranchParsingFailed parseError)
    Right r' -> do
      e <- runExceptT (check r')
      return $ case e of
        Left verr ->
          (BranchValidationFailed (mkFieldError name verr))
        Right x ->
          (BranchSucceeded x)

----------------------------------------------------------------------------
-- Running a form

runForm :: (Monad m, ToJSON b)
  => FormParser names m a
  -> Value
  -> (a -> m (FormResult names b))
  -> m Value
runForm (FormParser p) v f = do
  r <- p v
  case r of
    BranchParsingFailed parseError -> return . toJSON $
      def { responseParseError = pure parseError }
    BranchValidationFailed validationError -> return . toJSON $
      def { responseFieldError = pure validationError }
    BranchSucceeded x -> do
      r' <- f x
      return . toJSON $ case r' of
        FormResultErrors validationError ->
          def { responseFieldError = pure validationError }
        FormResultSuccess result murl ->
          def { responseRedirectTo = murl
              , responseResult     = toJSON result }
