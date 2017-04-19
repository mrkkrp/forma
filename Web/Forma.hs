-- |
-- Module      :  Web.Forma
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a tool for validation of forms that are represented
-- in the JSON format. Sending forms in JSON format via an AJAX request
-- instead of traditional submitting of forms has a number of advantages:
--
--     * Smoother user experience: no need to reload the whole page.
--     * Form rendering is separated and lives only in GET handler, POST (or
--       whatever method you deem appropriate for your use case) handler
--       only handles validation and actual effects that form submission
--       should initiate.
--     * You get a chance to organize form input just like you want.
--
-- The task of validation of a form in the JSON format may seem simple, but
-- it's not trivial to get it right. The library allows you to:
--
--     * Define form parser using type-safe applicative notation with field
--       labels being stored on type label which excludes any possibility of
--       typos and will force all your field labels be always up to date.
--     * Parse JSON 'Value' according to the definition of form you created.
--     * Stop parsing immediately if given form is malformed and cannot be
--       processed.
--     * Validate forms using any number of /composable/ checkers that you
--       write for your specific problem domain. Once you have a vocabulary
--       of checkers, creation of new forms is just a matter of combining
--       them, and yes they do combine nicely.
--     * Collect validation errors from multipe branches of parsing (one
--       branch per form field) in parallel, so validation errors in one
--       branch do not prevent us from collecting validation errors from
--       other branches. This allows for a better user experience as the
--       user can see all validation errors at the same time.
--     * Use 'optional' and @('<|>')@ from "Control.Applicative" in your
--       form definitions instead of ugly ad-hoc stuff (yes
--       @digestive-functors@, I'm looking at you).
--     * When individual validation of fields is done, you get a chance to
--       perform some actions and either decide that they succeeded, or
--       indeed perform additional checks that may involve several form
--       fields at once and signal a validation error assigned to a specific
--       field(s). This constitute a “second level” of validation.
--
-- Even though the library covers all use-cases of interest, it's still
-- lightweight, literally consisting of 5 functions: 'field', 'runForm',
-- 'pick', 'unSelectedName', and 'mkFieldError'. Read on to understand how
-- to use them.
--
-- __This library requires at least GHC 8 to work.__

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# language AllowAmbiguousTypes  #-}

module Web.Forma
  ( -- * Constructing a form
    field
    -- * Running a form
  , runForm
  , pick
  , unSelectedName
  , mkFieldError
    -- * Types and type functions
  , FormParser
  , FormResult
  , SelectedName
  , InSet
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

-- | State of a parsing branch.

data BranchState (names :: [Symbol]) a
  = BranchParsingFailed String
    -- ^ Parsing of JSON failed, this is fatal, we shut down and report the
    -- parsing error.
  | BranchValidationFailed (FieldError names)
    -- ^ Validation of a field failed. This is also fatal but we still try
    -- to validate other branches (fields) to collect as many validation
    -- errors as possible.
  | BranchSucceeded a
    -- ^ Success, we've got a result to return.
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

-- | The type represents the parser that you can run on a 'Value' with help
-- of 'runForm'. The only way for user of the library to create a parser is
-- via the 'field' function and by combining existing parsers using the
-- applicative notation.
--
-- 'FormParser' is parametrized by three type variables:
--
--     * @names@ — collection of field names we can use in a form to be
--       parsed with this parser.
--     * @m@ — underlying monad, 'FormParser' is not a monad itself, so it's
--       not a monad transformer, but validation can make use of the @m@
--       monad.
--     * @a@ — result of parsing.
--
-- 'FormParser' is not a monad because it's not possible to write a 'Monad'
-- instance with the properties that we want (validation errors should not
-- lead to short-cutting behavior).

newtype FormParser (names :: [Symbol]) m a
  = FormParser (Value -> m (BranchState names a))

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

-- | This a type that you must return in callback you give to 'runForm'.
-- Quite simply, it allows you either report a error or finish successfully.

data FormResult (names :: [Symbol]) a
  = FormResultError (FieldError names)
  | FormResultSuccess a

-- | @'SelectedName' names@ represents a name ('Text' value) that is
-- guaranteed to be in the @names@, which is a set of strings on type level.
-- The purpose if this type is to avoid typos and to force users to update
-- field names everywhere when they decide to change them. The only way to
-- obtain value of type 'SelectedName' is via the 'pick' function, which
-- see.

newtype SelectedName (names :: [Symbol]) = SelectedName Text

-- | The type function computes a 'Constraint' that holds when its first
-- argument is in its second argument. Otherwise a friendly type error is
-- displayed.

type family InSet (n :: Symbol) (ns :: [Symbol]) :: Constraint where
  InSet n '[]    = TypeError
    ('Text "The name " ':<>: 'ShowType n ':<>: 'Text " is not in the given set." ':$$:
     'Text "Either it's a typo or you need to add it to the set first.")
  InSet n (n:ns) = ()
  InSet n (m:ns) = InSet n ns

-- | Pick a name from a given collection of names.
--
-- Typical usage:
--
-- > type Fields = '["foo", "bar", "baz"]
-- >
-- > myName :: SelectedName Fields
-- > myName = pick @"foo" @Fields
--
-- This requires the @DataKinds@ and @TypeApplications@ language extensions.

pick :: forall (name :: Symbol) (names :: [Symbol]).
  ( KnownSymbol name
  , InSet name names )
  => SelectedName names
pick = (SelectedName . T.pack . symbolVal) (Proxy :: Proxy name)

-- | Extract a 'Text' value from 'SelectedName'.

unSelectedName :: SelectedName names -> Text
unSelectedName (SelectedName txt) = txt

-- | Error info in JSON format associtad with a particular form field.
-- Parametrized by @names@, which is a collection of field names (on type
-- level) target field belongs to. This is an instance of 'Semigroup' and
-- that's how you cobine 'FieldError's, note that it's not a 'Monoid',
-- because we do not want meaningless 'FieldError's.

data FieldError (names :: [Symbol])
  = FieldError (Map Text Value)

instance Semigroup (FieldError names) where
  (FieldError x) <> (FieldError y) = FieldError (M.union x y)

instance ToJSON (FieldError names) where
  toJSON (FieldError m) = (object . fmap f . M.toAscList) m
    where
      f (name, err) = name .= err

-- | This is a smart constructor for the 'FieldError' type, and the only way
-- to obtain values of that type.
--
-- Typical usage:
--
-- > type Fields = '["foo", "bar", "baz"]
-- >
-- > myError :: FieldError Fields
-- > myError = mkFieldError (pick @"foo" @Fields) "That's all wrong."
--
-- This requires the @DataKinds@ and @TypeApplications@ language extensions.
--
-- See also: 'pick' (to create 'SelectedName').

mkFieldError :: ToJSON e
  => SelectedName names -- ^ The field name
  -> e                 -- ^ Data that represents error
  -> FieldError names
mkFieldError name x =
  FieldError (M.singleton (unSelectedName name) (toJSON x))

data Response (names :: [Symbol]) = Response
  { responseParseError :: Maybe String
  , responseFieldError :: Maybe (FieldError names)
  , responseResult     :: Value }

instance Default (Response names) where
  def = Response
    { responseParseError = Nothing
    , responseFieldError = Nothing
    , responseResult     = Null }

instance ToJSON (Response names) where
  toJSON Response {..} = object
    [ "parse_error"  .= responseParseError
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
        FormResultError validationError ->
          def { responseFieldError = pure validationError }
        FormResultSuccess result ->
          def { responseResult = toJSON result }
