-- |
-- Module      :  Web.Forma
-- Copyright   :  © 2017–2018 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
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
--       labels being stored on the type label which guards against typos
--       and will force all your field labels be always up to date.
--     * Parse JSON 'Value' according to the definition of form you created.
--     * Stop parsing immediately if given form is malformed and cannot be
--       processed.
--     * Validate forms using any number of /composable/ checkers that you
--       write for your specific problem domain. Once you have a vocabulary
--       of checkers, creation of new forms is just a matter of combining
--       them, and yes they do combine nicely.
--     * Collect validation errors from multiple branches of parsing (one
--       branch per form field) in parallel, so validation errors in one
--       branch do not prevent us from collecting validation errors from
--       other branches. This allows for a better user experience as the
--       user can see all validation errors at the same time.
--     * Use 'optional' and @('<|>')@ from "Control.Applicative" in your
--       form definitions instead of ugly ad-hoc stuff (yes
--       @digestive-functors@, I'm looking at you).
--     * When individual validation of fields is done, you get a chance to
--       perform some actions and either decide that form submission has
--       succeeded, or indeed perform additional checks that may depend on
--       several form fields at once and signal a validation error assigned
--       to a specific field(s). This constitute the “second level” of
--       validation, so to speak.
--
-- __This library requires at least GHC 8 to work.__
--
-- You need to enable at least @DataKinds@ and @TypeApplications@ language
-- extensions to use this library.

{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Forma
  ( -- * Constructing a form
    field
  , field'
  , value
  , subParser
  , withCheck
    -- * Running a form
  , runForm
  , pick
  , unSelectedName
  , mkFieldError
  , unFieldError
    -- * Types and type functions
  , FormParser
  , FormResult (..)
  , SelectedName
  , InSet
  , FieldError )
where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.Except
import Data.Aeson
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Proxy
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)
import GHC.TypeLits
import qualified Data.Aeson.Types    as A
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty  as NE
import qualified Data.Map.Strict     as M
import qualified Data.Text           as T

----------------------------------------------------------------------------
-- Types

-- | Result of parsing. @names@ is the collection of allowed field names,
-- @e@ is the type of validation errors, and @a@ is the type of parsing
-- result.
--
-- @since 1.0.0

data FormResult (names :: [Symbol]) e a
  = ParsingFailed [SelectedName names] String
    -- ^ Parsing of JSON failed, this is fatal, we shut down and report the
    -- parsing error. The first component specifies path to a problematic
    -- field and the second component is the text of error message.
  | ValidationFailed (FieldError names e)
    -- ^ Validation of a field failed. This is also fatal but we still try
    -- to validate other branches (fields) to collect as many validation
    -- errors as possible.
  | Succeeded a
    -- ^ Success, we've got a result to return.
  deriving (Eq, Show, Functor)

instance (ToJSON e, ToJSON a) => ToJSON (FormResult names e a) where
  toJSON = \case
    ParsingFailed path msg ->
      f (Just (path, msg)) Nothing Nothing
    ValidationFailed verr ->
      f Nothing (Just verr) Nothing
    Succeeded x ->
      f Nothing Nothing (Just x)
    where
      f :: Maybe ([SelectedName names], String)
        -> Maybe (FieldError names e)
        -> Maybe a
        -> Value
      f perr verr result = object
        [ "parse_error" .=
          case perr of
            Nothing          -> Null
            Just (path, msg) -> fieldPathToJSON path (String $ T.pack msg)
        , "field_errors" .= maybe (Object HM.empty) toJSON verr
        , "result" .= result
        ]

instance Applicative (FormResult names e) where
  pure                                            = Succeeded
  (ParsingFailed l msg) <*> _                     = ParsingFailed l msg
  (ValidationFailed _)  <*> (ParsingFailed l msg) = ParsingFailed l msg
  (ValidationFailed e0) <*> (ValidationFailed e1) = ValidationFailed (e0 <> e1)
  (ValidationFailed e)  <*> Succeeded _           = ValidationFailed e
  Succeeded _           <*> (ParsingFailed l msg) = ParsingFailed l msg
  Succeeded _           <*> (ValidationFailed e)  = ValidationFailed e
  Succeeded f           <*> Succeeded x           = Succeeded (f x)

-- | The type represents the parser that you can run on a 'Value' with the
-- help of 'runForm'. The only way for the user of the library to create a
-- parser is via the 'field' function. Users can combine existing parsers
-- using the applicative notation.
--
-- 'FormParser' is parametrized by four type variables:
--
--     * @names@—collection of field names we can use in a form to be parsed
--       with this parser.
--     * @e@—type of validation errors.
--     * @m@—underlying monad, 'FormParser' is not a monad itself, so it's
--       not a monad transformer, but validation can make use of the @m@
--       monad.
--     * @a@—result of parsing.
--
-- 'FormParser' is not a monad because it's not possible to write a 'Monad'
-- instance with the properties that we want (validation errors should not
-- lead to short-cutting behavior).

newtype FormParser (names :: [Symbol]) e m a = FormParser
  { unFormParser
      :: Value
      -> ([SelectedName names] -> [SelectedName names])
      -> m (FormResult names e a)
  }

instance Functor m => Functor (FormParser names e m) where
  fmap f (FormParser x) = FormParser $ \v path ->
    fmap (fmap f) (x v path)

instance Applicative m => Applicative (FormParser names e m) where
  pure x = FormParser $ \_ _ ->
    pure (Succeeded x)
  (FormParser f) <*> (FormParser x) = FormParser $ \v path ->
    pure (<*>) <*> f v path <*> x v path

instance Applicative m => Alternative (FormParser names e m) where
  empty = FormParser $ \_ path ->
    pure (ParsingFailed (path []) "empty")
  (FormParser x) <|> (FormParser y) = FormParser $ \v path ->
    let g x' y' =
          case x' of
            ParsingFailed  _ _ -> y'
            ValidationFailed _ -> x'
            Succeeded        _ -> x'
    in pure g <*> x v path <*> y v path

-- | @'SelectedName' names@ represents a name ('Text' value) that is
-- guaranteed to be in the @names@, which is a set of strings on type level.
-- The purpose if this type is to avoid typos and to force users to update
-- field names everywhere when they decide to change them. The only way to
-- obtain a value of type 'SelectedName' is via the 'pick' function, which
-- see.

newtype SelectedName (names :: [Symbol])
  = SelectedName Text
  deriving (Eq, Ord, Show)

-- | The type function computes a 'Constraint' which is satisfied when its
-- first argument is contained in its second argument. Otherwise a friendly
-- type error is displayed.

type family InSet (n :: Symbol) (ns :: [Symbol]) :: Constraint where
  InSet n '[]    = TypeError
    ('Text "The name " ':<>: 'ShowType n ':<>: 'Text " is not in the given set."
     ':$$:
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
-- It's a good idea to use 'pick' to get field names not only where this
-- approach is imposed by the library, but everywhere you need to use the
-- field names, in your templates for example.

pick :: forall (name :: Symbol) (names :: [Symbol]).
  ( KnownSymbol name
  , InSet name names )
  => SelectedName names
pick = (SelectedName . T.pack . symbolVal) (Proxy :: Proxy name)

-- | Extract a 'Text' value from 'SelectedName'.

unSelectedName :: SelectedName names -> Text
unSelectedName (SelectedName txt) = txt

-- | Error info in JSON format associated with a particular form field.
-- Parametrized by @names@, which is a collection of field names (on type
-- level) the target field belongs to. 'FieldError' is an instance of
-- 'Semigroup' and that's how you combine values of that type. Note that
-- it's not a 'Monoid', because we do not want to allow empty 'FieldError's.

newtype FieldError (names :: [Symbol]) e
  = FieldError (Map (NonEmpty (SelectedName names)) e)
  deriving (Eq, Show)

instance Semigroup (FieldError names e) where
  (FieldError x) <> (FieldError y) = FieldError (M.union x y)

instance ToJSON e => ToJSON (FieldError names e) where
  toJSON (FieldError m) =
    concatObjects $ uncurry fieldPathToJSON . second toJSON <$> M.toAscList m

-- | This is a smart constructor for the 'FieldError' type, and the only way
-- to obtain values of that type.
--
-- Typical usage:
--
-- > type Fields = '["foo", "bar", "baz"]
-- >
-- > myError :: FieldError Fields Text
-- > myError = mkFieldError (pick @"foo" @Fields) "That's all wrong."
--
-- See also: 'pick' (to create 'SelectedName').
--
-- __Note__: type of the first argument has been changed in the version
-- /1.0.0/.

mkFieldError
  :: NonEmpty (SelectedName names) -- ^ The path to problematic field
  -> e                 -- ^ Data that represents error
  -> FieldError names e
mkFieldError path x = FieldError (M.singleton path x)

-- | Extract internal map from a 'FieldError'.
--
-- @since 1.0.0

unFieldError
  :: FieldError names e
  -> Map (NonEmpty (SelectedName names)) e
unFieldError (FieldError m) = m

----------------------------------------------------------------------------
-- Constructing a form

-- | Construct a parser for a field. Combine multiple 'field's using
-- applicative syntax like so:
--
-- > type LoginFields = '["username", "password", "remember_me"]
-- >
-- > data LoginForm = LoginForm
-- >   { loginUsername   :: Text
-- >   , loginPassword   :: Text
-- >   , loginRememberMe :: Bool
-- >   }
-- >
-- > loginForm :: Monad m => FormParser LoginFields Text m LoginForm
-- > loginForm = LoginForm
-- >   <$> field @"username" notEmpty
-- >   <*> field @"password" notEmpty
-- >   <*> field' @"remember_me"
-- >
-- > notEmpty :: Monad m => Text -> ExceptT Text m Text
-- > notEmpty txt =
-- >   if T.null txt
-- >     then throwError "This field cannot be empty"
-- >     else return txt
--
-- Referring to the types in the function's signature, @s@ is extracted from
-- JSON 'Value' for you automatically using its 'FromJSON' instance. The
-- field value is taken in assumption that top level 'Value' is a
-- dictionary, and field name is a key in that dictionary. So for example a
-- valid JSON input for the form shown above could be this:
--
-- > {
-- >   "username": "Bob",
-- >   "password": "123",
-- >   "remember_me": true
-- > }
--
-- Once value of type @s@ is extracted, validation phase beings. The
-- supplied checker (you can easy compose them with @('>=>')@, as they are
-- Kleisli arrows) is applied to the @s@ value and validation either
-- succeeds producing an @a@ value, or we collect an error in the form of a
-- value of @e@ type, which is fed into 'mkFieldError' internally.
--
-- To run a form composed from 'field's, see 'runForm'.

field :: forall (name :: Symbol) (names :: [Symbol]) e m a s.
  ( KnownSymbol name
  , InSet name names
  , Monad m
  , FromJSON s )
  => (s -> ExceptT e m a)
     -- ^ Checker that performs validation and possibly transformation of
     -- the field value
  -> FormParser names e m a
field check = withCheck @name check (field' @name)

-- | The same as 'field', but does not require a checker.

field' :: forall (name :: Symbol) (names :: [Symbol]) e m a.
  ( KnownSymbol name
  , InSet name names
  , Monad m
  , FromJSON a )
  => FormParser names e m a
field' = subParser @name value

-- | Interpret the current field as a value of type @a@.
--
-- @since 1.0.0

value :: (Monad m , FromJSON a) => FormParser names e m a
value = FormParser $ \v path ->
  case A.parseEither parseJSON v of
    Left msg -> do
      let msg' = drop 2 (dropWhile (/= ':') msg)
      return (ParsingFailed (path []) msg')
    Right x -> return (Succeeded x)

-- | Use a given parser to parse a field. Suppose that you have a parser
-- @loginForm@ that parses a structure like this one:
--
-- > {
-- >   "username": "Bob",
-- >   "password": "123",
-- >   "remember_me": true
-- > }
--
-- Then @subParser \@"login" loginForm@ will parse this:
--
-- > {
-- >   "login": {
-- >      "username": "Bob",
-- >      "password": "123",
-- >      "remember_me": true
-- >    }
-- > }
--
-- @since 1.0.0

subParser :: forall (name :: Symbol) (names :: [Symbol]) e m a.
  ( KnownSymbol name
  , InSet name names
  , Monad m )
  => FormParser names e m a -- ^ Subparser
  -> FormParser names e m a -- ^ Wrapped parser
subParser p = FormParser $ \v path -> do
  let name = pick @name @names
      f = withObject "form field" (.: unSelectedName name)
      path' = path . (name :)
  case A.parseEither f v of
    Left msg -> do
      let msg' = drop 2 (dropWhile (/= ':') msg)
      return (ParsingFailed (path' []) msg')
    Right v' ->
      unFormParser p v' path'

-- | Transform a form by applying a checker on its result.
--
-- > passwordsMatch (a, b) = do
-- >   if a == b
-- >     then return a
-- >     else throwError "Passwords don't match!"
-- >
-- > createNewPasswordForm =
-- >   withCheck @"password_confirmation" passwordsMatch
-- >     ((,) <$> field @"password" notEmpty
-- >          <*> field @"password_confirmation" notEmpty)
--
-- Note that you must specify the field name on which to add a validation
-- error message in case the check fails.
--
-- @since 0.2.0

withCheck :: forall (name :: Symbol) (names :: [Symbol]) e m a s.
  ( KnownSymbol name
  , InSet name names
  , Monad m )
  => (s -> ExceptT e m a) -- ^ The check to perform
  -> FormParser names e m s -- ^ Original parser
  -> FormParser names e m a -- ^ Parser with the check attached
withCheck check (FormParser f) = FormParser $ \v path -> do
  let name = pick @name @names
  r <- f v path
  case r of
    Succeeded x -> do
      res <- runExceptT (check x)
      return $ case res of
        Left verr ->
          let path' = NE.fromList (path . (name :) $ [])
          in ValidationFailed (mkFieldError path' verr)
        Right y ->
          Succeeded y
    ValidationFailed e ->
      return (ValidationFailed e)
    ParsingFailed path' msg ->
      return (ParsingFailed path' msg)

----------------------------------------------------------------------------
-- Running a form

-- | Run the supplied parser on given input and call the specified callback
-- that uses the result of parsing on success.
--
-- The callback can either report a 'FieldError' (one or more), or report
-- success providing a result value.
--
-- __Note__: type of the function has been changed in the version /1.0.0/.

runForm :: Monad m
  => FormParser names e m a
     -- ^ The form parser to run
  -> Value
     -- ^ Input for the parser
  -> (a -> m (Either (FieldError names e) b))
     -- ^ Callback that is called on success
  -> m (FormResult names e b)
     -- ^ The result of parsing
runForm (FormParser p) v f = do
  r <- p v id
  case r of
    ParsingFailed path msg ->
      return (ParsingFailed path msg)
    ValidationFailed verr ->
      return (ValidationFailed verr)
    Succeeded x -> do
      r' <- f x
      return $ case r' of
        Left verr -> ValidationFailed verr
        Right result -> Succeeded result

----------------------------------------------------------------------------
-- Helpers

-- | Concatenate JSON objects on their keys.

concatObjects :: [Value] -> Value
concatObjects =
  Object . foldr (HM.unionWith concatValues . unwrap) HM.empty
  where
    unwrap (Object o) = o
    unwrap _          = HM.empty

-- | Concatenate JSON objects\' values.

concatValues :: Value -> Value -> Value
concatValues (Object o1) (Object o2)  = Object $ HM.unionWith concatValues o1 o2
concatValues _           o@(Object _) = o
concatValues o           _            = o

-- | Unroll the field path to JSON that mimics the structure of the input.

fieldPathToJSON :: Foldable t => t (SelectedName names) -> Value -> Value
fieldPathToJSON =
  flip $ foldr (\next acc -> object [unSelectedName next .= acc])
