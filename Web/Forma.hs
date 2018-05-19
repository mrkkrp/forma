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
--       form definitions instead of ugly ad-hoc stuff.
--     * Perform validation using several form fields at once. You choose
--       which “sub-region” of your form a given check will have access to,
--       see 'withCheck'.
--
-- You need to enable at least @DataKinds@ and @OverloadedLabels@ language
-- extensions to use this library.
--
-- __Note__: version /1.0.0/ is completely different from older versions.

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Web.Forma
  ( -- * Constructing a form
    field
  , field'
  , value
  , subParser
  , withCheck
    -- * Running a form\/inspecting result
  , runForm
  , unFieldName
  , showFieldName
    -- * Types and type functions
  , FormParser
  , FormResult (..)
  , FieldName
  , InSet )
where

import Control.Applicative
import Control.Monad.Except
import Data.Aeson
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Proxy
import Data.Semigroup
import Data.Text (Text)
import GHC.OverloadedLabels (IsLabel (..))
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

data FormResult (names :: [Symbol]) e a
  = ParsingFailed (Maybe (FieldName names)) Text
    -- ^ Parsing of JSON failed, this is fatal, we shut down and report the
    -- parsing error. The first component specifies path to a problematic
    -- field and the second component is the text of error message.
  | ValidationFailed (Map (FieldName names) e)
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
      f :: Maybe (Maybe (FieldName names), Text)
        -> Maybe (Map (FieldName names) e)
        -> Maybe a
        -> Value
      f perr verr result = object
        [ "parse_error" .=
          case perr of
            Nothing -> Null
            Just (path, msg) -> object
              [ "field"   .= path
              , "message" .= msg
              ]
        , "field_errors" .=
          let g (fieldName, err) = showFieldName fieldName .= err
          in maybe (Object HM.empty) (object . fmap g . M.toAscList) verr
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
-- parser is via the 'field' function and its friends, see below. Users can
-- combine existing parsers using applicative notation.
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
      -> Option (FieldName names)
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
  empty = FormParser $ \_ _ ->
    pure (ParsingFailed Nothing "empty")
  (FormParser x) <|> (FormParser y) = FormParser $ \v path ->
    let g x' y' =
          case x' of
            ParsingFailed  _ _ -> y'
            ValidationFailed _ -> x'
            Succeeded        _ -> x'
    in pure g <*> x v path <*> y v path

-- | @'FieldName' names@ represents a non-empty vector of 'Text' components
-- that serve as a path to some field in a JSON structure. Every component
-- is guaranteed to be in the @names@, which is a set of strings on type
-- level. The purpose if this type is to avoid typos and to force users to
-- update field names everywhere when they decide to change them. The only
-- way to obtain a value of the type 'FieldName' is by using
-- @OverloadedLabels@. Note that you can combine field names using @('<>')@.
--
-- > showFieldName (#login_form <> #username) = "login_form.username"

newtype FieldName (names :: [Symbol])
  = FieldName (NonEmpty Text)
  deriving (Eq, Ord, Show)

instance (KnownSymbol name, InSet name names)
  => IsLabel (name :: Symbol) (FieldName names) where
#if MIN_VERSION_base(4,10,0)
  fromLabel =
#else
  fromLabel _ =
#endif
      (FieldName . nes . T.pack . symbolVal) (Proxy :: Proxy name)
    where
      nes x = x :| []

instance Semigroup (FieldName names) where
  FieldName x <> FieldName y = FieldName (x <> y)

instance ToJSON (FieldName names) where
  toJSON = toJSON . showFieldName

-- | Project field path from a 'FieldName'.

unFieldName :: FieldName names -> NonEmpty Text
unFieldName (FieldName path) = path

-- | Project textual representation of path to a field.

showFieldName :: FieldName names -> Text
showFieldName = T.intercalate "." . NE.toList . unFieldName

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
-- >   <$> field #username notEmpty
-- >   <*> field #password notEmpty
-- >   <*> field' #remember_me
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
-- supplied checker (you can easily compose them with @('>=>')@, as they are
-- Kleisli arrows) is applied to the @s@ value and validation either
-- succeeds producing an @a@ value, or we collect an error as a value of @e@
-- type.
--
-- To run a form composed from 'field's, see 'runForm'.
--
-- > field fieldName check = withCheck fieldName check (field' fieldName)

field :: forall (names :: [Symbol]) e m a s. (Monad m, FromJSON s)
  => FieldName names
     -- ^ Name of the field
  -> (s -> ExceptT e m a)
     -- ^ Checker that performs validation and possibly transformation of
     -- the field value
  -> FormParser names e m a
field fieldName check = withCheck fieldName check (field' fieldName)

-- | The same as 'field', but does not require a checker.
--
-- > field' fieldName = subParser fieldName value

field' :: forall (names :: [Symbol]) e m a. (Monad m, FromJSON a)
  => FieldName names
     -- ^ Name of the field
  -> FormParser names e m a
field' fieldName = subParser fieldName value

-- | Interpret the current field as a value of type @a@.

value :: (Monad m , FromJSON a) => FormParser names e m a
value = FormParser $ \v path ->
  case A.parseEither parseJSON v of
    Left msg -> return $
      ParsingFailed (getOption path) (fixupAesonError msg)
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
-- Then @subParser #login loginForm@ will parse this:
--
-- > {
-- >   "login": {
-- >      "username": "Bob",
-- >      "password": "123",
-- >      "remember_me": true
-- >    }
-- > }

subParser :: forall (names :: [Symbol]) e m a.
  Monad m
  => FieldName names        -- ^ Field name to descend to
  -> FormParser names e m a -- ^ Subparser
  -> FormParser names e m a -- ^ Wrapped parser
subParser fieldName p = FormParser $ \v path -> do
  let f = withObject "form field" (.: showFieldName fieldName)
      path' = path <> Option (Just fieldName)
  case A.parseEither f v of
    Left msg -> do
      let msg' = fixupAesonError msg
      return (ParsingFailed (getOption path') msg')
    Right v' ->
      unFormParser p v' path'

-- | Transform a form by applying a checker on its result.
--
-- > passwordsMatch (a, b) = do
-- >   if a == b
-- >     then return a
-- >     else throwError "Passwords don't match!"
-- >
-- > passwordForm =
-- >   withCheck #password_confirmation passwordsMatch
-- >     ((,) <$> field #password notEmpty
-- >          <*> field #password_confirmation notEmpty)
--
-- Note that you must specify the field name on which to add a validation
-- error message in case the check fails. The field name should be relative
-- and point to a field in the argument parser, not full path from top-level
-- of the form. For example this form:
--
-- > biggerForm = subParser #password_form passwordForm
--
-- will report validation error for the field
-- @\"password_form.password_confirmation\"@ if the check fails (note that
-- @\"password_form\"@ is correctly prepended to the field path).

withCheck :: forall (names :: [Symbol]) e m a s. Monad m
  => FieldName names        -- ^ Field to assign validation error to
  -> (s -> ExceptT e m a)   -- ^ The check to perform
  -> FormParser names e m s -- ^ Original parser
  -> FormParser names e m a -- ^ Parser with the check attached
withCheck fieldName check (FormParser f) = FormParser $ \v path -> do
  r <- f v path
  case r of
    Succeeded x -> do
      res <- runExceptT (check x)
      return $ case res of
        Left verr ->
          ValidationFailed (M.singleton (fromOption fieldName path) verr)
        Right y ->
          Succeeded y
    ValidationFailed e ->
      return (ValidationFailed e)
    ParsingFailed path' msg ->
      return (ParsingFailed path' msg)

----------------------------------------------------------------------------
-- Running a form

-- | Run a parser on given input.

runForm :: Monad m
  => FormParser names e m a
     -- ^ The form parser to run
  -> Value
     -- ^ Input for the parser
  -> m (FormResult names e a)
     -- ^ The result of parsing
runForm (FormParser p) v = p v (Option Nothing)

----------------------------------------------------------------------------
-- Helpers

-- | Fixup a error message returned by Aeson.

fixupAesonError :: String -> Text
fixupAesonError msg = T.pack (drop 2 (dropWhile (/= ':') msg))

-- | Like 'Data.Maybe.fromMaybe' but for 'Option'.

fromOption :: a -> Option a -> a
fromOption x = option x id
