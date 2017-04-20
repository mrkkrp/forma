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
--       labels being stored on the type label which excludes any
--       possibility of typos and will force all your field labels be always
--       up to date.
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
  , field'
    -- * Running a form
  , runForm
  , pick
  , unSelectedName
  , mkFieldError
    -- * Types and type functions
  , FormParser
  , FormResult (..)
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
  = ParsingFailed String
    -- ^ Parsing of JSON failed, this is fatal, we shut down and report the
    -- parsing error.
  | ValidationFailed (FieldError names)
    -- ^ Validation of a field failed. This is also fatal but we still try
    -- to validate other branches (fields) to collect as many validation
    -- errors as possible.
  | Succeeded a
    -- ^ Success, we've got a result to return.
  deriving Functor

instance Applicative (BranchState names) where
  pure                                            = Succeeded
  (ParsingFailed msg)   <*> _                     = ParsingFailed msg
  (ValidationFailed _)  <*> (ParsingFailed msg)   = ParsingFailed msg
  (ValidationFailed e0) <*> (ValidationFailed e1) = ValidationFailed (e0 <> e1)
  (ValidationFailed e)  <*> Succeeded _           = ValidationFailed e
  Succeeded _           <*> (ParsingFailed msg)   = ParsingFailed msg
  Succeeded _           <*> (ValidationFailed e)  = ValidationFailed e
  Succeeded f           <*> Succeeded x           = Succeeded (f x)

-- | The type represents the parser that you can run on a 'Value' with the
-- help of 'runForm'. The only way for the user of the library to create a
-- parser is via the 'field' function. Users can combine existing parsers
-- using the applicative notation.
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
  fmap f (FormParser x) = FormParser (fmap (fmap f) . x)

instance Applicative m => Applicative (FormParser names m) where
  pure x = (FormParser . const . pure) (Succeeded x)
  (FormParser f) <*> (FormParser x) = FormParser $ \v ->
    pure (<*>) <*> f v <*> x v

instance Applicative m => Alternative (FormParser names m) where
  empty = (FormParser . const . pure) (ParsingFailed "empty")
  (FormParser x) <|> (FormParser y) = FormParser $ \v ->
    let g x' y' =
          case x' of
            ParsingFailed    _ -> y'
            ValidationFailed _ -> x'
            Succeeded        _ -> x'
    in pure g <*> x v <*> y v

-- | This a type that user must return in the callback passed to the
-- 'runForm' function. Quite simply, it allows you either report a error or
-- finish successfully.

data FormResult (names :: [Symbol]) a
  = FormResultError (FieldError names)
    -- ^ Form submission failed, here are the validation errors.
  | FormResultSuccess a
    -- ^ Form submission succeeded, send this info.
  deriving (Eq, Show)

-- | @'SelectedName' names@ represents a name ('Text' value) that is
-- guaranteed to be in the @names@, which is a set of strings on type level.
-- The purpose if this type is to avoid typos and to force users to update
-- field names everywhere when they decide to change them. The only way to
-- obtain a value of type 'SelectedName' is via the 'pick' function, which
-- see.

newtype SelectedName (names :: [Symbol])
  = SelectedName Text
  deriving (Eq, Show)

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

data FieldError (names :: [Symbol])
  = FieldError (Map Text Value)
  deriving (Eq, Show)

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
-- See also: 'pick' (to create 'SelectedName').

mkFieldError :: ToJSON e
  => SelectedName names -- ^ The field name
  -> e                 -- ^ Data that represents error
  -> FieldError names
mkFieldError name x =
  FieldError (M.singleton (unSelectedName name) (toJSON x))

-- | An internal type of response that we covert to 'Value' before returning
-- it.

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
-- > loginForm :: Monad m => FormParser LoginFields m LoginForm
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

field :: forall (name :: Symbol) (names :: [Symbol]) m e s a.
  ( KnownSymbol name
  , InSet name names
  , Monad m
  , ToJSON e
  , FromJSON s )
  => (s -> ExceptT e m a)
     -- ^ Checker that performs validation and possibly transformation of
     -- the field value
  -> FormParser names m a
field check = FormParser $ \v -> do
  let name = pick @name @names
      f :: Value -> A.Parser s
      f = withObject "form field" (.: unSelectedName name)
      r = A.parseEither f v
  case r of
    Left parseError -> pure (ParsingFailed parseError)
    Right r' -> do
      e <- runExceptT (check r')
      return $ case e of
        Left verr ->
          (ValidationFailed (mkFieldError name verr))
        Right x ->
          (Succeeded x)

-- | The same as 'field', but does not require a checker.

field' :: forall (name :: Symbol) (names :: [Symbol]) m a.
  ( KnownSymbol name
  , InSet name names
  , Monad m
  , FromJSON a )
  => FormParser names m a
field' = field @name check
  where
    check :: a -> ExceptT () m a
    check = return

----------------------------------------------------------------------------
-- Running a form

-- | Run the supplied parser on given input and call the specified callback
-- that uses the result of parsing on success.
--
-- The callback can either report a 'FieldError' (one or more), or report
-- success providing a value that will be converted to JSON and including in
-- the resulting 'Value' (response).
--
-- The resulting 'Value' has the following format:
--
-- > {
-- >   "parse_error": "Text or null."
-- >   "field_errors":
-- >     {
-- >       "foo": "Foo's error serialized to JSON.",
-- >       "bar": "Bar's error…"
-- >     }
-- >   "result": "What you return from the callback in FormResultSuccess."
-- > }

runForm :: (Monad m, ToJSON b)
  => FormParser names m a -- ^ The form parser to run
  -> Value             -- ^ Input for the parser
  -> (a -> m (FormResult names b)) -- ^ Callback that is called on success
  -> m Value           -- ^ The result to send back to the client
runForm (FormParser p) v f = do
  r <- p v
  case r of
    ParsingFailed parseError -> return . toJSON $
      def { responseParseError = pure parseError }
    ValidationFailed validationError -> return . toJSON $
      def { responseFieldError = pure validationError }
    Succeeded x -> do
      r' <- f x
      return . toJSON $ case r' of
        FormResultError validationError ->
          def { responseFieldError = pure validationError }
        FormResultSuccess result ->
          def { responseResult = toJSON result }
