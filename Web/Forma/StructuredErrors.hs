-- |
-- Module      :  Web.Forma.Format
-- Copyright   :  © 2017–2018 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a wrapper newtype around 'FormResult' that formats
-- errors to JSON in an alternative style, in which the structure of errors
-- mimics the structure of the input.
--
-- Module "Web.Forma" is re-exported, so you can easily use this module as a drop-in
-- replacement.

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE KindSignatures #-}

module Web.Forma.StructuredErrors
  ( module Web.Forma
  , runForm'
  , StructuredErrors (..)
  )
where

import Control.Arrow (first, second)
import Data.Aeson
import Data.Text (Text)
import GHC.TypeLits
import Web.Forma
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as M

-- | This wrapper type around 'FormResult' overrides its 'ToJSON' instance
-- to format errors in a way that mimics the structure of the input.
--
-- For example, with form fields defined as:
--
-- > type SignupFields = '["user", "email", "password"]
--
-- (1)  A 'ParsingFailed' error like this:
--
--      > ParsingFailed (pure (#user <> #email)) "Expecting a string"
--
--      becomes:
--
--      > object
--      >  [ "user" .= object
--      >    [ "email" .= ("Expecting a string" :: Text)
--      >    ]
--      >  ]
--
-- (2) And 'ValidationFailed' error like this:
--
--      > ValidationFailed (M.fromList
--      >                   [ (#user <> #email, "Invalid email address")
--      >                   , (#user <> #password, "Too short")
--      >                   ]
--      >                  )
--
--      becomes:
--
--      > object
--      >  [ "user" .= object
--      >    [ "email" .= ("Invalid email address" :: Text)
--      >    , "password" .= ("Too short" :: Text)
--      >    ]
--      >  ]
--
-- __Note__ that this style of formatting validation errors gives
-- /precedence/ to errors nested at a deeper level, in which case the validation
-- of the parent object is implied to have failed. So for example an error
-- like this:
--
-- > ValidationFailed (M.fromList
-- >                   [ (#user, "Invalid user")
-- >                   , (#user <> #email, "Invalid email address")
-- >                   ]
-- >                  )
--
-- becomes:
--
-- > object
-- >  [ "user" .= object
-- >    [ "email" .= ("Invalid email address" :: Text)
-- >    ]
-- >  ]

newtype StructuredErrors (names :: [Symbol]) e a =
  StructuredErrors (FormResult names e a )
  deriving (Eq, Show, Functor)

instance (ToJSON e, ToJSON a) => ToJSON (StructuredErrors names e a) where
  toJSON (StructuredErrors formResult) =
    case formResult of
      ParsingFailed path msg ->
        maybe val (flip fieldPathToJSON val . unFieldName) path
        where val = String msg
      ValidationFailed err ->
        concatObjects $
          uncurry fieldPathToJSON . first unFieldName . second toJSON <$>
            M.toAscList err
      Succeeded x ->
        toJSON x

-- | Run a parser on given input that returns 'FormResult' wrapped in
-- 'StructuredErrors'.

runForm' :: Monad m
  => FormParser names e m a
     -- ^ The form parser to run
  -> Value
     -- ^ Input for the parser
  -> m (StructuredErrors names e a)
     -- ^ The result of parsing
runForm' f v = StructuredErrors <$> runForm f v

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

fieldPathToJSON :: Foldable t => t Text -> Value -> Value
fieldPathToJSON =
  flip $ foldr (\next acc -> object [next .= acc])
