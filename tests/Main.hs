--
-- Tests for the ‘forma’ package.
--
-- Copyright © 2017 Mark Karpov <markkarpov@openmailbox.org>
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name Mark Karpov nor the names of contributors may be used
--   to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import Control.Applicative
import Control.Monad.Except
import Data.Aeson
import Data.Semigroup ((<>))
import Data.Text (Text)
import Test.Hspec
import Web.Forma
import qualified Data.Text as T

type LoginFields = '["username", "password", "remember_me"]

data LoginForm = LoginForm
  { loginUsername   :: Text
  , loginPassword   :: Text
  , loginRememberMe :: Bool
  }

loginForm :: Monad m => FormParser LoginFields m LoginForm
loginForm = LoginForm
  <$> field @"username" notEmpty
  <*> field @"password" notEmpty
  <*> (empty <|> field' @"remember_me" <|> pure True)

notEmpty :: Monad m => Text -> ExceptT Text m Text
notEmpty txt =
  if T.null txt
    then throwError "This field cannot be empty."
    else return txt

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Forma" $ do
  context "when a parse error happens" $
    it "it's reported immediately" $ do
      let input = object
            [ "username"    .= (1 :: Int)
            , "password"    .= (2 :: Int)
            , "remember_me" .= True ]
      r <- runForm loginForm input $ \_ ->
        return (FormResultSuccess ())
      r `shouldBe` object
        [ "parse_error"  .= String "Error in $.username: expected Text, encountered Number"
        , "field_errors" .= object []
        , "result"       .= Null ]
  context "when no parse error happens" $ do
    context "when no validation errors happen in 1 step" $ do
      context "when callback reports success" $
        it "correct resulting value is returned" $ do
          let input = object
                [ "username"    .= String "Bob"
                , "password"    .= String "123" ]
          r <- runForm loginForm input $ \LoginForm {..} -> do
            loginRememberMe `shouldBe` True
            return (FormResultSuccess (loginUsername <> loginPassword))
          r `shouldBe` object
            [ "parse_error"  .= Null
            , "field_errors" .= object []
            , "result"       .= String "Bob123" ]
      context "when callback reports validation errors" $
        it "correct resulting value is returned" $ do
          let input = object
                [ "username"    .= String "Bob"
                , "password"    .= String "123"
                , "remember_me" .= True ]
              msg0, msg1 :: Text
              msg0 = "I don't like this username."
              msg1 = "I don't like this password."
          r <- runForm loginForm input $ \LoginForm {..} -> do
            let e0 = mkFieldError (pick @"username" @LoginFields) msg0
                e1 = mkFieldError (pick @"password" @LoginFields) msg1
            return (FormResultError (e0 <> e1) :: FormResult LoginFields ())
          r `shouldBe` object
            [ "parse_error"  .= Null
            , "field_errors" .= object
              [ "username" .= msg0
              , "password" .= msg1 ]
            , "result"       .= Null ]
    context "when validation errors happen in 1 step" $
      it "all of them are reported" $ do
        let input = object
              [ "username"    .= String ""
              , "password"    .= String ""
              , "remember_me" .= True ]
        r <- runForm loginForm input $ \_ ->
          return (FormResultSuccess ())
        r `shouldBe` object
          [ "parse_error"  .= Null
          , "field_errors" .= object
            [ "username" .= String "This field cannot be empty."
            , "password" .= String "This field cannot be empty." ]
          , "result"       .= Null ]
