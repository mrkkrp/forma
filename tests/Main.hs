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
