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


notEmpty :: Monad m => Text -> ExceptT Text m Text
notEmpty txt =
  if T.null txt
    then throwError "This field cannot be empty."
    else return txt


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


type SignupFields = '["username", "password", "password_confirmation"]

data SignupForm = SignupForm
  { signupUsername :: Text
  , signupPassword :: Text
  }

tupleValuesMatch :: Monad m => FieldError names -> ((Text, Text) -> ExceptT (FieldError names) m Text)
tupleValuesMatch err (a,b) =
  if a == b
    then return a
    else throwError err

signupForm :: Monad m => FormParser SignupFields m SignupForm
signupForm = SignupForm
  <$> field @"username" notEmpty
  <*> (addValidation
        ((,) <$> field @"password" notEmpty <*> field @"password_confirmation" notEmpty)
        (tupleValuesMatch (fieldError @"password_confirmation" "Passwords don't match!"))
      )



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
            , "result"     .= Null ]
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

  context "For addValidation being used in SignupForm example" $ do
    context "when both password fields are empty" $ 
      it "we get errors for both empty password fields" $ do
        let input = object
              [ "username"    .= String ""
              , "password"    .= String ""
              , "password_confirmation" .= String "" ]
        r <- runForm signupForm input $ \_ ->
          return (FormResultSuccess ())
        r `shouldBe` object
          [ "parse_error"  .= Null
          , "field_errors" .= object
            [ "username" .= String "This field cannot be empty."
            , "password" .= String "This field cannot be empty."
            , "password_confirmation" .= String "This field cannot be empty." ]
          , "result"       .= Null ]
    context "when both password fields contain values that don't match" $
      it "the validation added with addValidation reports that passwords don't match" $ do
        let input = object
              [ "username"    .= String ""
              , "password"    .= String "abc"
              , "password_confirmation" .= String "def" ]
        r <- runForm signupForm input $ \_ ->
          return (FormResultSuccess ())
        r `shouldBe` object
          [ "parse_error"  .= Null
          , "field_errors" .= object
            [ "username" .= String "This field cannot be empty."
            , "password_confirmation" .= String "Passwords don't match!" ]
          , "result"       .= Null ]
    context "when username and both password fields are filled in correctly" $
      it "it validates and returns the correct value" $ do
        let input = object
              [ "username"    .= String "Bob"
              , "password"    .= String "abc"
              , "password_confirmation" .= String "abc" ]
        r <- runForm signupForm input $ \SignupForm {..} -> do
          return (FormResultSuccess ( signupUsername <> signupPassword ))
        r `shouldBe` object
          [ "parse_error"  .= Null
          , "field_errors" .= object []
          , "result"       .= String "Bobabc" ]
    
