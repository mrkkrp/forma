{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeApplications     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.Applicative
import Control.Monad.Except
import Data.Aeson
import Data.List.NonEmpty (NonEmpty (..))
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

type SignupFields = '["username", "password", "password_confirmation"]

data SignupForm = SignupForm
  { signupUsername :: Text
  , signupPassword :: Text
  }

signupForm :: Monad m => FormParser SignupFields m SignupForm
signupForm = SignupForm
  <$> field @"username" notEmpty
  <*> withCheck @"password_confirmation" passwordsMatch
        ((,) <$> field @"password" notEmpty
             <*> field @"password_confirmation" notEmpty)

passwordsMatch :: Monad m => (Text, Text) -> ExceptT Text m Text
passwordsMatch (a,b) =
  if a == b
    then return a
    else throwError "Passwords don't match!"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "value" $ do
    let text :: Monad m => FormParser n m Text
        text = value
    context "when the data is convertable" $
      it "succeeds" $ do
        let input = String "Foo"
        r <- runForm text input finalize
        r `shouldBe` rSuccess input
    context "when the data is not convertable" $
      it "fails to parse" $ do
        let input = Bool True
        r <- runForm text input finalize
        r `shouldBe` rParseError "" "expected Text, encountered Boolean"
  describe "subParser" $ do
    let p :: Monad m => FormParser LoginFields m Text
        p = withCheck @"username" notEmpty
          $ subParser @"username"
          $ subParser @"password"
          $ field' @"remember_me"
    it "accesses fields correctly" $ do
      let txt = String "Foo"
          input = object
            [ "username" .= object
              [ "password" .= object
                [ "remember_me" .= txt
                ]
              ]
            ]
      r <- runForm p input finalize
      r `shouldBe` rSuccess txt
    it "reports correct failure when field is missing" $ do
      let input = object
            [ "username" .= object
              [ "password" .= object []
              ]
            ]
      r <- runForm p input finalize
      r `shouldBe` rParseError "username.password.remember_me"
                               "key \"remember_me\" not present"
    it "reports correct field path on parse failure" $ do
      let input = object
            [ "username" .= object
              [ "password" .= object
                [ "remember_me" .= Bool True
                ]
              ]
            ]
      r <- runForm p input finalize
      r `shouldBe` rParseError "username.password.remember_me"
                               "expected Text, encountered Boolean"
    it "reports correct field path on validation failure" $ do
      let input = object
            [ "username" .= object
              [ "password" .= object
                [ "remember_me" .= String ""
                ]
              ]
            ]
      r <- runForm p input finalize
      r `shouldBe` rFieldErrors [("username", "This field cannot be empty.")]
  describe "Forma (older test suite)" $ do
    context "when a parse error happens" $
      it "it's reported immediately" $ do
        let input = object
              [ "username"    .= (1 :: Int)
              , "password"    .= (2 :: Int)
              , "remember_me" .= True ]
        r <- runForm loginForm input $ \_ ->
          return (FormResultSuccess ())
        r `shouldBe` object
          [ "parse_error"  .= object
            [ "field" .= ("username" :: Text)
            , "message" .= ("expected Text, encountered Number" :: Text)
            ]
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
              let e0 = mkFieldError (nes $ pick @"username" @LoginFields) msg0
                  e1 = mkFieldError (nes $ pick @"password" @LoginFields) msg1
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
    context "for withCheck being used in SignupForm example" $ do
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
        it "the validation added with withCheck reports that passwords don't match" $ do
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
          r <- runForm signupForm input $ \SignupForm {..} ->
            return (FormResultSuccess ( signupUsername <> signupPassword ))
          r `shouldBe` object
            [ "parse_error"  .= Null
            , "field_errors" .= object []
            , "result"       .= String "Bobabc" ]

----------------------------------------------------------------------------
-- Helpers

rParseError :: Text -> String -> Value
rParseError fld msg = object
  [ "parse_error" .= object
    [ "field" .= fld
    , "message" .= msg
    ]
  , "field_errors" .= object []
  , "result" .= Null
  ]

rFieldErrors :: [(Text, Text)] -> Value
rFieldErrors xs = object
  [ "parse_error" .= Null
  , "field_errors" .= object (fmap f xs)
  , "result" .= Null
  ]
  where
    f (x, y) = x .= y

rSuccess :: Value -> Value
rSuccess r = object
  [ "parse_error"  .= Null
  , "field_errors" .= object []
  , "result"       .= r
  ]

finalize :: Monad m => a -> m (FormResult names a)
finalize = return . FormResultSuccess

nes :: a -> NonEmpty a
nes x = x :| []
