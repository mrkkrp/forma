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
  } deriving (Eq, Show)

loginForm :: Monad m => FormParser LoginFields Text m LoginForm
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
  } deriving (Eq, Show)

signupForm :: Monad m => FormParser SignupFields Text m SignupForm
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
  describe "ToJSON FormResult" $ do
    it "ParsingFailed case (1)" $ do
      let r :: FormResult LoginFields Text Text
          r = ParsingFailed [] "Foo baz."
      toJSON r `shouldBe` object
        [ "parse_error" .= ("Foo baz." :: Text)
        , "field_errors" .= object []
        , "result" .= Null
        ]
    it "ParsingFailed case (2)" $ do
      let r :: FormResult LoginFields Text Text
          r = ParsingFailed [pick @"username"] "Foo foo."
      toJSON r `shouldBe` object
        [ "parse_error" .= object
          [ "username" .= ("Foo foo." :: Text)
          ]
        , "field_errors" .= object []
        , "result" .= Null
        ]
    it "ParsingFailed case (3)" $ do
      let r :: FormResult LoginFields Text Text
          r = ParsingFailed
            [ pick @"username"
            , pick @"password"
            , pick @"remember_me"
            ] "Foo bar."
      toJSON r `shouldBe` object
        [ "parse_error" .= object
          [ "username" .= object
            [ "password" .= object
              [ "remember_me" .= ("Foo bar." :: Text)
              ]
            ]
          ]
        , "field_errors" .= object []
        , "result" .= Null
        ]
    it "ValidationFailed (1)" $ do
      let r :: FormResult LoginFields Text Text
          r = ValidationFailed e
          e = mkFieldError (nes $ pick @"username" @LoginFields) msg
          msg = "Something" :: Text
      toJSON r `shouldBe` object
        [ "parse_error" .= Null
        , "field_errors" .= object
          [ "username" .= ("Something" :: Text)
          ]
        , "result" .= Null
        ]
    it "ValidationFailed (2)" $ do
      let r :: FormResult LoginFields Text Text
          r = ValidationFailed (e0 <> e1)
          e0 = mkFieldError (pick @"username" :| [pick @"password"]) msg
          e1 = mkFieldError (pick @"username" :| [pick @"remember_me"]) msg
          msg = "Something" :: Text
      toJSON r `shouldBe` object
        [ "parse_error" .= Null
        , "field_errors" .= object
          [ "username" .= object
            [ "password" .= ("Something" :: Text)
            , "remember_me" .= ("Something" :: Text)
            ]
          ]
        , "result" .= Null
        ]
    it "ValidationFailed (3)" $ do
      let r :: FormResult LoginFields Text Text
          r = ValidationFailed (e0 <> e1)
          e0 = mkFieldError (nes $ pick @"username") msg
          e1 = mkFieldError (pick @"username" :| [pick @"remember_me"]) msg
          msg = "Something" :: Text
      toJSON r `shouldBe` object
        [ "parse_error" .= Null
        , "field_errors" .= object
          [ "username" .= object
            -- the validation error at deeper level `e1` has precedence
            [ "remember_me" .= ("Something" :: Text)
            ]
          ]
        , "result" .= Null
        ]
    it "Succeeded" $ do
      let r :: FormResult LoginFields Text Text
          r = Succeeded "Yeah"
      toJSON r `shouldBe` object
        [ "parse_error" .= Null
        , "field_errors" .= object []
        , "result" .= ("Yeah" :: Text)
        ]
  describe "value" $ do
    let text :: Monad m => FormParser names Text m Text
        text = value
    context "when the data is convertable" $
      it "succeeds" $ do
        let input = "Foo"
        r <- runForm text (String input) finalize
        r `shouldBe` Succeeded input
    context "when the data is not convertable" $
      it "fails to parse" $ do
        let input = Bool True
        r <- runForm text input finalize
        r `shouldBe` ParsingFailed [] "expected Text, encountered Boolean"
  describe "subParser" $ do
    let p :: Monad m => FormParser LoginFields Text m Text
        p = withCheck @"username" notEmpty
          $ subParser @"username"
          $ subParser @"password"
          $ field' @"remember_me"
    it "accesses fields correctly" $ do
      let txt = "Foo"
          input = object
            [ "username" .= object
              [ "password" .= object
                [ "remember_me" .= String txt
                ]
              ]
            ]
      r <- runForm p input finalize
      r `shouldBe` Succeeded txt
    it "reports correct failure when field is missing" $ do
      let input = object
            [ "username" .= object
              [ "password" .= object []
              ]
            ]
      r <- runForm p input finalize
      r `shouldBe` ParsingFailed
        [ pick @"username"
        , pick @"password"
        , pick @"remember_me"
        ] "key \"remember_me\" not present"
    it "reports correct field path on parse failure" $ do
      let input = object
            [ "username" .= object
              [ "password" .= object
                [ "remember_me" .= Bool True
                ]
              ]
            ]
      r <- runForm p input finalize
      r `shouldBe` ParsingFailed
        [ pick @"username"
        , pick @"password"
        , pick @"remember_me"
        ] "expected Text, encountered Boolean"
    it "reports correct field path on validation failure" $ do
      let input = object
            [ "username" .= object
              [ "password" .= object
                [ "remember_me" .= String ""
                ]
              ]
            ]
          msg = "This field cannot be empty."
          e0 = mkFieldError (nes $ pick @"username") msg
      r <- runForm p input finalize
      r `shouldBe` ValidationFailed e0
  describe "Forma (older test suite)" $ do
    context "when a parse error happens" $
      it "it's reported immediately" $ do
        let input = object
              [ "username"    .= (1 :: Int)
              , "password"    .= (2 :: Int)
              , "remember_me" .= True ]
        r <- runForm loginForm input finalize
        r `shouldBe` ParsingFailed [pick @"username"]
          "expected Text, encountered Number"
    context "when no parse error happens" $ do
      context "when no validation errors happen in 1 step" $ do
        context "when callback reports success" $
          it "correct resulting value is returned" $ do
            let input = object
                  [ "username"    .= String "Bob"
                  , "password"    .= String "123" ]
            r <- runForm loginForm input $ \LoginForm {..} -> do
              loginRememberMe `shouldBe` True
              return (Right (loginUsername <> loginPassword))
            r `shouldBe` Succeeded "Bob123"
        context "when callback reports validation errors" $
          it "correct resulting value is returned" $ do
            let input = object
                  [ "username"    .= String "Bob"
                  , "password"    .= String "123"
                  , "remember_me" .= True ]
                msg0 = "I don't like this username."
                msg1 = "I don't like this password."
                e0 = mkFieldError (nes $ pick @"username" @LoginFields) msg0
                e1 = mkFieldError (nes $ pick @"password" @LoginFields) msg1
            r <- runForm loginForm input $ \_ ->
              return (Left (e0 <> e1) :: Either (FieldError LoginFields Text) LoginForm)
            r `shouldBe` ValidationFailed (e0 <> e1)
      context "when validation errors happen in 1 step" $
        it "all of them are reported" $ do
          let input = object
                [ "username"    .= String ""
                , "password"    .= String ""
                , "remember_me" .= True ]
              msg = "This field cannot be empty."
              e0 = mkFieldError (nes $ pick @"username") msg
              e1 = mkFieldError (nes $ pick @"password") msg
          r <- runForm loginForm input finalize
          r `shouldBe` ValidationFailed (e0 <> e1)
    context "for withCheck being used in SignupForm example" $ do
      context "when both password fields are empty" $
        it "we get errors for both empty password fields" $ do
          let input = object
                [ "username"    .= String ""
                , "password"    .= String ""
                , "password_confirmation" .= String "" ]
              msg = "This field cannot be empty." :: Text
              e0 = mkFieldError (nes $ pick @"username") msg
              e1 = mkFieldError (nes $ pick @"password") msg
              e2 = mkFieldError (nes $ pick @"password_confirmation") msg
          r <- runForm signupForm input finalize
          r `shouldBe` ValidationFailed (e0 <> e1 <> e2)
      context "when both password fields contain values that don't match" $
        it "the validation added with withCheck reports that passwords don't match" $ do
          let input = object
                [ "username"    .= String ""
                , "password"    .= String "abc"
                , "password_confirmation" .= String "def" ]
              e0 = mkFieldError (nes $ pick @"username")
                ("This field cannot be empty." :: Text)
              e1 = mkFieldError (nes $ pick @"password_confirmation")
                ("Passwords don't match!" :: Text)
          r <- runForm signupForm input finalize
          r `shouldBe` ValidationFailed (e0 <> e1)
      context "when username and both password fields are filled in correctly" $
        it "it validates and returns the correct value" $ do
          let input = object
                [ "username"    .= String "Bob"
                , "password"    .= String "abc"
                , "password_confirmation" .= String "abc" ]
          r <- runForm signupForm input $ \SignupForm {..} ->
            return (Right ( signupUsername <> signupPassword ))
          r `shouldBe` Succeeded "Bobabc"

----------------------------------------------------------------------------
-- Helpers

finalize :: Monad m => a -> m (Either (FieldError names Text) a)
finalize = return . Right

nes :: a -> NonEmpty a
nes x = x :| []
