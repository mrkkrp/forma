{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Monad.Except
import Data.Aeson
import Data.Semigroup ((<>))
import Data.Text (Text)
import Test.Hspec
import Web.Forma
import Web.Forma.StructuredErrors
import qualified Data.Map.Strict as M
import qualified Data.Text       as T

type LoginFields = '["username", "password", "remember_me"]

data LoginForm = LoginForm
  { loginUsername   :: Text
  , loginPassword   :: Text
  , loginRememberMe :: Bool
  } deriving (Eq, Show)

loginForm :: Monad m => FormParser LoginFields Text m LoginForm
loginForm = LoginForm
  <$> field #username notEmpty
  <*> field #password notEmpty
  <*> (empty <|> field' #remember_me <|> pure True)

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
  <$> field #username notEmpty
  <*> withCheck #password_confirmation passwordsMatch
        ((,) <$> field #password notEmpty
             <*> field #password_confirmation notEmpty)

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
          r = ParsingFailed Nothing "Foo baz."
      toJSON r `shouldBe` object
        [ "parse_error" .= object
          [ "field"   .= Null
          , "message" .= ("Foo baz." :: Text)
          ]
        , "field_errors" .= object []
        , "result" .= Null
        ]
    it "ParsingFailed case (2)" $ do
      let r :: FormResult LoginFields Text Text
          r = ParsingFailed (pure #username) "Foo foo."
      toJSON r `shouldBe` object
        [ "parse_error" .= object
          [ "field" .= ("username" :: Text)
          , "message" .= ("Foo foo." :: Text)
          ]
        , "field_errors" .= object []
        , "result" .= Null
        ]
    it "ParsingFailed case (3)" $ do
      let r :: FormResult LoginFields Text Text
          r = ParsingFailed (pure (#username <> #password <> #remember_me))
                            "Foo bar."
      toJSON r `shouldBe` object
        [ "parse_error" .= object
          [ "field" .= ("username.password.remember_me" :: Text)
          , "message" .= ("Foo bar." :: Text)
          ]
        , "field_errors" .= object []
        , "result" .= Null
        ]
    it "ValidationFailed" $ do
      let r :: FormResult LoginFields Text Text
          r = ValidationFailed (M.singleton #username msg)
          msg = "Something" :: Text
      toJSON r `shouldBe` object
        [ "parse_error" .= Null
        , "field_errors" .= object
          [ "username" .= ("Something" :: Text)
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
        r <- runForm text (String input)
        r `shouldBe` Succeeded input
    context "when the data is not convertable" $
      it "fails to parse" $ do
        let input = Bool True
        r <- runForm text input
        r `shouldBe` ParsingFailed Nothing "expected Text, encountered Boolean"
  describe "subParser" $ do
    let p :: Monad m => FormParser LoginFields Text m Text
        p = withCheck #username notEmpty
          $ subParser #username
          $ subParser #password
          $ field' #remember_me
    it "accesses fields correctly" $ do
      let txt = "Foo"
          input = object
            [ "username" .= object
              [ "password" .= object
                [ "remember_me" .= String txt
                ]
              ]
            ]
      r <- runForm p input
      r `shouldBe` Succeeded txt
    it "reports correct failure when field is missing" $ do
      let input = object
            [ "username" .= object
              [ "password" .= object []
              ]
            ]
      r <- runForm p input
      r `shouldBe` ParsingFailed (pure (#username <> #password <> #remember_me))
                                 "key \"remember_me\" not present"
    it "reports correct field path on parse failure" $ do
      let input = object
            [ "username" .= object
              [ "password" .= object
                [ "remember_me" .= Bool True
                ]
              ]
            ]
      r <- runForm p input
      r `shouldBe` ParsingFailed (pure (#username <> #password <> #remember_me))
                                 "expected Text, encountered Boolean"
    it "reports correct field path on validation failure" $ do
      let input = object
            [ "username" .= object
              [ "password" .= object
                [ "remember_me" .= String ""
                ]
              ]
            ]
          msg = "This field cannot be empty."
      r <- runForm p input
      r `shouldBe` ValidationFailed (M.singleton #username msg)
  describe "Forma (older test suite)" $ do
    context "when a parse error happens" $
      it "it's reported immediately" $ do
        let input = object
              [ "username"    .= (1 :: Int)
              , "password"    .= (2 :: Int)
              , "remember_me" .= True ]
        r <- runForm loginForm input
        r `shouldBe` ParsingFailed (pure #username)
          "expected Text, encountered Number"
    context "when no parse error happens" $ do
      context "when validation errors happen" $
        it "all of them are reported" $ do
          let input = object
                [ "username"    .= String ""
                , "password"    .= String ""
                , "remember_me" .= True ]
              msg = "This field cannot be empty."
          r <- runForm loginForm input
          r `shouldBe` ValidationFailed (M.fromList
            [ (#username, msg)
            , (#password, msg)
            ])
      context "when no validation errors happens" $
        it "the parsing succeeds" $ do
          let input = object
                [ "username"    .= String "foo"
                , "password"    .= String "bar"
                , "remember_me" .= True ]
          r <- runForm loginForm input
          r `shouldBe` Succeeded LoginForm
            { loginUsername = "foo"
            , loginPassword = "bar"
            , loginRememberMe = True
            }
    context "for withCheck being used in SignupForm example" $ do
      context "when both password fields are empty" $
        it "we get errors for both empty password fields" $ do
          let input = object
                [ "username"    .= String ""
                , "password"    .= String ""
                , "password_confirmation" .= String "" ]
              msg = "This field cannot be empty." :: Text
          r <- runForm signupForm input
          r `shouldBe` ValidationFailed (M.fromList
            [ (#username, msg)
            , (#password, msg)
            , (#password_confirmation, msg)
            ])
      context "when both password fields contain values that don't match" $
        it "the validation added with withCheck reports that passwords don't match" $ do
          let input = object
                [ "username"    .= String ""
                , "password"    .= String "abc"
                , "password_confirmation" .= String "def" ]
          r <- runForm signupForm input
          r `shouldBe` ValidationFailed (M.fromList
            [ (#username, "This field cannot be empty.")
            , (#password_confirmation, "Passwords don't match!")
            ])
      context "when username and both password fields are filled in correctly" $
        it "it validates and returns the correct value" $ do
          let input = object
                [ "username"    .= String "Bob"
                , "password"    .= String "abc"
                , "password_confirmation" .= String "abc" ]
          r <- runForm signupForm input
          r `shouldBe` Succeeded SignupForm
            { signupUsername = "Bob"
            , signupPassword = "abc"
            }
  describe "Web.Forma.StructuredErrors" $ do
    it "ParsingFailed case (1)" $ do
      let r :: StructuredErrors LoginFields Text Text
          r = StructuredErrors $ ParsingFailed Nothing "Foo baz."
      toJSON r `shouldBe` toJSON ("Foo baz." :: Text)
    it "ParsingFailed case (2)" $ do
      let r :: StructuredErrors LoginFields Text Text
          r = StructuredErrors $ ParsingFailed (pure #username) "Foo foo."
      toJSON r `shouldBe` object
        [ "username" .= ("Foo foo." :: Text)
        ]
    it "ParsingFailed case (3)" $ do
      let r :: StructuredErrors LoginFields Text Text
          r = StructuredErrors $ ParsingFailed
                (pure (#username <> #password <> #remember_me)) "Foo bar."
      toJSON r `shouldBe` object
        [ "username" .= object
          [ "password" .= object
            [ "remember_me" .= ("Foo bar." :: Text)
            ]
          ]
        ]
    it "ValidationFailed (1)" $ do
      let r :: StructuredErrors LoginFields Text Text
          r = StructuredErrors $ ValidationFailed (M.singleton #username msg)
          msg = "Something" :: Text
      toJSON r `shouldBe` object
        [ "username" .= ("Something" :: Text)
        ]
    it "ValidationFailed (2)" $ do
      let r :: StructuredErrors LoginFields Text Text
          r = StructuredErrors $ ValidationFailed (M.fromList
                                                   [ (#username, msg0)
                                                   , (#username <> #password, msg1)
                                                   ]
                                                  )
          msg0 = "Something" :: Text
          msg1 = "Invalid" :: Text
      toJSON r `shouldBe` object
        [ "username" .= object
          [ "password" .= msg1
          ]
        ]
    it "Succeeded" $ do
      let r :: StructuredErrors LoginFields Text Text
          r = StructuredErrors $ Succeeded "Yeah"
      toJSON r `shouldBe` toJSON ("Yeah" :: Text)
