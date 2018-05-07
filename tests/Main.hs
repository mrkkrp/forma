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
import qualified Data.Text           as T

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

fieldEmptyMsg :: Text
fieldEmptyMsg = "This field cannot be empty."

notEmpty :: Monad m => Text -> ExceptT Text m Text
notEmpty txt =
  if T.null txt
    then throwError fieldEmptyMsg
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

passwordsDontMatchMsg :: Text
passwordsDontMatchMsg = "Passwords don't match!"

passwordsMatch :: Monad m => (Text, Text) -> ExceptT Text m Text
passwordsMatch (a,b) =
  if a == b
    then return a
    else throwError passwordsDontMatchMsg

main :: IO ()
main = hspec spec

type PlayerFields = '["name", "coords", "x", "y"]

data PlayerForm = PlayerForm
  { playerName :: String
  , playerCoords :: CoordsForm
  }

data CoordsForm = CoordsForm
  { coordsX :: Float
  , coordsY :: Float
  }

instance Show CoordsForm where
  show CoordsForm {..} = "{" ++ show coordsX ++ ", " ++ show coordsY ++ "}"

coords :: Monad m => FormParser PlayerFields m CoordsForm
coords = CoordsForm
  <$> field' @"x"
  <*> field' @"y"

player :: Monad m => FormParser PlayerFields m PlayerForm
player = PlayerForm
  <$> field' @"name"
  <*> subParser @"coords" coords

spec :: Spec
spec = do
  describe "value" $ do
    let text :: Monad m => FormParser n m Text
        text = value
    context "when the data is convertable" $
      it "succeeds" $ do
        let input = String "Foo"
        r <- runForm text input finalize
        toJSON (toResponse r) `shouldBe` rSuccess input
    context "when the data is not convertable" $
      it "fails to parse" $ do
        let input = Bool True
        r <- runForm text input finalize
        toJSON (toResponse r) `shouldBe` rParseError
          "" "expected Text, encountered Boolean"

  describe "subParser" $ do
    let p :: Monad m => FormParser LoginFields m Text
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
    it "accesses nested object correctly" $ do
      let input = object
            [ "name" .= String "Fanny"
            , "coords" .= object
              [ "x" .= (1.1 :: Float)
              , "y" .= (25 :: Float)
              ]
            ]
      r <- runForm player input $ \PlayerForm {..} ->
        return $ FormResultSuccess (playerName ++ " at " ++ show playerCoords)
      r `shouldBe` Succeeded "Fanny at {1.1, 25.0}"
    it "reports correct failure when field is missing" $ do
      let input = object
            [ "username" .= object
              [ "password" .= object []
              ]
            ]
      r <- runForm p input finalize
      r `shouldBe` ParsingFailed
                    [ pick @"username" , pick @"password" , pick @"remember_me"]
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
      r `shouldBe` ParsingFailed
                    [ pick @"username" , pick @"password" , pick @"remember_me"]
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
      r `shouldBe` ValidationFailed (mkFieldError (nes $ pick @"username") $
                      String fieldEmptyMsg)

  describe "Forma (older test suite)" $ do
    context "when a parse error happens" $
      it "it's reported immediately" $ do
        let input = object
              [ "username"    .= (1 :: Int)
              , "password"    .= (2 :: Int)
              , "remember_me" .= True ]
        r <- runForm loginForm input $ \_ ->
          return (FormResultSuccess ())
        r `shouldBe` ParsingFailed
                      [ pick @"username" ] "expected Text, encountered Number"
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
            r `shouldBe` Succeeded "Bob123"
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
            r `shouldBe` ValidationFailed
               (mkFieldError (nes $ pick @"username") (String msg0)
               <> mkFieldError (nes $ pick @"password") (String msg1))
      context "when validation errors happen in 1 step" $
        it "all of them are reported" $ do
          let input = object
                [ "username"    .= String ""
                , "password"    .= String ""
                , "remember_me" .= True ]
          r <- runForm loginForm input $ \_ ->
            return (FormResultSuccess ())
          r `shouldBe` ValidationFailed
              (mkFieldError (nes $ pick @"username") (String fieldEmptyMsg)
              <> mkFieldError (nes $ pick @"password") (String fieldEmptyMsg))
    context "for withCheck being used in SignupForm example" $ do
      context "when both password fields are empty" $
        it "we get errors for both empty password fields" $ do
          let input = object
                [ "username"    .= String ""
                , "password"    .= String ""
                , "password_confirmation" .= String "" ]
          r <- runForm signupForm input $ \_ ->
            return (FormResultSuccess ())
          r `shouldBe` ValidationFailed
              (mkFieldError (nes $ pick @"username") (String fieldEmptyMsg)
              <> mkFieldError (nes $ pick @"password") (String fieldEmptyMsg)
              <> mkFieldError (nes $ pick @"password_confirmation") (String fieldEmptyMsg))
      context "when both password fields contain values that don't match" $
        it "the validation added with withCheck reports that passwords don't match" $ do
          let input = object
                [ "username"    .= String ""
                , "password"    .= String "abc"
                , "password_confirmation" .= String "def" ]
          r <- runForm signupForm input $ \_ ->
            return (FormResultSuccess ())
          r `shouldBe` ValidationFailed
              (mkFieldError (nes $ pick @"username") (String fieldEmptyMsg)
              <> mkFieldError (nes $ pick @"password_confirmation") (String passwordsDontMatchMsg))
      context "when username and both password fields are filled in correctly" $
        it "it validates and returns the correct value" $ do
          let input = object
                [ "username"    .= String "Bob"
                , "password"    .= String "abc"
                , "password_confirmation" .= String "abc" ]
          r <- runForm signupForm input $ \SignupForm {..} ->
            return (FormResultSuccess ( signupUsername <> signupPassword ))
          r `shouldBe` Succeeded "Bobabc"

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
