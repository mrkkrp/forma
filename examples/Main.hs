{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import           Control.Monad        (forM_)
import           Control.Monad.Except
import           Data.Aeson
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NE
import qualified Data.Map.Strict      as M
import           Data.Monoid          ((<>))
import           Data.String          (fromString)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Web.Forma

type LoginFields = '["username", "password", "remember_me"]

data LoginForm = LoginForm
  { loginUsername   :: Text
  , loginPassword   :: Text
  , loginRememberMe :: Bool
  } deriving Show

loginForm :: Monad m => FormParser LoginFields m LoginForm
loginForm = LoginForm
  <$> field @"username" notEmpty
  <*> field @"password" notEmpty
  <*> field' @"remember_me"

notEmpty :: Monad m => Text -> ExceptT Text m Text
notEmpty txt =
  if T.null txt
    then throwError "This field cannot be empty."
    else return txt

myInput :: Value
myInput = object
  [ "username"    .= ("Bob" :: Text)
  , "password"    .= ("123" :: Text)
  , "remember_me" .= True
  ]

invalidInput :: Value
invalidInput = object
  [ "username"    .= ("Bob" :: Text)
  , "remember_me" .= True
  ]


main :: IO ()
main = do
  r <- runForm loginForm myInput $ \LoginForm {..} -> do
    print loginUsername
    print loginPassword
    print loginRememberMe
    return $ FormResultSuccess $ String "Success"
  print $ toJSON (toResponse r)

  -- success
  r' <- runForm loginForm myInput $ return . FormResultSuccess
  printResult r'

  -- parsing error
  r'' <- runForm loginForm invalidInput $ return . FormResultSuccess
  printResult r''

  -- validation error
  r''' <- runForm loginForm myInput $ \LoginForm {..} -> do
    let msg = String "I don't like this username"
        e = mkFieldError (nes $ pick @"username" @LoginFields) msg
    return $ FormResultError e
  printResult (r''' :: BranchState LoginFields Text)

printResult :: Show a => BranchState names a -> IO ()
printResult r =
  case r of
    ParsingFailed path err ->
      T.putStrLn $ "Parse error: " <> err <> " at " <> showFieldPath path
    ValidationFailed (FieldError errs) ->
      forM_ (M.toAscList errs) $ \(path, err) ->
        T.putStrLn $ "Validation error: " <> showErr err <> " at " <>
          showFieldPath (NE.toList path)
      where showErr err = case err of
              String e -> e
              _ -> fromString $ show err
    Succeeded result -> do
      T.putStr "Success: "
      print result

nes :: a -> NonEmpty a
nes x = x :| []
