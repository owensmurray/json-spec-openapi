{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Lens (At(at), (&), set)
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.JsonSpec (Field(Field), HasJsonDecodingSpec(DecodingSpec,
  fromJSONStructure), HasJsonEncodingSpec(EncodingSpec, toJSONStructure),
  SpecJSON(SpecJSON), Specification(JsonArray, JsonBool, JsonDateTime,
  JsonNum, JsonObject, JsonString))
import Data.JsonSpec.OpenApi (EncodingSchema, toOpenApiSchema)
import Data.OpenApi (ToSchema)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Time (UTCTime)
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified Data.OpenApi as OA


main :: IO ()
main =
  hspec $ do
    describe "toOpenApiSchema" $ do
      it "string" $
        let
          actual :: OA.Schema
          actual =
            toOpenApiSchema (Proxy @JsonString)

          expected :: OA.Schema
          expected = stringSchema
        in
          actual `shouldBe` expected

      it "object" $
        let
          actual :: OA.Schema
          actual =
            toOpenApiSchema (Proxy @(
              JsonObject '[
                '("Foo", JsonString),
                '("Bar", JsonString)
              ]
            ))

          expected :: OA.Schema
          expected =
            mempty
              & set OA.type_ (Just OA.OpenApiObject)
              & set OA.properties (
                  mempty
                    & set (at "Foo") (Just (OA.Inline stringSchema))
                    & set (at "Bar") (Just (OA.Inline stringSchema))
                )
              & set OA.required ["Foo", "Bar"]
              & set
                  OA.additionalProperties
                  (Just (OA.AdditionalPropertiesAllowed False))
        in
          actual `shouldBe` expected

      it "num" $
        let
          actual :: OA.Schema
          actual =
            toOpenApiSchema (Proxy @JsonNum)

          expected :: OA.Schema
          expected = numSchema
        in
          actual `shouldBe` expected

      it "complex array" $
        let
          actual :: OA.Schema
          actual =
            toOpenApiSchema (Proxy @(
              JsonArray (
                JsonObject '[
                  '("Foo", JsonString),
                  '("Bar", JsonString)
                ]
              )
            ))

          expected :: OA.Schema
          expected =
            let
              elementSchema :: OA.Schema
              elementSchema =
                mempty
                  & set OA.type_ (Just OA.OpenApiObject)
                  & set OA.properties (
                      mempty
                        & set (at "Foo") (Just (OA.Inline stringSchema))
                        & set (at "Bar") (Just (OA.Inline stringSchema))
                    )
                  & set OA.required ["Foo", "Bar"]
                  & set
                      OA.additionalProperties
                      (Just (OA.AdditionalPropertiesAllowed False))
            in
              mempty
                & set OA.type_ (Just OA.OpenApiArray)
                & set OA.items (Just (OA.OpenApiItemsObject (OA.Inline elementSchema)))

        in
          actual `shouldBe` expected

      it "bool" $
        let
          actual :: OA.Schema
          actual =
            toOpenApiSchema (Proxy @JsonBool)

          expected :: OA.Schema
          expected = boolSchema
        in
          actual `shouldBe` expected

      it "date-time" $
        let
          actual :: OA.Schema
          actual = 
            toOpenApiSchema (Proxy @JsonDateTime)

          expected :: OA.Schema
          expected =
            stringSchema
            & set OA.format (Just "date-time")
        in
          actual `shouldBe` expected
    describe "EncodingSpec" $
      it "works" $
        let
          actual :: OA.Schema
          actual = OA.toSchema (Proxy @User)

          expected :: OA.Schema
          expected =
            mempty
              & set OA.type_ (Just OA.OpenApiObject)
              & set OA.properties (
                  mempty
                    & set (at "name") (Just (OA.Inline stringSchema))
                    & set (at "last-login") (Just (OA.Inline dateSchema))
                )
              & set OA.required ["name", "last-login"]
              & set
                  OA.additionalProperties
                  (Just (OA.AdditionalPropertiesAllowed False))

        in
          encode actual `shouldBe` encode expected


data User = User
  {      name :: Text
  , lastLogin :: UTCTime
  }
  deriving stock (Show, Eq)
  deriving ToSchema via (EncodingSchema User)
  deriving (ToJSON, FromJSON) via (SpecJSON User)
instance HasJsonEncodingSpec User where
  type EncodingSpec User =
    JsonObject
      '[ '("name", JsonString)
       , '("last-login", JsonDateTime)
       ]
  toJSONStructure user =
    (Field @"name" (name user),
    (Field @"last-login" (lastLogin user),
    ()))
instance HasJsonDecodingSpec User where
  type DecodingSpec User = EncodingSpec User
  fromJSONStructure
      (Field @"name" name,
      (Field @"last-login" lastLogin,
      ()))
    =
      pure User { name , lastLogin }


stringSchema :: OA.Schema
stringSchema = mempty & set OA.type_ (Just OA.OpenApiString)


numSchema :: OA.Schema
numSchema = mempty & set OA.type_ (Just OA.OpenApiNumber)


boolSchema :: OA.Schema
boolSchema = mempty & set OA.type_ (Just OA.OpenApiBoolean)


dateSchema :: OA.Schema
dateSchema =
  mempty
    & set OA.type_ (Just OA.OpenApiString)
    & set OA.format (Just "date-time")


