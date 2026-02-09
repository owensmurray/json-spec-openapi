{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Lens (At(at), (&), set)
import Data.Aeson (ToJSON(toJSON), FromJSON)
import Data.JsonSpec
  ( Field(Field), FieldSpec(Optional, Required)
  , HasJsonDecodingSpec(DecodingSpec, fromJSONStructure)
  , HasJsonEncodingSpec(EncodingSpec, toJSONStructure), SpecJSON(SpecJSON)
  , Specification
    ( JsonAnnotated, JsonArray, JsonBool, JsonDateTime, JsonEither, JsonInt
    , JsonLet, JsonNullable, JsonNum, JsonObject, JsonRaw, JsonRef, JsonString
    , JsonTag
    )
  , (:::), (::?), unField
  )
import Data.JsonSpec.OpenApi
  ( EncodingSchema, Rename, SchemaModifier(modifySchema), toOpenApiSchema
  )
import Data.OpenApi (Definitions, ToSchema)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Time (UTCTime)
import Prelude
  ( Applicative(pure), Bool(False), Eq, Functor(fmap), Int, IO, Maybe(Just)
  , Monoid(mempty), Show, ($), (.)
  )
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict.InsOrd as HMI
import qualified Data.OpenApi as OA


main :: IO ()
main =
  hspec $ do
    describe "toOpenApiSchema" $ do
      it "string" $
        let
          actual :: (Definitions OA.Schema, OA.Schema)
          actual = toOpenApiSchema (Proxy @JsonString)

          expected :: (Definitions OA.Schema, OA.Schema)
          expected = (mempty, stringSchema)
        in
          actual `shouldBe` expected

      it "sum" $
        let
          actual :: (Definitions OA.Schema, OA.Schema)
          actual =
            toOpenApiSchema (Proxy @(
              JsonEither
                '[ JsonObject
                    '[ Required "tag" (JsonTag "a")
                     , Required "content" JsonString
                     ]
                , JsonObject
                    '[ Required "tag" (JsonTag "b")
                     , Required "content" JsonString
                     ]
                ]
            ))

          expected :: (Definitions OA.Schema, OA.Schema)
          expected =
            ( mempty
            , mempty
                & set OA.oneOf (Just
                  [ OA.Inline $
                      mempty
                        & set OA.type_ (Just OA.OpenApiObject)
                        & set OA.properties (
                            mempty
                              & set (at "tag") (Just (OA.Inline (
                                  mempty
                                    & set OA.enum_ (Just [toJSON ("a" :: Text)])
                                )))
                              & set (at "content") (Just (
                                  OA.Inline stringSchema
                                ))
                          )
                        & set OA.required ["tag", "content"]
                        & set
                            OA.additionalProperties
                            (Just (OA.AdditionalPropertiesAllowed False))
                  , OA.Inline $
                      mempty
                        & set OA.type_ (Just OA.OpenApiObject)
                        & set OA.properties (
                            mempty
                              & set (at "tag") (Just (OA.Inline (
                                  mempty
                                    & set OA.enum_ (Just [toJSON ("b" :: Text)])
                                )))
                              & set (at "content") (Just (
                                  OA.Inline stringSchema
                                ))
                          )
                        & set OA.required ["tag", "content"]
                        & set
                            OA.additionalProperties
                            (Just (OA.AdditionalPropertiesAllowed False))
                  ]
                )
            )
        in
          actual `shouldBe` expected

      it "object" $
        let
          actual :: (Definitions OA.Schema, OA.Schema)
          actual =
            toOpenApiSchema (Proxy @(
              JsonObject '[
                Required "Foo" JsonString,
                Required "Bar" JsonString
              ]
            ))

          expected :: (Definitions OA.Schema, OA.Schema)
          expected =
            ( mempty
            , mempty
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
            )
        in
          actual `shouldBe` expected

      it "raw" $
        let
          actual :: (Definitions OA.Schema, OA.Schema)
          actual = toOpenApiSchema (Proxy @JsonRaw)

          expected :: (Definitions OA.Schema, OA.Schema)
          expected =
            ( mempty
            , mempty & set OA.type_ (Just OA.OpenApiObject)
            )
        in
          actual `shouldBe` expected

      it "num" $
        let
          actual :: (Definitions OA.Schema, OA.Schema)
          actual = toOpenApiSchema (Proxy @JsonNum)

          expected :: (Definitions OA.Schema, OA.Schema)
          expected = (mempty, numSchema)
        in
          actual `shouldBe` expected

      it "complex array" $
        let
          actual :: (Definitions OA.Schema, OA.Schema)
          actual =
            toOpenApiSchema (Proxy @(
              JsonArray (
                JsonObject '[
                  Required "Foo" JsonString,
                  Required "Bar" JsonString
                ]
              )
            ))

          expected :: (Definitions OA.Schema, OA.Schema)
          expected =
            ( mempty
            , let
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
                  & set OA.items (Just (
                      OA.OpenApiItemsObject (OA.Inline elementSchema)
                    ))
            )

        in
          actual `shouldBe` expected

      it "bool" $
        let
          actual :: (Definitions OA.Schema, OA.Schema)
          actual =
            toOpenApiSchema (Proxy @JsonBool)

          expected :: (Definitions OA.Schema, OA.Schema)
          expected = (mempty, boolSchema)
        in
          actual `shouldBe` expected

      it "nullable" $
        let
          actual :: (Definitions OA.Schema, OA.Schema)
          actual =
            toOpenApiSchema (Proxy @(JsonNullable JsonInt))

          expected :: (Definitions OA.Schema, OA.Schema)
          expected =
            ( mempty
            , mempty
                & set OA.oneOf (Just
                  [ OA.Inline $
                      mempty & set OA.type_ (Just OA.OpenApiNull)
                  , OA.Inline $
                      mempty & set OA.type_ (Just OA.OpenApiInteger)
                  ]
                )
            )
        in
          Aeson.encode actual `shouldBe` Aeson.encode expected

      it "optional" $
        let
          actual :: (Definitions OA.Schema, OA.Schema)
          actual =
            toOpenApiSchema (Proxy @(
              JsonObject '[
                Required "foo" JsonString,
                Optional "bar" JsonString
              ]
            ))

          expected :: (Definitions OA.Schema, OA.Schema)
          expected =
            ( mempty
            , mempty
                & set OA.type_ (Just OA.OpenApiObject)
                & set OA.properties (
                    mempty
                      & set (at "foo") (Just (OA.Inline stringSchema))
                      & set (at "bar") (Just (OA.Inline stringSchema))
                  )
                & set OA.required ["foo"]
                & set
                    OA.additionalProperties
                    (Just (OA.AdditionalPropertiesAllowed False))
            )
        in
          Aeson.encode actual `shouldBe` Aeson.encode expected

      it "date-time" $
        let
          actual :: (Definitions OA.Schema, OA.Schema)
          actual =
            toOpenApiSchema (Proxy @JsonDateTime)

          expected :: (Definitions OA.Schema, OA.Schema)
          expected =
            ( mempty
            , stringSchema & set OA.format (Just "date-time")
            )
        in
          actual `shouldBe` expected

      describe "let bindings" $ do
        it "basic let expression" $
          let
            actual :: (Definitions OA.Schema, OA.Schema)
            actual =
              toOpenApiSchema (Proxy @(
                JsonLet '[
                  '("thing", JsonString)
                ]
                (
                  JsonObject '[
                    Required "foo" (JsonRef "thing")
                  ]
                )
              ))

            expected :: (Definitions OA.Schema, OA.Schema)
            expected =
              ( HMI.singleton "thing" stringSchema
              , mempty
                  & set OA.type_ (Just OA.OpenApiObject)
                  & set OA.properties (
                      mempty
                        & set (at "foo") (Just (OA.Ref (OA.Reference "thing")))
                    )
                  & set OA.required ["foo"]
                  & set
                      OA.additionalProperties
                      (Just (OA.AdditionalPropertiesAllowed False))
              )
          in
            actual `shouldBe` expected

        it "back reference" $
          let
            actual :: (Definitions OA.Schema, OA.Schema)
            actual =
              toOpenApiSchema (Proxy @(
                JsonLet
                 '[ '( "thing1"
                     , JsonString
                     )
                  , '( "thing2"
                     , JsonRef "thing1"
                     )
                  ]
                  (
                    JsonRef "thing2"
                  )
              ))

            expected :: (Definitions OA.Schema, OA.Schema)
            expected =
              ( HMI.fromList
                  [ ("thing1", stringSchema)
                  , ("thing2", stringSchema)
                  ]
              , stringSchema
              )
          in
            actual `shouldBe` expected

      it "top level reference" $
        let
          actual :: (Definitions OA.Schema, OA.Schema)
          actual =
            toOpenApiSchema (Proxy @(
              JsonLet '[
                '("foo", JsonNum),
                '("bar", JsonString)
              ]
              (JsonRef "bar")
            ))

          expected :: (Definitions OA.Schema, OA.Schema)
          expected =
            ( HMI.fromList [("foo", numSchema), ("bar", stringSchema)]
            , stringSchema
            )
        in
          actual `shouldBe` expected

      it "lower level reference" $
        let
          actual :: (Definitions OA.Schema, OA.Schema)
          actual =
            toOpenApiSchema (Proxy @(
              JsonObject '[
                Required "foo" (
                  JsonLet
                    '[ '("thing", JsonString)]
                    (JsonRef "thing")
                )
              ]
            ))

          expected :: (Definitions OA.Schema, OA.Schema)
          expected =
            ( HMI.singleton "thing" stringSchema
            , mempty
                & set OA.type_ (Just OA.OpenApiObject)
                & set OA.properties (
                    mempty
                      & set (at "foo") (Just (OA.Ref (OA.Reference "thing")))
                  )
                & set OA.required ["foo"]
                & set
                    OA.additionalProperties
                    (Just (OA.AdditionalPropertiesAllowed False))
            )
        in
          Aeson.encode actual `shouldBe` Aeson.encode expected

      it "Mutual recursion" $
        let
          barSchema :: OA.Schema
          barSchema =
            mempty
              & set OA.type_ (Just OA.OpenApiObject)
              & set
                  OA.additionalProperties
                  (Just (OA.AdditionalPropertiesAllowed False))
              & set
                  OA.properties
                  (
                    HMI.fromList
                      [ ( "recbar"
                        , OA.Inline (
                            mempty
                              & set OA.type_ (Just OA.OpenApiArray)
                              & set
                                  OA.items
                                  (
                                    Just
                                    . OA.OpenApiItemsObject
                                    . OA.Ref
                                    . OA.Reference
                                    $ "foo"
                                  )
                          )
                        )
                      , ( "valbar"
                        , OA.Inline stringSchema
                        )
                      ]
                  )
              & set OA.required ["recbar", "valbar"]

          expected :: (Definitions OA.Schema, OA.Schema)
          expected =
            ( HMI.fromList
                [ ( "foo"
                  , mempty
                      & set OA.type_ (Just OA.OpenApiObject)
                      & set
                          OA.additionalProperties
                          (Just (OA.AdditionalPropertiesAllowed False))
                      & set
                          OA.properties
                          (
                            HMI.fromList
                              [ ( "recfoo"
                                , OA.Inline (
                                    mempty
                                      & set OA.type_ (Just OA.OpenApiArray)
                                      & set
                                          OA.items
                                          (
                                            Just
                                            . OA.OpenApiItemsObject
                                            . OA.Ref
                                            . OA.Reference
                                            $ "bar"
                                          )
                                  )
                                )
                              , ( "valfoo"
                                , OA.Inline (
                                    mempty
                                      & set OA.type_ (Just OA.OpenApiInteger)
                                  )
                                )
                              ]
                          )
                      & set OA.required ["recfoo", "valfoo"]
                  )
                , ( "bar"
                  , barSchema
                  )
                ]
            , barSchema
            )

          actual :: (Definitions OA.Schema, OA.Schema)
          actual =
            toOpenApiSchema (Proxy @(
              JsonLet
                '[ '( "foo"
                    , JsonObject
                        '[ "recfoo" ::: JsonArray (JsonRef "bar")
                         , "valfoo" ::: JsonInt
                         ]
                    )
                 , '( "bar"
                    , JsonObject
                        '[ "recbar" ::: JsonArray (JsonRef "foo")
                         , "valbar" ::: JsonString
                         ]
                    )
                 ]
                 (JsonRef "bar")
            ))
        in
          Aeson.encode actual `shouldBe` Aeson.encode expected

      it "Nested name conflict" $
        let
          expected :: (Definitions OA.Schema, OA.Schema)
          expected =
            ( mempty
                & set (at "foo.1") (Just (stringSchema))
                & set (at "foo") (Just (stringSchema))
            , stringSchema
            )

          actual :: (Definitions OA.Schema, OA.Schema)
          actual =
            toOpenApiSchema (Proxy @(Rename (
              JsonLet
                '[ '("foo"
                    , JsonLet
                        '[ '( "foo"
                            , JsonString
                            )
                         ]
                         (JsonRef "foo")
                    )
                 ]
                 (JsonRef "foo")
            )))
        in
          Aeson.encode actual `shouldBe` Aeson.encode expected

      it "Nested name conflict 2" $
        let
          expectedFooSchema :: OA.Schema
          expectedFooSchema =
            mempty
              & set OA.type_ (Just OA.OpenApiObject)
              & set
                  OA.additionalProperties
                  (Just (OA.AdditionalPropertiesAllowed False))
              & set OA.properties (
                  mempty
                    & set (at "field1") (Just (
                        OA.Ref (OA.Reference "foo.1")
                      ))
                    & set (at "field2") (Just (
                        OA.Ref (OA.Reference "bar")
                      ))
                )

          expected :: (Definitions OA.Schema, OA.Schema)
          expected =
            ( mempty
                & set (at "bar") (Just stringSchema)
                & set (at "foo.1") (Just stringSchema)
                & set (at "foo") (Just expectedFooSchema)
            , expectedFooSchema
            )

          actual :: (Definitions OA.Schema, OA.Schema)
          actual =
            toOpenApiSchema (Proxy @(Rename (
              JsonLet
                '[ '("foo"
                    , JsonLet
                        '[ '( "foo"
                            , JsonString
                            )
                         ]
                         (
                           JsonObject
                             '[ ("field1" ::? JsonRef "foo")
                              , ("field2" ::? JsonRef "bar")
                              ]
                         )
                    )
                 , '( "bar"
                    , JsonString
                    )
                 ]
                 (JsonRef "foo")
            )))
        in
          Aeson.encode actual `shouldBe` Aeson.encode expected

      it "Nested name conflict 3" $
        let
          expected :: (Definitions OA.Schema, OA.Schema)
          expected =
            ( mempty
                & set (at "foo.1") (Just stringSchema)
                & set (at "foo.2") (Just (
                    mempty & set OA.type_ (Just OA.OpenApiInteger)
                  ))
                & set (at "foo.3") (Just (
                    mempty & set OA.type_ (Just OA.OpenApiBoolean)
                  ))
                & set (at "foo") (Just (
                    mempty
                      & set OA.type_ (Just OA.OpenApiObject)
                      & set OA.properties (HMI.fromList
                          [ ("field1", OA.Ref (OA.Reference "foo.1"))
                          , ("field2", OA.Ref (OA.Reference "foo.2"))
                          , ("field3", OA.Ref (OA.Reference "foo.3"))
                          ]
                        )
                      & set OA.additionalProperties (Just (
                          OA.AdditionalPropertiesAllowed False
                        ))
                      & set OA.required ["field1", "field2", "field3"]
                  ))
                & set (at "foo.4") (Just (
                    mempty
                      & set OA.oneOf (
                          Just
                            [ OA.Inline $
                                mempty & set OA.type_ (Just OA.OpenApiNull)
                            , OA.Inline $ stringSchema
                            ]
                        )
                  ))
            , mempty
                & set OA.type_ (Just OA.OpenApiObject)
                & set OA.properties (HMI.fromList
                    [ ("field1", OA.Ref (OA.Reference "foo"))
                    , ("field2", OA.Ref (OA.Reference "foo.4"))
                    ]
                  )
                & set OA.required ["field1", "field2"]
                & set OA.additionalProperties (Just (
                    OA.AdditionalPropertiesAllowed False
                  ))
            )

          actual :: (Definitions OA.Schema, OA.Schema)
          actual =
            toOpenApiSchema (Proxy @(Rename (
              JsonLet
                '[ '( "foo"
                    , JsonLet
                        '[ '("foo", JsonString) ]
                         (
                           JsonObject
                            '[ "field1" ::: JsonRef "foo"
                             , "field2" ::: JsonLet
                                             '[ '("foo", JsonInt) ]
                                              (JsonRef "foo")
                             , "field3" ::: JsonLet
                                             '[ '("foo", JsonBool) ]
                                              (JsonRef "foo")
                             ]

                         )
                    )
                 ]
                 (
                   JsonObject
                     '[ "field1" ::: JsonRef "foo"
                      , "field2" ::: JsonLet
                                      '[ '("foo", JsonNullable JsonString) ]
                                       (JsonRef "foo")
                      ]
                 )
            )))
        in
          Aeson.encode actual `shouldBe` Aeson.encode expected

    describe "annotated" $ do
      it "JsonAnnotated wrapping object (EncodingSpec AnnotatedUser)" $
        let
          actual :: (Definitions OA.Schema, OA.Schema)
          actual = toOpenApiSchema (Proxy @(EncodingSpec AnnotatedUser))

          expected :: (Definitions OA.Schema, OA.Schema)
          expected =
            ( mempty
            , mempty
                & set OA.type_ (Just OA.OpenApiObject)
                & set OA.description (Just "A user with a name and age")
                & set OA.properties (
                    mempty
                      & set (at "name") (Just (OA.Inline stringSchema))
                      & set (at "age") (Just (OA.Inline intSchema))
                  )
                & set OA.required ["name", "age"]
                & set
                    OA.additionalProperties
                    (Just (OA.AdditionalPropertiesAllowed False))
            )
        in
          actual `shouldBe` expected

      it "JsonAnnotated with empty list is no-op" $
        let
          actual :: (Definitions OA.Schema, OA.Schema)
          actual = toOpenApiSchema (Proxy @(EncodingSpec EmptyAnnotatedUser))

          expected :: (Definitions OA.Schema, OA.Schema)
          expected =
            ( mempty
            , mempty
                & set OA.type_ (Just OA.OpenApiObject)
                & set OA.properties (
                    mempty
                      & set (at "name") (Just (OA.Inline stringSchema))
                      & set (at "age") (Just (OA.Inline intSchema))
                  )
                & set OA.required ["name", "age"]
                & set
                    OA.additionalProperties
                    (Just (OA.AdditionalPropertiesAllowed False))
            )
        in
          actual `shouldBe` expected

      it "JsonAnnotated with Symbol-valued \"schema-modifier\" is ignored (no-op)" $
        let
          actual :: (Definitions OA.Schema, OA.Schema)
          actual = toOpenApiSchema (Proxy @(EncodingSpec SymbolSchemaModifierUser))

          expected :: (Definitions OA.Schema, OA.Schema)
          expected =
            ( mempty
            , mempty
                & set OA.type_ (Just OA.OpenApiObject)
                & set OA.properties (
                    mempty
                      & set (at "name") (Just (OA.Inline stringSchema))
                      & set (at "age") (Just (OA.Inline intSchema))
                  )
                & set OA.required ["name", "age"]
                & set
                    OA.additionalProperties
                    (Just (OA.AdditionalPropertiesAllowed False))
            )
        in
          actual `shouldBe` expected

      it "JsonAnnotated with Type-valued list but no \"schema-modifier\" key is no-op" $
        let
          actual :: (Definitions OA.Schema, OA.Schema)
          actual = toOpenApiSchema (Proxy @(EncodingSpec OtherTypeAnnotationUser))

          expected :: (Definitions OA.Schema, OA.Schema)
          expected =
            ( mempty
            , mempty
                & set OA.type_ (Just OA.OpenApiObject)
                & set OA.properties (
                    mempty
                      & set (at "name") (Just (OA.Inline stringSchema))
                      & set (at "age") (Just (OA.Inline intSchema))
                  )
                & set OA.required ["name", "age"]
                & set
                    OA.additionalProperties
                    (Just (OA.AdditionalPropertiesAllowed False))
            )
        in
          actual `shouldBe` expected

    describe "EncodingSchema" $
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
              & set OA.required ["name"]
              & set
                  OA.additionalProperties
                  (Just (OA.AdditionalPropertiesAllowed False))

        in
          Aeson.encode actual `shouldBe` Aeson.encode expected


{-
  This is the example used in the docs. If you update it, then update
  the docs as well.
-}
data User = User
  {      name :: Text
  , lastLogin :: Maybe UTCTime
  }
  deriving stock (Show, Eq)
  deriving ToSchema via (EncodingSchema User) -- <-- ToSchema instance defined here
  deriving (ToJSON, FromJSON) via (SpecJSON User)
instance HasJsonEncodingSpec User where
  type EncodingSpec User =
    JsonObject
      '[ Required "name" JsonString
       , Optional "last-login" JsonDateTime
       ]
  toJSONStructure user =
    (Field @"name" (name user),
    (fmap (Field @"last-login") (lastLogin user),
    ()))
instance HasJsonDecodingSpec User where
  type DecodingSpec User = EncodingSpec User
  fromJSONStructure
      (Field @"name" name,
      (fmap (unField @"last-login") ->  lastLogin,
      ()))
    =
      pure User { name , lastLogin }


{- Annotated test: EncodingSpec uses JsonAnnotated and SchemaModifier. -}
data AnnotatedUser = AnnotatedUser
  { auName :: Text
  ,  auAge :: Int
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON AnnotatedUser)
instance SchemaModifier AnnotatedUser where
  modifySchema schema =
    schema & set OA.description (Just "A user with a name and age")
instance HasJsonEncodingSpec AnnotatedUser where
  type EncodingSpec AnnotatedUser =
    JsonAnnotated
      '[ '("schema-modifier", AnnotatedUser)
       ]
      (JsonObject
        '[ Required "name" JsonString
         , Required "age" JsonInt
         ])
  toJSONStructure AnnotatedUser { auName, auAge } =
    (Field @"name" auName,
    (Field @"age" auAge,
    ()))
instance HasJsonDecodingSpec AnnotatedUser where
  type DecodingSpec AnnotatedUser = EncodingSpec AnnotatedUser
  fromJSONStructure
      (Field @"name" auName,
      (Field @"age" auAge,
      ()))
    =
      pure AnnotatedUser { auName, auAge }


{- Empty annotations: catch-all applies, no-op. -}
data EmptyAnnotatedUser = EmptyAnnotatedUser
  { eauName :: Text
  ,  eauAge :: Int
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON EmptyAnnotatedUser)
instance HasJsonEncodingSpec EmptyAnnotatedUser where
  type EncodingSpec EmptyAnnotatedUser =
    JsonAnnotated
      '[]
      (JsonObject
        '[ Required "name" JsonString
         , Required "age" JsonInt
         ])
  toJSONStructure EmptyAnnotatedUser { eauName, eauAge } =
    (Field @"name" eauName,
    (Field @"age" eauAge,
    ()))
instance HasJsonDecodingSpec EmptyAnnotatedUser where
  type DecodingSpec EmptyAnnotatedUser = EncodingSpec EmptyAnnotatedUser
  fromJSONStructure
      (Field @"name" eauName,
      (Field @"age" eauAge,
      ()))
    =
      pure EmptyAnnotatedUser { eauName, eauAge }


{- Symbol-valued "schema-modifier" key: kind is [(Symbol, Symbol)], no-op. -}
data SymbolSchemaModifierUser = SymbolSchemaModifierUser
  { ssmName :: Text
  ,  ssmAge :: Int
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON SymbolSchemaModifierUser)
instance HasJsonEncodingSpec SymbolSchemaModifierUser where
  type EncodingSpec SymbolSchemaModifierUser =
    JsonAnnotated
      '[ '("schema-modifier", "ignored-symbol-value")
       ]
      (JsonObject
        '[ Required "name" JsonString
         , Required "age" JsonInt
         ])
  toJSONStructure SymbolSchemaModifierUser { ssmName, ssmAge } =
    (Field @"name" ssmName,
    (Field @"age" ssmAge,
    ()))
instance HasJsonDecodingSpec SymbolSchemaModifierUser where
  type DecodingSpec SymbolSchemaModifierUser = EncodingSpec SymbolSchemaModifierUser
  fromJSONStructure
      (Field @"name" ssmName,
      (Field @"age" ssmAge,
      ()))
    =
      pure SymbolSchemaModifierUser { ssmName, ssmAge }


{- Type-valued annotations but key is not "schema-modifier": no-op. -}
data OtherAnnotation
data OtherTypeAnnotationUser = OtherTypeAnnotationUser
  { otaName :: Text
  ,  otaAge :: Int
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON OtherTypeAnnotationUser)
instance HasJsonEncodingSpec OtherTypeAnnotationUser where
  type EncodingSpec OtherTypeAnnotationUser =
    JsonAnnotated
      '[ '("other-key", OtherAnnotation)
       ]
      (JsonObject
        '[ Required "name" JsonString
         , Required "age" JsonInt
         ])
  toJSONStructure OtherTypeAnnotationUser { otaName, otaAge } =
    (Field @"name" otaName,
    (Field @"age" otaAge,
    ()))
instance HasJsonDecodingSpec OtherTypeAnnotationUser where
  type DecodingSpec OtherTypeAnnotationUser = EncodingSpec OtherTypeAnnotationUser
  fromJSONStructure
      (Field @"name" otaName,
      (Field @"age" otaAge,
      ()))
    =
      pure OtherTypeAnnotationUser { otaName, otaAge }


stringSchema :: OA.Schema
stringSchema = mempty & set OA.type_ (Just OA.OpenApiString)


numSchema :: OA.Schema
numSchema = mempty & set OA.type_ (Just OA.OpenApiNumber)


intSchema :: OA.Schema
intSchema = mempty & set OA.type_ (Just OA.OpenApiInteger)


boolSchema :: OA.Schema
boolSchema = mempty & set OA.type_ (Just OA.OpenApiBoolean)


dateSchema :: OA.Schema
dateSchema =
  mempty
    & set OA.type_ (Just OA.OpenApiString)
    & set OA.format (Just "date-time")


