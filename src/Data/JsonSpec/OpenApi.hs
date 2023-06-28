{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
  This module provides tools for integrating the type-level JSON
  'Specification' with the "openapi" package.

  You can use 'toOpenApiSchema' as a low-level tool to transform @json-spec@
  'Specification's into @openapi3@ 'Schema's directly, irrespective of any
  particular business data type.

  More likely you will want to use @-XDerivingVia@ along with 'EncodingSchema'
  or 'DecodingSchema' to derive 'ToSchema' instances for your data types.

  Example, given this data type:

  > data User = User
  >   {      name :: Text
  >   , lastLogin :: UTCTime
  >   }
  >   deriving ToSchema via (EncodingSchema User) -- <-- ToSchema instance defined here
  > instance HasJsonEncodingSpec User where
  >   type EncodingSpec User =
  >     JsonObject
  >       '[ '("name", JsonString)
  >        , '("last-login", JsonDateTime)
  >        ]
  >   toJSONStructure user =
  >     (Field @"name" (name user),
  >     (Field @"last-login" (lastLogin user),
  >     ()))

  Calling @'Data.Aeson.encode' ('Data.OpenApi3.toSchema' ('Proxy' :: 'Proxy' User))@
  will produce the following Schema:

  > {
  >   "additionalProperties": false,
  >   "properties": {
  >     "last-login": {
  >       "format": "date-time",
  >       "type": "string"
  >     },
  >     "name": {
  >       "type": "string"
  >     }
  >   },
  >   "required": [
  >     "name",
  >     "last-login"
  >   ],
  >   "type": "object"
  > }

  If you needed more control over the content of the schema you might also
  consider doing something like this, e.g. in the case where you would like to
  allow additional properties:

  > data User = User
  >   {      name :: Text
  >   , lastLogin :: UTCTime
  >   }
  > instance HasJsonEncodingSpec User where
  >   type EncodingSpec User =
  >     JsonObject
  >       '[ '("name", JsonString)
  >        , '("last-login", JsonDateTime)
  >        ]
  >   toJSONStructure user =
  >     (Field @"name" (name user),
  >     (Field @"last-login" (lastLogin user),
  >     ()))
  > instance ToSchema User where
  >   declareNamedSchema _proxy =
  >       pure $
  >         NamedSchema
  >           Nothing
  >           (
  >             toOpenApiSchema (EncodingSpec User)
  >               & set
  >                   additionalProperties
  >                   (Just (AdditionalPropertiesAllowed True))
  >           )

-}
module Data.JsonSpec.OpenApi (
  toOpenApiSchema,
  EncodingSchema(..),
  DecodingSchema(..),
) where


import Control.Lens (At(at), (&), over, set)
import Data.JsonSpec (HasJsonDecodingSpec(DecodingSpec),
  HasJsonEncodingSpec(EncodingSpec), Specification(JsonArray, JsonBool,
  JsonDateTime, JsonInt, JsonNum, JsonObject, JsonString))
import Data.OpenApi (AdditionalProperties(AdditionalPropertiesAllowed),
  HasAdditionalProperties(additionalProperties),
  HasFormat(format), HasItems(items), HasProperties(properties),
  HasRequired(required), HasType(type_), NamedSchema(NamedSchema),
  OpenApiItems(OpenApiItemsObject), OpenApiType(OpenApiArray,
  OpenApiBoolean, OpenApiInteger, OpenApiNumber, OpenApiObject,
  OpenApiString), Referenced(Inline), ToSchema(declareNamedSchema),
  Schema)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Typeable (Proxy(Proxy), Typeable)
import GHC.TypeLits (KnownSymbol, symbolVal)


{-|
  Convert a 'Specification' into an OpenAPI 'Schema'. The type class
  'Internal' is an internal and opaque implementation detail and not
  something you should have to worry about. It /should/ already have an
  instance for every possible 'Specification' that can be constructed. If
  it does not, then that is a bug. Please report it! :-)
-}
toOpenApiSchema
  :: (Internal spec)
  => Proxy (spec :: Specification)
  -> Schema
toOpenApiSchema = internal


class Internal (spec :: Specification) where
  {-|
    Given a `Specification`, produce an OpenApi schema equivalent to
    the specification. Usually you will want to use this in conjunction with
    `HasJsonEncodingSpec` or `HasJsonDecodingSpec`.

    Example:

    > data MyType = ...
    > instance HasJsonEncodingSpec MyType where
    >   type EncodingSpec MyType = ...
    >
    > schema :: Schema
    > schema = toOpenApiSchema (Proxy :: Proxy (EncodingSpec MyType))

  -}
  internal :: Proxy spec -> Schema
instance Internal 'JsonString where
  internal _ =
    mempty & set type_ (Just OpenApiString)
instance Internal 'JsonNum where
  internal _ =
    mempty & set type_ (Just OpenApiNumber)
instance Internal 'JsonInt where
  internal _ =
    mempty & set type_ (Just OpenApiInteger)
instance Internal ('JsonObject '[]) where
  internal _ =
    mempty
      & set type_ (Just OpenApiObject)
      & set additionalProperties (Just (AdditionalPropertiesAllowed False))
instance (KnownSymbol key, Internal spec, Internal ('JsonObject more)) => Internal ('JsonObject ( '(key, spec) : more )) where
  internal _ =
    let
      propertyName :: Text
      propertyName = sym @key

      propertySchema :: Schema
      propertySchema = internal (Proxy @spec)
    in
      internal (Proxy @('JsonObject more))
        & over properties (set (at propertyName) (Just (Inline propertySchema)))
        & over required (propertyName:)
instance (Internal spec) => Internal ('JsonArray spec) where
  internal _ =
    let
      elementSchema :: Schema
      elementSchema = internal (Proxy @spec)
    in
      mempty
        & set type_ (Just OpenApiArray)
        & set items (Just (OpenApiItemsObject (Inline elementSchema)))
instance Internal 'JsonBool where
  internal _ =
    mempty & set type_ (Just OpenApiBoolean)

instance Internal 'JsonDateTime where
  internal _ =
    mempty
      & set type_ (Just OpenApiString)
      & set format (Just "date-time")


{-|
  Helper for defining `ToSchema` instances based on `HasJsonEncodingSpec`
  using @deriving via@.

  Example:

  > data MyType = ...
  >   deriving ToSchema via (EncodingSchema MyType)
  > instance HasJsonEncodingSchema MyType where
  >   ...
-}
newtype EncodingSchema a =
  EncodingSchema {unEncodingSchema :: a}
instance (Typeable a, Internal (EncodingSpec a)) => ToSchema (EncodingSchema a) where
  declareNamedSchema _ =
    pure (NamedSchema Nothing (toOpenApiSchema (Proxy @(EncodingSpec a))))


{-|
  Helper for defining `ToSchema` instances based on `HasJsonDecodingSpec`
  using @deriving via@.
  
  Example:

  > data MyType = ...
  >   deriving ToSchema via (DecodingSchema MyType)
  > instance HasJsonDecodingSchema MyType where
  >   ...
-}
newtype DecodingSchema a =
  DecodingSchema {unDecodingSchema :: a}
instance (Typeable a, Internal (DecodingSpec a)) => ToSchema (DecodingSchema a) where
  declareNamedSchema _ =
    pure (NamedSchema Nothing (toOpenApiSchema (Proxy @(DecodingSpec a))))


{- | Shorthand for demoting type-level strings.  -}
sym
  :: forall a b.
     ( IsString b
     , KnownSymbol a
     )
  => b
sym = fromString $ symbolVal (Proxy @a)


