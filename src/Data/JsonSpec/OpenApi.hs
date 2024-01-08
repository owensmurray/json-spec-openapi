{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  Schemaable,
  EncodingSchema(..),
  DecodingSchema(..),
) where


import Control.Lens (At(at), (&), over, set)
import Data.Aeson (ToJSON(toJSON))
import Data.Functor.Identity (Identity(runIdentity))
import Data.JsonSpec (HasJsonDecodingSpec(DecodingSpec),
  HasJsonEncodingSpec(EncodingSpec), Specification(JsonArray, JsonBool,
  JsonDateTime, JsonEither, JsonInt, JsonLet, JsonNullable, JsonNum,
  JsonObject, JsonRef, JsonString, JsonTag))
import Data.OpenApi (AdditionalProperties(AdditionalPropertiesAllowed),
  HasAdditionalProperties(additionalProperties), HasEnum(enum_),
  HasFormat(format), HasItems(items), HasOneOf(oneOf),
  HasProperties(properties), HasRequired(required), HasType(type_),
  NamedSchema(NamedSchema), OpenApiItems(OpenApiItemsObject),
  OpenApiType(OpenApiArray, OpenApiBoolean, OpenApiInteger, OpenApiNull,
  OpenApiNumber, OpenApiObject, OpenApiString), Reference(Reference),
  Referenced(Inline, Ref), ToSchema(declareNamedSchema), Definitions,
  Schema)
import Data.OpenApi.Declare (DeclareT(runDeclareT), MonadDeclare(declare))
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Typeable (Proxy(Proxy), Typeable)
import GHC.TypeError (ErrorMessage((:$$:), (:<>:)), Unsatisfiable,
  unsatisfiable)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Prelude (Applicative(pure), Bool(False), Functor(fmap), Maybe(Just,
  Nothing), Monoid(mempty), ($), (.))
import qualified Data.HashMap.Strict.InsOrd as HMI
import qualified GHC.TypeError as TE


{-|
  Convert a 'Specification' into an OpenApi 'Schema'. The type class
  'Schemaable' is an internal and opaque implementation detail and not
  something you should have to worry about.

  It should already have an instance for every 'Specification' that can
  be turned into a 'Schema'. If it does not, then that is a bug. Please
  report it! :-)

  The limitations of this function are:

  * It behaves in a possibly unexpected way when given a top level schema
    of the form:

      > JsonLet '[
      >   '("foo", ...)
      > ] (
      >   JsonRef "foo"
      > )

      'toOpenApiSchema' returns a 'Schema', not a @'Referenced' 'Schema'@.
      Therefore, if the \"top level\" of the 'Specification' is a 'JsonRef',
      then we will try to dereference and inline the referenced schema. In
      other words,

      > toOpenApiSchema (Proxy @(
      >     JsonLet
      >       '[ '("foo", JsonString) ]
      >       (JsonRef "foo")
      >   ))

      will behave as if you had called

      > toOpenApiSchema (Proxy @(
      >     JsonLet
      >       '[ '("foo", JsonString) ]
      >       JsonString
      >   ))

      However, if the reference is undefined, then you will get a custom
      type error explaining what the problem is.

  * With the exception of the above point, we do not check to make sure
    that every referenced used in the returned 'Schema' actually contains
    a definition. So for instance this will \"work\":

      > let
      >   (defs, schema) =
      >     toOpenApiSchema
      >       (Proxy @(
      >         JsonObject '[
      >           ("bar", JsonRef "not-defined")
      >         ]
      >       ))
      > in
      >   ...

      This will compile, and will not throw any runtime errors directly,
      but depending on how you /use/ @defs@ and @schema@ (like, for
      instance, generating an OpenApi specification) you will probably
      encounter a runtime error complaining that "not-defined" hasn't
      been defined.
-}
toOpenApiSchema
  :: (Schemaable spec)
  => Proxy (spec :: Specification)
  -> (Definitions Schema, Schema)
toOpenApiSchema spec =
  runIdentity (runDeclareT (schemaable spec) mempty)


{-|
  Specifications that can be turned into OpenApi Schemas or a reference
  to a schema.
-}
class Refable (spec :: Specification) where
  refable
    :: (MonadDeclare (Definitions Schema) m)
    => Proxy spec
    -> m (Referenced Schema)
instance (KnownSymbol name) => Refable (JsonRef name) where
  refable Proxy =
    pure (ref (sym @name))
instance
    ( Defs defs
    , KnownSymbol name
    )
  =>
    Refable (JsonLet defs (JsonRef name))
  where
    refable Proxy = do
      mkDefs (Proxy @defs)
      refable (Proxy @(JsonRef name))
instance {-# overlaps #-} (Schemaable a) => Refable a where
  refable = fmap Inline . schemaable


{-|
  Specifications that can be turned into OpenApi 'Schema's.

  This is intended to be an opaque implementation detail. The only
  reason it is exported is because there are some cases where you might
  need to be able to spell this constraint in code that builds off of
  this package.
-}
class Schemaable (spec :: Specification) where
  schemaable
    :: (MonadDeclare (Definitions Schema) m)
    => Proxy spec
    -> m Schema
instance (KnownSymbol tag) => Schemaable ('JsonTag tag) where
  schemaable Proxy =
    pure $
      mempty & set enum_ (Just [toJSON (sym @tag :: Text)])
instance Schemaable 'JsonString where
  schemaable Proxy =
    pure $
      mempty & set type_ (Just OpenApiString)
instance {- Schemaable ('JsonEither left right) -}
    ( Schemaable left
    , Schemaable right
    )
  =>
    Schemaable ('JsonEither left right)
  where
    schemaable Proxy = do
      schemaLeft <- schemaable (Proxy @left)
      schemaRight <- schemaable (Proxy @right)
      pure $
        mempty
          & set oneOf (Just
              [ Inline schemaLeft
              , Inline schemaRight
              ]
          )
instance Schemaable 'JsonNum where
  schemaable Proxy =
    pure $
      mempty & set type_ (Just OpenApiNumber)
instance Schemaable 'JsonInt where
  schemaable Proxy =
    pure $
      mempty & set type_ (Just OpenApiInteger)
instance Schemaable ('JsonObject '[]) where
  schemaable Proxy =
    pure $
      mempty
        & set type_ (Just OpenApiObject)
        & set additionalProperties (Just (AdditionalPropertiesAllowed False))
instance {- Schemaable ('JsonObject ( '(key, spec) : more )) -}
    ( Schemaable ('JsonObject more)
    , Refable spec
    , KnownSymbol key
    )
  =>
    Schemaable ('JsonObject ( '(key, spec) : more ))
  where
    schemaable Proxy = do
      let
        propertyName :: Text
        propertyName = sym @key

      propertySchema <- refable (Proxy @spec)
      more <- schemaable (Proxy @('JsonObject more))
      pure $
        more
          & over
              properties
              (set (at propertyName) (Just propertySchema))
          & over required (propertyName:)
instance (Schemaable spec) => Schemaable ('JsonArray spec) where
  schemaable Proxy = do
    elementSchema <- schemaable (Proxy @spec)
    pure $
      mempty
        & set type_ (Just OpenApiArray)
        & set items (Just (OpenApiItemsObject (Inline elementSchema)))
instance Schemaable 'JsonBool where
  schemaable Proxy =
    pure $
      mempty & set type_ (Just OpenApiBoolean)
instance Schemaable 'JsonDateTime where
  schemaable Proxy =
    pure $
      mempty
        & set type_ (Just OpenApiString)
        & set format (Just "date-time")
instance {- Undefined Let -}
    Unsatisfiable (
      T "`JsonRef \"" :<>: T target :<>: T "\"` is not defined.\n"
      :$$: T "You are trying to use a JsonRef as the \"top level\" "
      :$$: T "schema. We try to satisfy this request by looking up "
      :$$: T "the reference and inlining it.  However in this case you "
      :$$: T "are trying to reference a schema which is not defined, "
      :$$: T "so this won't work.\n"
    )
  =>
    Schemaable (JsonLet '[] (JsonRef target))
  where
    schemaable = unsatisfiable
instance {- Schemaable (JsonLet ( '(target, def) ': more) (JsonRef target)) -}
    {-# overlaps #-}
    ( KnownSymbol target
    , Schemaable def
    , Schemaable (JsonLet more def)
    )
  =>
    Schemaable (JsonLet ( '(target, def) ': more) (JsonRef target))
  where
    schemaable Proxy = do
      defSchema <- schemaable (Proxy @def)
      declare (HMI.singleton (sym @target) defSchema)
      schemaable (Proxy @(JsonLet more def))
instance {- Schemaable (JsonLet ( '(name, def) ': more) (JsonRef target)) -}
    {-# overlaps #-}
    ( KnownSymbol name
    , Schemaable def
    , Schemaable (JsonLet more (JsonRef target))
    )
  =>
    Schemaable (JsonLet ( '(name, def) ': more) (JsonRef target))
  where
    schemaable Proxy = do
      defSchema <- schemaable (Proxy @def)
      declare (HMI.singleton (sym @name) defSchema)
      schemaable (Proxy @(JsonLet more (JsonRef target)))
instance {- Schemaable (JsonLet defs spec) -}
    {-# overlaps #-}
    ( Defs defs
    , Schemaable spec
    )
  =>
    Schemaable (JsonLet defs spec)
  where
    schemaable Proxy = do
      mkDefs (Proxy @defs)
      schemaable (Proxy @spec)
instance (Schemaable spec) => Schemaable (JsonNullable spec) where
  schemaable Proxy = do
    schema <- schemaable (Proxy @spec)
    pure $
      mempty
        & set oneOf (Just
            [ Inline (mempty & set type_ (Just OpenApiNull))
            , Inline schema
            ]
        )


{-| Go through and make a declaration for each item in a JsonLet.  -}
class Defs (defs :: [(Symbol, Specification)]) where
  mkDefs
    :: (MonadDeclare (Definitions Schema) m)
    => Proxy defs
    -> m ()
instance Defs '[] where
  mkDefs Proxy = pure ()
instance
    ( Defs more
    , Schemaable spec
    , KnownSymbol name
    )
  =>
    Defs ( '(name, spec) ': more)
  where
    mkDefs Proxy = do
      schema <- schemaable (Proxy @spec)
      declare (HMI.singleton (sym @name) schema)
      mkDefs (Proxy @more)


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
instance
    ( Schemaable (EncodingSpec a)
    , Typeable a
    )
  =>
    ToSchema (EncodingSchema a)
  where
    declareNamedSchema _ = do
      let (declarations, schema) = toOpenApiSchema (Proxy @(EncodingSpec a))
      declare declarations
      pure (NamedSchema Nothing schema)


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
instance
    ( Schemaable (DecodingSpec a)
    , Typeable a
    )
  =>
    ToSchema (DecodingSchema a)
  where
    declareNamedSchema _ = do
      let (declarations, schema) = toOpenApiSchema (Proxy @(DecodingSpec a))
      declare declarations
      pure (NamedSchema Nothing schema)


{- | Shorthand for demoting type-level strings.  -}
sym
  :: forall a b.
     ( IsString b
     , KnownSymbol a
     )
  => b
sym = fromString $ symbolVal (Proxy @a)


ref :: Text -> Referenced a
ref = Ref . Reference


{-| Shorthand for building custom type errors.  -}
type T (msg :: Symbol) = TE.Text msg


