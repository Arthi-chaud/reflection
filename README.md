# Reflection

## Introduction

Reflection is a technique commonly used in high-level languages like Java, Haskell, JavaScript and C++ to observe (at runtime) information about a value, such as its type, its member functions (in the case of OOP) or its fields. It can also be used to modify values at runtime.

In some languages, like Java, reflection is costly. In others, especially in strongly-typed ones, the compiler is able to optimise operations that use reflection so that the reflection objects (i.e. objects that reflect a given value, e.g. that gives the list of its member functions, the name of its constructor, etc.) have little to no memory footprint at runtime. This is the case for the `Generic` typeclass in Haskell: the compiler is able to determine a reflection object from a value's type, and so is able to partially evaluate simple operations that uses them (e.g. NFData, Functor, etc.).

For example: the `rnf` function, which evaluates a value to normal form, performs equally well whether it uses `Generics` or an equivalent hand-written implementation. The same applies for the `fmap` function
When these functions are dervied using `Generic`, they usually have the form `f = to . genOp . from`, and GHC is able to inline the composition, evaluate the expression, and end up with a compuation without any trace of the reflection objects 

However, in some cases, computations may use reflection in a way that is opaque to the compiler.
In Aeson, we can derive serialisation methods using `Generics`. The flow is the following:
- Using a reflection object, produce a computation that turns a value of type `a` into a `Value` (an ADT that represents JSON values)
- Then turns that value into a `ByteString`.

GHC cannot optimise this operation because it relies on runtime values.
The same goes for deserialisation:
- Parses a `ByteString` into a `Value`
- Using a reflection object, turn a `Value` into a value of type `a` 

In both cases, the reflection object is used to produce a function which will not be inlined by GHC. Thus, in both cases, the `Value` object will have a memory footprint and any conversion from/to such object will have an impact on the program's performance.

We propose a way to remove that memory footprint by executing computations that use reflection objects at compile time, and compile them so that intermediary types like `Value` do not exist at runtime.

## Approach

### Lifting 

We could define a way to lift the computation that relies on reflection:

```haskell

data Type = TypeVariable Name | ADT [Constructor]
data Constructor = RecordCons Name [(Name, Type)] | ADTCons Name [Type]

observeValueType :: 
    -- | The name of the variable to observe
    Name -> 
    -- | The function that uses refection. Produces an expression
    (Type -> Q Exp) -> 
    Q Exp

toJSON :: Tree -> String
toJSON tree = $(observeValueType 'tree f)
    where f :: (Type -> Q Expr)
```

### Add a user-defined DSL layer

Using Template Haskell's types directly might be a bit overwelming to programmers. Moreover, if we focus on type definitions, we don't need access to the API in the `observeType`'s second argument.

We could add an `EDSL` layer, allowing us to split the responsability between the use of the reflection object and the interpretation of the computation that uses it.

```haskell

class EDSL a where
    optimise :: a -> a 
    compile :: a -> Q Exp -- Produces a Haskell expression from AST 

-- Observes the type of a value. The handling of the reflection object/Type is done at compile type
observeValueType :: (MonadQ m, EDSL expr) => Name -> (Type -> m expr) -> Q Expr
observeValueType n f = TH.reify >>= (fmap optimise . f . toType >=> compile)
    where toType :: TH.Type -> Type

-- Alternative that allows observing a type without passing a value of that type
observeType :: forall a m expr. (MonadQ m, EDSL expr) => (Type -> m expr) -> Q Expr
observeType = observeValueType (undefined :: a)
```

## Example

### JSON Serialisation

```haskell
-- From the previously developped library
data Template = Token Char | And Template Template | Hole | Empty

data JSON
class SerialisationFormat format where
    objectTemplate :: [(FieldName, Template)] -> Template
instance SerialisationFormat JSON where
    objectTemplate = ... -- Use implementation from futadata

class Serialisable format a where
    builder :: a -> Builder
---

instance Serialisable JSON (Tree a) where
    builder tree = $(genBuilder 'tree)

data TemplateForVar = TFV Name Template 
-- We need this to let the 'compile' function know the name of the variable to generate the serialiser for

instance EDSL TemplateForVar where
    optimise = .. -- Collates adjacent tokens, as in the previous library
    compile = .. -- Generates the expression, should be very close to the code from the previous library

genBuilder :: Name -> Q Expr
genBuilder name = observeValueType name (return . TemplateForVar name . objectTemplate)
```

### JSON Parsing

```haskell
-- Naive implementation of a parser. We could use (mega)parsec
newtype Parser m a => { runParser :: String -> m a }

data JSONParserAST = 
    Object [(String, JSONParserAST)] 
    | Number 
    | String 
    | Nullable JSONParserAST 
    | Null

-- an AST for a parser 
data ParserAST a


instance EDSL (ParserAST a) where
    optimise = id
    -- Turn AST into an expression with type (Parser m a)
    compile = todo
    
class FromJSON a where
    parse :: (MonadFail m) => String -> m a

instance FromJSON (Tree a) where
    parse s = runParser $(genParser @(Tree a)) s

--  For all a, generates a function with type `String -> m a`
genParser :: forall a. Q Exp
genParser = observeType @a (return . jsonASTToParserAST . tyToJSONParserAST) 
    where 
        tyToJSONParserAST :: Type -> JSONParserAST
        jsonASTToParserAST :: JSONParserAST -> ParserAST
```

## Additional notes

- TODO: Find a project name
    - Something to do with reflection, mirrors, 
    - We avoid reflection, so the light does not bounce on a reflective surface
