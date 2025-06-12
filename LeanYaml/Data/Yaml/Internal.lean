-- Lean port of `Data.Yaml.Internal` (stub)


-- This file contains *placeholder* definitions that mirror the primary
-- data-types from the upstream Haskell module.  They allow us to
-- compile incrementally while we port functionality.
--
-- Some functions are still stubs; others (e.g. the `StringStyle`
-- helpers) now have working Lean implementations.

-- Import the scalar/event definitions shared with this module.
import LeanYaml.Data.Yaml.Event

namespace LeanYaml.Data.Yaml

/-- Core YAML value type (mirrors Aeson `Value`). -/
inductive Value : Type where
  | null : Value
  | bool : Bool → Value
  | number : Float → Value -- TODO: use Scientific type
  | string : String → Value
  | array  : Array Value → Value
  | object : List (String × Value) → Value
  deriving Repr, BEq, Inhabited

/-- Warnings that can arise while parsing YAML. -/
inductive Warning : Type where
  | duplicateKey : List String → Warning
  deriving Repr, BEq, Inhabited

-- NOTE: This comment block was superseded by the richer documentation
-- further below and is now removed to avoid confusion.
/-!
`ParseException` mirrors the rich error type from the original Haskell
`yaml` library.  At the moment we do **not** yet support all payloads
the upstream type carries; however, we provide the most useful
constructors together with a user-friendly pretty printer.  Additional
details can be added incrementally.
-/

inductive ParseException : Type where
  | nonScalarKey                                             -- ^ Map key was not a scalar value
  | unknownAlias      (anchorName : String)                  -- ^ Unknown YAML anchor
  | unexpectedEvent   (received : Option Event)
                      (expected : Option Event)
  | invalidYaml       (msg? : Option String)                 -- ^ Wrapped libyaml parse error (human text)
  | multipleDocuments
  | aesonException    (msg : String)
  | otherParseException (msg : String)
  | nonStringKey      (path : List String)
  | nonStringKeyAlias (anchorName : String) (value : Value)
  | cyclicIncludes
  | loadSettingsException (file : String) (inner : ParseException)
  deriving Repr, BEq, Inhabited

/-- Human-readable rendering of `ParseException`.  The implementation is
    *greatly simplified* compared to the Haskell version but already
    provides meaningful messages for debugging. -/
def prettyPrintParseException : ParseException → String
  | .nonScalarKey => "Non-scalar key in mapping"
  | .unknownAlias name => s!"Unknown alias '{name}'"
  | .unexpectedEvent received expected =>
      let r := match received with
        | some e => toString e
        | none   => "<none>"
      let exp := match expected with
        | some e => toString e
        | none   => "<none>"
      "Unexpected event – expected " ++ exp ++ ", got " ++ r
  | .invalidYaml none => "Invalid YAML (unspecified parser error)"
  | .invalidYaml (some msg) => "Invalid YAML: " ++ msg
  | .multipleDocuments => "Multiple YAML documents encountered"
  | .aesonException msg => "Aeson exception: " ++ msg
  | .otherParseException msg => "Parser exception: " ++ msg
  | .nonStringKey path =>
      "Non-string key at path: " ++ String.intercalate "." path
  | .nonStringKeyAlias an _ => "Non-string key alias: " ++ an
  | .cyclicIncludes => "Cyclic includes detected"
  | .loadSettingsException file inner =>
      "Could not parse file '" ++ file ++ "':\n" ++ prettyPrintParseException inner

instance : ToString ParseException where
  toString := prettyPrintParseException

/-!
### String style helpers

The original Haskell `yaml` library provides a number of helpers that
decide how textual values should be encoded when *emitting* YAML.  For
the early stages of this port we implement a **minimal** subset that is
already exercised by downstream code.  The behaviour is intentionally
simple and can be enhanced later without affecting the public API.
-/

open LeanYaml.Data.Yaml (Tag Style Event)

-- -------------------------------------------------------------------------

-- Helper predicates ------------------------------------------------------

private def specialStrings : List String :=
  [ "y", "Y", "yes", "Yes", "YES"
  , "n", "N", "no", "No", "NO"
  , "true", "True", "TRUE"
  , "false", "False", "FALSE"
  , "on", "On", "ON"
  , "off", "Off", "OFF"
  , "null", "Null", "NULL", "~", "*"
  ]

private def isNumeric (s : String) : Bool :=
  if s.isEmpty then
    false
  else
    s.data.all fun c =>
      c.isDigit || c == '+' || c == '-' || c == '.'

def isSpecialString (s : String) : Bool :=
  (s ∈ specialStrings) || isNumeric s

/-- A function that, given a string payload, selects the YAML `Tag` and
    scalar `Style` that should be used when serialising the scalar. -/
abbrev StringStyle := String → Tag × Style

/-- Encode a string scalar using the provided `StringStyle`.  We map
    directly onto the `Event.scalar` constructor defined in
    `Event.lean`.

    The empty-string case needs a special treatment to avoid LeanYaml
    choosing the *plain* style, which would be ambiguous in YAML.  We
    therefore fall back to *single-quoted* in that situation, matching
    the behaviour of the Haskell implementation.

    NOTE: At the moment we ignore the *anchor* feature of YAML.  The
    parameter is kept so that the signature aligns with the upstream
    API.
-/
def stringScalar (stringStyle : StringStyle) (anchor? : Option String)
    (s : String) : Event :=
  if s.isEmpty then
    -- Always single-quote the empty string (see
    -- <https://github.com/snoyberg/yaml/issues/24>).
    Event.scalar s Tag.noTag Style.singleQuoted anchor?
  else
    let (tag, style) := stringStyle s
    Event.scalar s tag style anchor?


/-- A conservative *default* implementation that chooses an encoding
    style according to very simple heuristics:

    • Strings that contain a newline are emitted using the *literal*
      block style.
    • Strings that are considered *special* (see `isSpecialString`)
      are single-quoted to prevent them from being interpreted as
      booleans/nulls/… by parsers.
    • Everything else is emitted as a plain scalar.
-/
def defaultStringStyle : StringStyle := fun s =>
  if s.data.any (fun c => c == '\n') then
    (Tag.noTag, Style.literal)
  else if isSpecialString s then
    (Tag.noTag, Style.singleQuoted)
  else
    (Tag.noTag, Style.plainNoTag)


/-!
### Object → Event conversion

The Haskell `yaml` package exposes an `objToEvents` helper which turns a
fully-materialised `Value` tree into a linear list of `Event`s suitable
for emission.  For the purposes of boot-strapping the Lean port we
provide a *minimal* version that

1. preserves the structural information (streams, sequences, mappings),
2. relies on `stringScalar` / `defaultStringStyle` to serialise
   individual scalars, and
3. deliberately ignores YAML anchors and tags for now.

This is **good enough** for round-tripping simple documents and allows
us to exercise the encoder end-to-end in upcoming tests.  It can be
refined later once more of the feature-set is in place.
-/

open LeanYaml.Data.Yaml (Event Value Tag Style)

private def boolToString (b : Bool) : String :=
  if b then "true" else "false"

private def floatToString (f : Float) : String :=
  -- The default `toString` on `Float` uses scientific notation for
  -- extreme values which is acceptable for YAML.  We'll keep it simple
  -- until a proper `Scientific` replacement lands.
  toString f

/-- Convert a `Value` into a *list* of `Event`s.  A caller can prepend
    `Event.streamStart` and append `Event.streamEnd` if a complete YAML
    stream is required. -/
partial def objToEvents (v : Value)
    (stringStyle : StringStyle := defaultStringStyle)
    : List Event :=
  match v with
  | .null      => [ Event.scalar "null" Tag.noTag Style.plainNoTag none ]
  | .bool b    => [ Event.scalar (boolToString b) Tag.boolTag Style.plainNoTag none ]
  | .number n  => [ Event.scalar (floatToString n) Tag.intTag Style.plainNoTag none ] -- TODO refine tag
  | .string s  => [ stringScalar stringStyle none s ]
  | .array arr =>
      let inner := arr.toList.foldr (fun v acc => objToEvents v stringStyle ++ acc) []
      [ Event.sequenceStart none ] ++ inner ++ [ Event.sequenceEnd ]
  | .object kvs =>
      let entries := kvs.foldr (fun (kv : String × Value) acc =>
        match kv with
        | (k, v) => (stringScalar stringStyle none k :: objToEvents v stringStyle) ++ acc) []
      [ Event.mappingStart none ] ++ entries ++ [ Event.mappingEnd ]

/-- Convenience wrapper that wraps `objToEvents` with the
    surrounding `streamStart` / `streamEnd` markers. -/
def objToStream (v : Value)
    (stringStyle : StringStyle := defaultStringStyle) : List Event :=
  [ Event.streamStart, Event.documentStart ] ++
    objToEvents v stringStyle ++
  [ Event.documentEnd, Event.streamEnd ]


-- -------------------------------------------------------------------------

-- (Duplicate helper definitions removed – canonical versions defined
-- earlier in the module.)

end LeanYaml.Data.Yaml
