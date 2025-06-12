import LeanYaml.Data.Yaml.Internal
import LeanYaml.Data.Yaml.Parser

open LeanYaml.Data.Yaml
open LeanYaml.Data.Yaml.Parser

/-!  Very small test suite exercising round-trip encoder ↔ parser logic.
    We keep it lean to avoid introducing external dependencies.  The
    executable terminates with a non-zero exit code on failure so CI can
    pick it up easily. -/

def roundTrip (v : Value) : Except ParseException Value :=
  fromEvents (objToStream v)

private def reprVal (v : Value) : String := (repr v).pretty 80

def expectEq (name : String) (lhs rhs : Value) : IO Unit :=
  if lhs == rhs then
    pure ()
  else
    throw <| IO.userError s!"{name}: values differ\nexpected: {reprVal lhs}\n but got: {reprVal rhs}"

def simpleValues : List (String × Value) :=
  [ ("scalar", Value.string "hello")
  , ("boolTrue", Value.bool true)
  , ("boolFalse", Value.bool false)
  , ("number", Value.number 42.0)
  , ("null", Value.null)
  , ("array", Value.array #[Value.string "foo", Value.string "bar"])
  , ("map",   Value.object [("k1", Value.string "v1"), ("k2", Value.number 99.0)])
  ]

def runRoundTripTests : IO Unit := do
  for (name, v) in simpleValues do
    match roundTrip v with
    | .ok v' => expectEq name v v'
    | .error e => throw <| IO.userError s!"{name}: parsing failed – {e}"

def main : IO Unit := do
  try
    runRoundTripTests
    IO.println "All tests passed."
  catch err =>
    -- Re-throw so that the process exits with failure.
    throw err
