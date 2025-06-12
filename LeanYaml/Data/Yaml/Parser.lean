/-
  Minimal event-stream → Value parser
  ---------------------------------

  This is **not** a complete YAML parser – it is just enough to consume
  the (currently very restricted) `Event` sequences produced by the Lean
  encoder (`objToStream`).  Implementing a full-fledged YAML front-end
  will require proper handling of anchors, tags, merge keys, multi-doc
  streams, etc.  For now we only support:

  • `scalar`   → always decoded as `Value.string` (no implicit typing)
  • sequences  → `[ ... ]`
  • mappings   → `{ key : value }` with **string scalar** keys only

  The goal is to enable simple round-trip tests so that other parts of
  the port can be verified incrementally.
-/

import LeanYaml.Data.Yaml.Event
import LeanYaml.Data.Yaml.Internal -- for Value, ParseException

open LeanYaml.Data.Yaml

/- Scalar classification helper ----------------------------------------- -/

private def classifyScalar (s : String) : Value :=
  let lower := s.foldl (fun acc c => acc.push c.toLower) ""
  match lower with
  | "true"  => Value.bool true
  | "false" => Value.bool false
  | "null" | "~" => Value.null
  | _ =>
      let isNumeric :=
        if s.isEmpty then false else
        s.data.all fun c => c.isDigit || c == '+' || c == '-' || c == '.'

      /- Very small helper just for positive/negative decimal numbers
         without exponent. -/
      let parseFloat? (str : String) : Option Float :=
        -- handle sign
        let (sign, body) :=
          if str.startsWith "-" then (-1.0, str.drop 1)
          else if str.startsWith "+" then (1.0, str.drop 1)
          else (1.0, str)
        let parts := body.split (fun c => c == '.')
        match parts with
        | [intPart] =>
            match intPart.toNat? with
            | some n => some (sign * Float.ofNat n)
            | none => none
        | [intPart, fracPart] =>
            match intPart.toNat?, fracPart.toNat? with
            | some nInt, some nFrac =>
                let denomNat := Nat.pow 10 fracPart.length
                let denom : Float := Float.ofNat denomNat
                some (sign * (Float.ofNat nInt + Float.ofNat nFrac / denom))
            | _, _ => none
        | _ => none

      if isNumeric then
        match parseFloat? s with
        | some f => Value.number f
        | none => Value.string s
      else
        Value.string s

namespace LeanYaml.Data.Yaml.Parser

/- Helper structure that threads the remaining input list through the
   recursive descent parser. -/
structure Stream where
  events : List Event
  deriving Inhabited

def Stream.peek? (st : Stream) : Option Event := st.events.head?
def Stream.pop! (st : Stream) : Except ParseException (Event × Stream) :=
  match st.events with
  | [] => .error <| ParseException.invalidYaml none
  | e :: es => .ok (e, { events := es })

/- Parse a full YAML document.  Accepts optional `streamStart` and
   `documentStart` markers. -/
/- Mutual recursion between node/collection parsers. -/

mutual

  /-- Parse any YAML node. -/
  partial def parseNode (st : Stream) : Except ParseException (Value × Stream) := do
    let (ev, st) ← st.pop!
    match ev with
    | Event.scalar val _ _ _ => pure (classifyScalar val, st)
    | Event.sequenceStart _  => parseSequence st []
    | Event.mappingStart _   => parseMapping st []
    | _ => .error <| ParseException.unexpectedEvent (received := some ev) (expected := none)

  /-- Parse sequence elements until the closing `sequenceEnd`. -/
  partial def parseSequence (st : Stream) (acc : List Value)
      : Except ParseException (Value × Stream) := do
    match st.peek? with
    | some Event.sequenceEnd =>
        let (_, st) ← st.pop!
        pure (Value.array (acc.reverse.toArray), st)
    | _ => do
        let (v, st) ← parseNode st
        parseSequence st (v :: acc)

  /-- Parse mapping key/value pairs until `mappingEnd`. -/
  partial def parseMapping (st : Stream) (acc : List (String × Value))
      : Except ParseException (Value × Stream) := do
    match st.peek? with
    | some Event.mappingEnd =>
        let (_, st) ← st.pop!
        pure (Value.object acc.reverse, st)
    | _ => do
        let (keyVal, st) ← parseNode st
        match keyVal with
        | Value.string k => do
            let (val, st) ← parseNode st
            parseMapping st ((k, val) :: acc)
        | _ => .error ParseException.nonScalarKey

end

/- Parse a complete document with optional wrappers. -/
partial def parseDocument (st₀ : Stream) : Except ParseException (Value × Stream) := do
  let st ← match st₀.peek? with
    | some Event.streamStart => do
        let (_, st) ← st₀.pop!
        pure st
    | _ => pure st₀
  let st ← match st.peek? with
    | some Event.documentStart => do
        let (_, st) ← st.pop!
        pure st
    | _ => pure st
  let (value, st) ← parseNode st
  let st ← match st.peek? with
    | some Event.documentEnd => do
        let (_, st) ← st.pop!
        pure st
    | _ => pure st
  let st ← match st.peek? with
    | some Event.streamEnd => do
        let (_, st) ← st.pop!
        pure st
    | _ => pure st
  pure (value, st)

/- Entry point utility. -/
def fromEvents (events : List Event) : Except ParseException Value := do
  let (v, rest) ← parseDocument { events := events }
  if rest.events.isEmpty then
    pure v
  else
    .error <| ParseException.unexpectedEvent (received := rest.peek?) (expected := none)

end LeanYaml.Data.Yaml.Parser
