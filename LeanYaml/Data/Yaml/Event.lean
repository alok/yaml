/-!  Minimal YAML event model for Lean port.

This intentionally captures only the subset of events needed for the
initial porting work.  We can extend/rename later as the implementation
matures.
-/

namespace LeanYaml.Data.Yaml

/-- YAML tag category.  Only a few built-ins for now. -/
inductive Tag : Type where
  | noTag
  | strTag
  | boolTag
  | intTag
  deriving Repr, BEq, Inhabited

/-- YAML scalar style – a rough subset. -/
inductive Style : Type where
  | plainNoTag
  | singleQuoted
  | doubleQuoted
  | folded
  | literal
  deriving Repr, BEq, Inhabited

/-- Optional anchor associated with an event. -/
structure Anchor where
  name? : Option String := none
  deriving Repr, BEq, Inhabited

/-- Streaming parse events produced by a YAML lexer/emitter. -/
inductive Event : Type where
  | streamStart
  | streamEnd
  | documentStart
  | documentEnd
  | sequenceStart (anchor? : Option String := none)
  | sequenceEnd
  | mappingStart (anchor? : Option String := none)
  | mappingEnd
  | scalar (value : String) (tag : Tag := Tag.noTag) (style : Style := Style.plainNoTag) (anchor? : Option String := none)
  | alias (anchorName : String)
  deriving Repr, BEq, Inhabited

/-!  Simple `ToString` instance for debugging purposes.  We define it
after the inductive so that pattern matching on the constructors is
well-typed. -/

private def eventToString : Event → String
  | .streamStart => "streamStart"
  | .streamEnd => "streamEnd"
  | .documentStart => "documentStart"
  | .documentEnd => "documentEnd"
  | .sequenceStart _ => "sequenceStart"
  | .sequenceEnd => "sequenceEnd"
  | .mappingStart _ => "mappingStart"
  | .mappingEnd => "mappingEnd"
  | .scalar val _ _ _ => s!"scalar({val})"
  | .alias name => s!"alias({name})"

instance : ToString Event where
  toString := eventToString

end LeanYaml.Data.Yaml
