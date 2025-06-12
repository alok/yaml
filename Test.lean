import LeanYaml.Data.Yaml.Internal
import LeanYaml.Data.Yaml.Parser
open LeanYaml.Data.Yaml
open LeanYaml.Data.Yaml.Parser

#eval
  let val : Value := .array #[ .string "hello", .string "world" ]
  let events := LeanYaml.Data.Yaml.objToStream val
  match fromEvents events with
  | .ok v => v
  | .error e => panic! e.toString
