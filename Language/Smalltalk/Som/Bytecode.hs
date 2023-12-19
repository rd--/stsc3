module Language.Smalltalk.Som.Bytecode where

data Bytecode
  = Halt
  | Dup
  | PushLocal
  | PushArgument
  | PushField
  | PushBlock
  | PushConstant
  | PushGlobal
  | Pop
  | PopLocal
  | PopArgument
  | PopField
  | Send
  | SuperSend
  | ReturnLocal
  | ReturnNonLocal
  deriving (Eq, Enum, Bounded, Ord, Show)

bytecodeLength :: Bytecode -> Int
bytecodeLength bytecode =
  case bytecode of
    Halt -> 1
    Dup -> 1
    PushLocal -> 3 -- temporary index, context index
    PushArgument -> 3 -- argument index, context index
    PushField -> 2 -- field index (index into an array of all class and instance variables)
    PushBlock -> 2
    PushConstant -> 2 -- constant index
    PushGlobal -> 2 -- global index
    Pop -> 1
    PopLocal -> 3
    PopArgument -> 3
    PopField -> 2
    Send -> 2 -- method index
    SuperSend -> 2 -- method index
    ReturnLocal -> 1
    ReturnNonLocal -> 1

bytecodeName :: Bytecode -> String
bytecodeName = show

-- > map (length . paddedBytecodeName) [minBound .. maxBound]
paddedBytecodeName :: Bytecode -> String
paddedBytecodeName bytecode =
  let longest = length (show ReturnNonLocal)
      name = bytecodeName bytecode
  in name ++ replicate (longest - length name) ' '
