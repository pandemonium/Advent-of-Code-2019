namespace AdventOfCode

open System


module Day2 =
  
  module Intcode =
    let todo<'a> : 'a = failwith "Not yet!"

    type Memory = 
      Map<int, int>
    and 
      Instruction =
      | Add      of int * int * int
      | Multiply of int * int * int
      | Hcf

    module Location =
      let write = Map.add

      let read  = Map.find

      let rec readMultiple offset length memory : int list =
        [ for i in 0..(length - 1) -> 
            read (offset + i) memory 
        ]

    module Fetcher =
      type Opcode =
        | Add      = 1
        | Multiply = 2
        | Hcf      = 99

      let (|Tag|_|) = function
      | 1  -> Some Opcode.Add
      | 2  -> Some Opcode.Multiply
      | 99 -> Some Opcode.Hcf
      | _  -> None

      let decodeBinop binop at =
        Location.readMultiple at 3
        >> List.take 3
        >> function
           | [source1; source2; destination] ->
               (source1, source2, destination)
               |> binop

      let halt = failwith

      let decode at memory : Instruction =
        Location.read at memory
        |> function
          | Tag Opcode.Add -> 
            decodeBinop Add (at + 1) memory
          | Tag Opcode.Multiply -> 
            decodeBinop Multiply (at + 1) memory
          | Tag Opcode.Hcf -> 
            Hcf
          | tag ->
            halt <| sprintf "%d: unknown opcode" tag


    module Loader =
      open System.IO

      let parseContent (content: string) =
        content.Split [| ',' |]
        |> List.ofArray
        |> List.map int

      let tuple a b = a, b

      let load =
        List.mapi tuple >> Map.ofList

      let loadProgram =
        File.ReadAllText
        >> parseContent
        >> load

    module Interpreter =
      type MachineState = Make of int * Memory

      module MachineState =
        let bootstrap code = 
          Make (0, code)

        let apply (f: Memory -> Memory) = function
        | Make (instructionPointer, memory) ->
          Make (instructionPointer + 4, f memory)

      let evaluateOperation a b c f memory =
        let a' = Location.read a memory
        let b' = Location.read b memory
        Location.write c (f a' b') memory

      let runInstruction = 
        MachineState.apply 
        << function
          | Add (source1, source2, destination) -> 
            evaluateOperation source1 source2 destination (+)
          | Multiply (source1, source2, destination) -> 
            evaluateOperation source1 source2 destination (*)
          | _ ->
            id

      let rec runLoop = function
      | Make (instructionPointer, memory) as machine ->
        Fetcher.decode instructionPointer memory
        |> function
           | Hcf ->
             Location.read 0 memory
           | ins ->
             runInstruction ins machine
             |> runLoop

      let makeParameter noun verb =
        Map.add    1 noun
        << Map.add 2 verb

      let start intialParameter =
        MachineState.bootstrap 
        >> MachineState.apply intialParameter
        >> runLoop

    module Executive =
      let parameters =
        Interpreter.makeParameter 12 2

      let run =
        Loader.loadProgram 
        >> Interpreter.start parameters

    module ParamFinder =
      let determineInputs target program =
        let searchSpace = 
          Seq.allPairs [0..99] [0..99]

        let successfulTrial (noun, verb) = 
            let runTrial =
              Interpreter.makeParameter noun verb
              |> Interpreter.start

            runTrial program = target

        searchSpace
        |> Seq.tryFind successfulTrial
    
      let present (noun, verb) =
        100 * noun + verb

      let run =
        Loader.loadProgram
        >> determineInputs 19690720
        >> Option.map present
