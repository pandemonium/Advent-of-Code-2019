namespace AdventOfCode

open System


module Day2 =
  
  let tuple a b = a, b
  let tupleApply f a b = f (a, b)
  let untuple f (a, b) = f a b
  let konst x = fun _ -> x

  module Intcode =
    let todo<'a> : 'a = failwith "Not yet!"

    type Memory = 
      Map<int, int>
    and 
      Instruction =
      | Add      of Select * Select * Select
      | Multiply of Select * Select * Select
      | Input    of Select
      | Output   of Select
      | Hcf
    and Select =
      | Address of int
      | Immediate of int

    module Instruction =
      let size = function
      | Add _
      | Multiply _ -> 4
      | Input _
      | Output _   -> 2
      | Hcf _      -> 1

    type Operation =
      Op of Opcode * ParameterMode list
    and Opcode =
      | Add      = 1
      | Multiply = 2
      | Input    = 3
      | Output   = 4
      | Hcf      = 99
    and ParameterMode =
      | Position
      | Immediate

    module Operation =
      let digits =
        string >> List.ofSeq

      let sequence =
        Day1.List.sequence

      let decode image : Operation option =
        let opCode = 
          match image % 100 with
            | 1  -> Some Opcode.Add
            | 2  -> Some Opcode.Multiply
            | 3  -> Some Opcode.Input
            | 4  -> Some Opcode.Output
            | 99 -> Some Opcode.Hcf
            | _  -> None
  
        let parameterModeFlags = 
          let tryParseFlag = function 
          | '0' -> Some Position
          | '1' -> Some Immediate
          | _   -> None

          image / 100
          |> digits
          |> List.map tryParseFlag
          |> List.rev
          |> sequence

        (opCode, parameterModeFlags)
        ||> Option.map2 (tupleApply Op)
        

    module Location =
      let write = Map.add

      let read location memory = 
        try
          Map.find location memory
        with
          _ -> 
            printfn "%A not found in %A" location memory
            reraise ()

      let rec readMultiple offset length memory : int list =
        [ for i in 0..(length - 1) -> 
            read (offset + i) memory 
        ]

    module Fetcher =
      let select = function
      | Position  -> Select.Address
      | Immediate -> Select.Immediate

      let decodeOperand parameterMode at =
        Location.read at 
        >> select parameterMode

      let rightPad padWith length list =
        let length'   = List.length list
        let padLength = length - length'
        let padding   = List.replicate padLength padWith

        list @ padding

      let decodeBinop parameterModes binop at =
        Location.readMultiple at 3
        >> List.take 3
        >> List.zip (rightPad Position 3 parameterModes)
        >> List.map (untuple select)
        >> function
           | [source1; source2; destination] ->
               (source1, source2, destination)
               |> binop

      let haltCatchFire at memory =
        Location.read at memory
        |> sprintf "Unknown instruction %d at %d" at
        |> failwith

      let decode at memory : Instruction =
        Location.read at memory
        |> Operation.decode
        |> Option.map (function
          | Op (Opcode.Add, parameterModes) -> 
            decodeBinop parameterModes Add (at + 1) memory
          | Op (Opcode.Multiply, parameterModes) -> 
            decodeBinop parameterModes Multiply (at + 1) memory
          | Op (Opcode.Input, parameterMode::_) ->
            Input <| decodeOperand parameterMode (at + 1) memory
          | Op (Opcode.Output, parameterMode::_) ->
            Output <| decodeOperand parameterMode (at + 1) memory
          | Op (Opcode.Hcf, parameterModes) -> 
            Hcf
        )
        |> Option.defaultWith (fun _ ->
          haltCatchFire at memory
        )

    module Loader =
      open System.IO

      let parseContent (content: string) =
        content.Split [| ',' |]
        |> List.ofArray
        |> List.map int

      let load =
        List.mapi tuple >> Map.ofList

      let loadProgram =
        File.ReadAllText
        >> parseContent
        >> load

    module Bios =
      let input () = 1 // todo
      let output s = 
        printfn "stdout: %d" s

    module Interpreter =
      type MachineState = Make of int * Memory

      module MachineState =
        let bootstrap code = 
          Make (0, code)

        let mapInstructionPointer f = function
        | Make (instructionPointer, memory) ->
          Make (f instructionPointer, memory)

        let step =
          (+) >> mapInstructionPointer

        let goto =
          konst >> mapInstructionPointer

        let mapMemory (f: Memory -> Memory) = function
        | Make (instructionPointer, memory) ->
          Make (instructionPointer, f memory)


      let resolve = function
      | Select.Address address ->
        Location.read address
      | Select.Immediate immediate ->
        fun _ -> immediate

      let evaluateBinary a b c f memory =
        let a' = resolve a memory
        let b' = resolve b memory

        match c with
        | Address c' ->
          Location.write c' (f a' b') memory
        | _ ->
          failwith "evaluateBinary error."

      let runInstruction = 
        MachineState.mapMemory 
        << function
           | Add (source1, source2, destination) -> 
             evaluateBinary source1 source2 destination (+)
           | Multiply (source1, source2, destination) as ins -> 
             evaluateBinary source1 source2 destination (*)
           | Input (Address destination) -> 
             Bios.input ()
             |> Location.write destination
           | Output source ->
             fun memory ->
             resolve source memory 
             |> Bios.output
             memory

           | tag ->
             failwith <| sprintf "%A" tag

      let rec runLoop = function
      | Make (instructionPointer, memory) as machine ->

        Fetcher.decode instructionPointer memory
        |> function
           | Hcf ->
             Location.read 0 memory
           | instruction ->
             runInstruction instruction machine
             |> MachineState.step (Instruction.size instruction)
             |> runLoop

      let makeParameter noun verb =
        Map.add    1 noun
        << Map.add 2 verb

      let start intialParameter =
        MachineState.bootstrap 
        >> MachineState.mapMemory intialParameter
        >> runLoop

    module Executive =
      let parameters =
        id
//        Interpreter.makeParameter 12 2

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
