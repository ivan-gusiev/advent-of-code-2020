namespace global

type Operation = | Acc | Jmp | Nop

module Operation =
    let parse (opstr : string) : Operation option =
        match opstr.ToLower() with
        | "acc" -> Some Acc
        | "jmp" -> Some Jmp
        | "nop" -> Some Nop
        | _ -> None

    let updates_ip (op : Operation) : bool =
        op = Jmp

type Instruction = { op : Operation; arg : int }

module Instruction =
    open Optional

    let mk op arg = { Instruction.op = op; arg = arg }

    let parse (asm : string) : Instruction option =
        optional {
            let! (opstr, argstr) = Util.split2 ' ' asm
            let! op = Operation.parse opstr
            let! arg = Util.intParse argstr
            return mk op arg
        }

type Stats = { ops_executed : Map<Operation, int>; instructions_executed : Map<int, int> }

module Stats =
    let empty = { Stats.ops_executed = Map.ofSeq [ Acc, 0; Jmp, 0; Nop, 0 ]; Stats.instructions_executed = Map.empty }

    let inc<'t when 't : comparison> (key :'t) (map : Map<'t, int>) =
        let cur = Map.tryFind key map |> Option.defaultValue 0
        Map.add key (cur + 1) map

    let countOp (op : Operation) (stats : Stats) =
        { stats with Stats.ops_executed = inc op stats.ops_executed }

    let countIp (index : int) (stats : Stats) =
        { stats with Stats.instructions_executed = inc index stats.instructions_executed }

    let hasLooped (stats : Stats) (threshold : int) : bool =
        Map.toSeq stats.instructions_executed
        |> Seq.exists (fun (_, num) -> num > threshold)


type MachineState = {
    accumulator : int
    next_instruction : int
    stats : Stats
}

module MachineState =
    let empty = { 
        MachineState.accumulator = 0
        next_instruction = 0
        stats = Stats.empty
    }

    let setAccumulator (newAcc : int) (machine : MachineState) = { 
        machine with accumulator = newAcc
    }

    let setIp (newIp : int) (machine : MachineState) = { 
        machine with next_instruction = newIp
    }

    let private executeImpl (asm : Instruction) (machine : MachineState) =
        match (asm.op, asm.arg) with
        | (Acc, x) -> setAccumulator (machine.accumulator + x) machine
        | (Jmp, y) -> setIp (machine.next_instruction + y) machine
        | (Nop, _) -> machine

    let private bump (asm : Instruction) (ip : int) (machine : MachineState) = {
        MachineState.accumulator = machine.accumulator
        next_instruction = machine.next_instruction + if Operation.updates_ip asm.op then 0 else 1
        stats = Stats.countOp asm.op machine.stats |> Stats.countIp ip
    }

    let executeInstruction (asm : Instruction) (machine : MachineState) : MachineState =
        executeImpl asm machine
        |> bump asm machine.next_instruction

    let executeProgram (asm : Instruction array) (state : MachineState) : MachineState seq =
        let step (machine : MachineState) =
            let ip = machine.next_instruction
            if (ip >= asm.Length || ip < 0) then None else
                let newMachine = executeInstruction asm.[ip] machine
                (newMachine, newMachine) |> Some
        Seq.unfold step state

module Day8 =
    open FSharp.Collections.ParallelSeq

    let lines = Fixture.lines 8
    let instructions = Array.map (Instruction.parse >> Option.get) lines
    
    let accIfTerminates (program : Instruction array) : int option =
        let res = MachineState.executeProgram program MachineState.empty
                  |> Seq.indexed
                  |> Seq.takeWhile (fun (i, _) -> i < 1000)
                  |> Seq.toArray
        if (res.Length >= 1000) then 
            None 
        else 
            let (_, last) = Array.last res
            Some last.accumulator

    let solve () =
        let update index value arr =
            let arr' = Array.copy arr
            arr'.[index] <- value
            arr'

        let tryPatch (i, instr) =
            match instr with
            | { op = Jmp; arg = arg } -> update i { op = Nop; arg = arg } |> Some
            | { op = Nop; arg = arg } -> update i { op = Jmp; arg = arg } |> Some
            | _ -> None

        let progress i =
            if i % 10 = 0 then
                printfn "Attempt %i/%i" i instructions.Length
            else
                ()
        
        let programs = 
            instructions
            |> Seq.indexed
            |> Seq.choose tryPatch
            |> Seq.map (fun x -> x instructions)
            |> Seq.toArray

        programs
        |> PSeq.withDegreeOfParallelism 12
        |> PSeq.mapi  (fun i x -> progress i; accIfTerminates x)
        |> Seq.tryFind Option.isSome
        |> Option.flatten