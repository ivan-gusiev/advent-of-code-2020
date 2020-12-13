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

type Stats = { ops_executed : Map<Operation, int> }

module Stats =
    let empty = { Stats.ops_executed = Map.ofSeq [ Acc, 0; Jmp, 0; Nop, 0 ] }

    let countOp (op : Operation) (stats : Stats) =
        let current = stats.ops_executed.[op] + 1
        { Stats.ops_executed = Map.add op current stats.ops_executed }

    let hasLooped (stats : Stats) : bool =
        Map.toSeq stats.ops_executed
        |> Seq.exists (fun (_, num) -> num > 1)

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

    let private bump (asm : Instruction) (machine : MachineState) = {
        MachineState.accumulator = machine.accumulator
        next_instruction = machine.next_instruction + if Operation.updates_ip asm.op then 0 else 1
        stats = Stats.countOp asm.op machine.stats
    }

    let executeInstruction (asm : Instruction) (machine : MachineState) : MachineState =
        executeImpl asm machine
        |> bump asm

    let executeProgram (asm : Instruction array) (state : MachineState) : MachineState seq =
        let step (machine : MachineState) =
            let ip = machine.next_instruction
            if (ip >= asm.Length || ip < 0) then None else
                let newMachine = executeInstruction asm.[ip] machine
                (newMachine, newMachine) |> Some
        Seq.unfold step state

module Day8 =

    let lines = Fixture.lines 8
    let instructions = Array.map (Instruction.parse >> Option.get) lines

    let solve () =
        let states = MachineState.executeProgram instructions MachineState.empty
                     |> Seq.takeWhile (fun x -> Stats.hasLooped x.stats |> not)
                     |> Seq.toArray
        states.[states.Length - 2]