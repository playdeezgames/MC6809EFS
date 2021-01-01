namespace MC6809EFS

open System

type Machine = 
    {
        trace : string -> unit
        programCounter : uint16
        memory: Map<uint16,byte>
    }

module Machine =
    let private readMemoryByte(address:uint16) (machine:Machine): byte =
        machine.memory
        |> Map.tryFind address
        |> Option.defaultValue 0uy

    let private readProgramCounterByte (machine:Machine) : byte * Machine =
        let result = readMemoryByte(machine.programCounter) machine
        result, {machine with programCounter = machine.programCounter + 1us}

    let private bytesToWord (msb:byte, lsb:byte) : uint16 =
        (msb |> uint16) * 0x100us + (lsb |> uint16)

    let private readProgramCounterWord (machine:Machine) : uint16 * Machine =
        let msb, machine = readProgramCounterByte machine
        let lsb, machine = readProgramCounterByte machine
        bytesToWord(msb, lsb), machine

    let private readMemoryWord(address:uint16) (machine:Machine) : uint16 =
        bytesToWord(readMemoryByte address machine, readMemoryByte (address+1us) machine) 

    let private resetVector = 0xFFFEus

    let private reset (machine:Machine) : Machine =
        {machine with programCounter = readMemoryWord resetVector machine}

    [<Literal>]
    let private ExtendedJmp = 0x7Euy
    [<Literal>]
    let private InherentNop = 0x12uy
    [<Literal>]
    let private RelativeBrn = 0x21uy
    [<Literal>]
    let private Page10 = 0x10uy

    let private handleExtendedJmp (machine:Machine) : Machine =
        let address, machine = readProgramCounterWord machine
        let machine = {machine with programCounter = address}
        address
        |> sprintf "JMP %4X"
        |> machine.trace
        machine

    let private handleInherentNop (machine:Machine) : Machine =
        sprintf "NOP"
        |> machine.trace
        machine

    let private handleRelativeBrn (machine:Machine) : Machine =
        let value, machine = readProgramCounterByte machine
        value
        |> sprintf "BRN %2X"
        |> machine.trace
        machine

    let private handleRelativeLbrn (machine:Machine) : Machine =
        let value, machine = readProgramCounterWord machine
        value
        |> sprintf "LBRN %4X"
        |> machine.trace
        machine

    let private handlePage10 (machine:Machine) : Machine =
        let opCode, machine = readProgramCounterByte machine
        match opCode with
        | RelativeBrn ->
            handleRelativeLbrn machine
        | _ ->
            raise (opCode |> sprintf "Page10 Opcode '%x' not handled!" |> NotImplementedException)

    let private step (machine:Machine) : Machine =
        let opCode, machine = readProgramCounterByte machine
        match opCode with
        | ExtendedJmp ->
            handleExtendedJmp machine
        | InherentNop ->
            handleInherentNop machine
        | Page10 ->
            handlePage10 machine
        | RelativeBrn ->
            handleRelativeBrn machine
        | _ ->
            raise (opCode |> sprintf "Opcode '%x' not handled!" |> NotImplementedException)

    let rec private start (machine:Machine) : Machine =
        let machine = step machine
        start machine

    let restart (machine:Machine) : Machine =
        let machine = reset machine
        start machine

    let spool(mem:Map<uint16, byte>) : Machine =
        {
            trace = ignore
            programCounter=0us
            memory = mem
        }

    let tron(f:string->unit) (machine:Machine) : Machine =
        {machine with trace = f}

    let troff (machine:Machine) : Machine =
        {machine with trace = ignore}



