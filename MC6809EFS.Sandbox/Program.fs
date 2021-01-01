open System.IO
open System
open MC6809EFS

let rec private processInputCodeLine (line:string) (mem:Map<uint16, byte>, ptr:uint16) : Map<uint16, byte> * uint16 =
    match line.Length with
    | 0 ->
        (mem, ptr)
    | 1 ->
        raise (NotImplementedException "Need odd number for code line!")
    | _ ->
        let byteValue = (Convert.ToByte(line.Substring(0,2),16))
        printfn "[%4x] <- %2x" ptr byteValue
        let mem= mem |> Map.add ptr byteValue
        let ptr = ptr + 1us
        processInputCodeLine (line.Substring(2)) (mem, ptr)

let private processInputLine (mem:Map<uint16, byte>, ptr:uint16) (line:string) : Map<uint16, byte> * uint16 =
    if line.StartsWith('@') then
        let wordValue = Convert.ToUInt16(line.Substring(1),16)
        printfn "@ <- %4x" wordValue
        (mem, wordValue)
    elif line.StartsWith('#') then
        //comment!
        (mem, ptr)
    else
        (mem, ptr)
        |> processInputCodeLine line

[<EntryPoint>]
let main argv =
    File.ReadAllLines("test.asm")
    |> Array.fold processInputLine (Map.empty, 0us)
    |> fst
    |> Machine.spool
    |> Machine.tron Console.WriteLine
    |> Machine.restart
    |> ignore
    0
