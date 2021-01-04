// Learn more about F# at http://fsharp.org

open System
open Flips
open Flips.Types
open Flips.SliceMap

let settings = {
    SolverType = SolverType.CBC
    MaxDuration = 10_000L
    WriteLPFile = None
    WriteMPSFile = None
}

[<EntryPoint>]
let main argv =
    //let plan = Solver.solve settings MaximizeIncome.model
    //let plan = Solver.solve settings Gadgets.Simple.model
    //printfn "%A" plan
    //let plan = Solver.solve settings Gadgets.Maps.model
    //printfn "%A" plan
    //let plan = Solver.solve settings NumberOfTrucksToHire.model
    //let plan = Solver.solve settings FoodTruck.model
    //let plan = Solver.solve settings Junk.model
    //let plan = Solver.solve settings ToyStore.model
    let plan = Solver.solve settings TableFactory.model
    printfn "%A" plan
    0 // return an integer exit code
