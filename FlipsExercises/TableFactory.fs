module TableFactory

open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure

(*
    https://www.analyzemath.com/linear_programming/linear_prog_applications.html

    A company produces two types of tables, T1 and T2. 
    
    It takes 2 hours to produce the parts of one unit of T1, 
    1 hour to assemble and 2 hours to polish. 
    It takes 4 hours to produce the parts of one unit of T2, 
    2.5 hour to assemble and 1.5 hours to polish. 
    
    Per month, 7000 hours are available for producing the parts, 
    4000 hours for assembling the parts and 5500 hours for polishing the tables. 
    
    The profit per unit of T1 is $90 and per unit of T2 is $110. 
    
    How many of each type of tables should be produced in order to maximize the total monthly 
    profit?

*)

// Parameters.
[<Measure>] type Hour
[<Measure>] type NZD
type TableDetail = {
    PartsProductionTime : float<Hour>
    AssemblyTime : float<Hour>
    PolishTime : float<Hour>
    Profit : float<NZD>
}

let maxPartsProductionTime = 7_000.0<Hour>
let maxAssemblyTime = 4_000.0<Hour>
let maxPolishingTime = 5_500.0<Hour>

let tables = 
    [
        ("Table T1", { PartsProductionTime = 2.0<Hour>; AssemblyTime = 1.0<Hour>; PolishTime = 2.0<Hour>; Profit =  90.00<NZD>})
        ("Table T2", { PartsProductionTime = 4.0<Hour>; AssemblyTime = 2.5<Hour>; PolishTime = 1.5<Hour>; Profit = 110.00<NZD>})
    ] |> SMap

// Decision.
let tablesToProduce =
    [
        for KeyValue(table, _) in tables ->
            table, Decision.createContinuous $"Table{table}" 0. infinity
    ] |> SMap

// Objective.
let objective =
    let objectiveExpr =
        [
            for KeyValue(table, tableDetail) in tables ->
                tablesToProduce.[table] * tableDetail.Profit
        ] |> List.sum
    Objective.create "Maximize" Maximize objectiveExpr

// Constraint.
let (maxPartsProductionTimeConstraint, maxAssemblyTimeConstraint, maxPolishingTimeConstraint) =
    let maxPartsProductionTimeExpr =
        [
            for KeyValue(table, tableDetail) in tables ->
                tablesToProduce.[table] * tableDetail.PartsProductionTime
        ] |> List.sum
    let maxAssemblyTimeExpr =
        [
            for KeyValue(table, tableDetail) in tables ->
                tablesToProduce.[table] * tableDetail.AssemblyTime
        ] |> List.sum
    let maxPolishingTimeExpr =
        [
            for KeyValue(table, tableDetail) in tables ->
                tablesToProduce.[table] * tableDetail.PolishTime
        ] |> List.sum

    (
        Constraint.create "maxPartsProductionTimeConstraint" (maxPartsProductionTimeExpr <== maxPartsProductionTime),
        Constraint.create "maxAssemblyTimeConstraint" (maxAssemblyTimeExpr <== maxAssemblyTime),
        Constraint.create "maxPolishingTimeConstraint" (maxPolishingTimeExpr <== maxPolishingTime)
    )    

let model =
    Model.create objective
    |> Model.addConstraint maxPartsProductionTimeConstraint
    |> Model.addConstraint maxAssemblyTimeConstraint
    |> Model.addConstraint maxPolishingTimeConstraint
        
