module NumberOfTrucksToHire

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure

// https://www.superprof.co.uk/resources/academic/maths/linear-algebra/linear-programming/linear-programming-problems-and-solutions.html
(*
    A transport company has two types of trucks, Type A and Type B. 

    Type A has a refrigerated capacity of 20 m^3 and a non-refrigerated capacity of 40 m^3 
    while Type B has the same overall volume with equal sections for refrigerated and 
    non-refrigerated stock. 
    
    A grocer needs to hire trucks for the transport of 3000 m ^3 of refrigerated stock and 
    4,000 m^3 of non-refrigerated stock. The cost per kilometer of a Type A is $30, and $40 for 
    Type B. How many trucks of each type should the grocer rent to achieve the minimum total cost?
*)

type TruckType = TruckType of string

[<Measure>] type NZD
[<Measure>] type km

// Parameters.
type TruckInfo = {
    CostPerKm : float<NZD/km> 
    RefrigeratedCapacity : float<m^3>
    NonRefrigeratedCapacity : float<m^3>
}

let truckTypeInfoList = 
    [ 
        (TruckType "TypeA", { CostPerKm = 30.<NZD/km>; RefrigeratedCapacity = 20.<m^3>; NonRefrigeratedCapacity = 40.<m^3> })
        (TruckType "TypeB", { CostPerKm = 40.<NZD/km>; RefrigeratedCapacity = 30.<m^3>; NonRefrigeratedCapacity = 30.<m^3> })
    ] 
let truckTypeInfo = truckTypeInfoList |> SMap.ofList

// Decision vars.
let truckTypesToSelect = 
    [
        for KeyValue(x, _) in truckTypeInfo -> 
            let (TruckType name) = x
            x, Decision.createContinuous name 0. infinity
    ] |> SMap

// Objective expression.
let objectiveExpression = 
    let trucksCostPerKm = truckTypeInfoList |> List.map (fun (x, y) -> x, y.CostPerKm) |> SMap.ofList
    sum (trucksCostPerKm .* truckTypesToSelect)
let objective = Objective.create "MinimizeTransportCost" Minimize objectiveExpression

// Constraints.
let (refrigeratedStockExpr, nonRefrigeratedStockExpr) = 
    let decisions =
        [
            for KeyValue(x, decVar) in truckTypesToSelect ->
                let truckInfo = truckTypeInfo.[x]
                (truckInfo.RefrigeratedCapacity * decVar), (truckInfo.NonRefrigeratedCapacity * decVar)
        ] 
    let (refrigeratedStock, nonRefrigeratedStock) = decisions |> List.unzip
    (refrigeratedStock |> List.sum, nonRefrigeratedStock |> List.sum)

let maxRefrigeratedStock = Constraint.create "refrigeratedStock" (refrigeratedStockExpr == 3000.<m^3>)
let maxNonRefrigeratedStock = Constraint.create "nonRefrigeratedStock" (nonRefrigeratedStockExpr == 4000.<m^3>)

let model =
    Model.create objective
    |> Model.addConstraint maxRefrigeratedStock
    |> Model.addConstraint maxNonRefrigeratedStock
