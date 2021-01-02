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
[<Measure>] type PerKm
type CapacityType = | Refrigerated of float<m^3> | NonRefrigerated of float<m^3>
// Parameters.
type TruckInfo = {
    CostPerKm : float<NZD> 
    RefrigeratedCapacity : float<m^3>
    NonRefrigeratedCapacity : float<m^3>
}

let truckTypeInfoList = 
    [ 
        (TruckType "TypeA", { CostPerKm = 30.<NZD>; RefrigeratedCapacity = 20.<m^3>; NonRefrigeratedCapacity = 40.<m^3> })
        (TruckType "TypeB", { CostPerKm = 40.<NZD>; RefrigeratedCapacity = 30.<m^3>; NonRefrigeratedCapacity = 30.<m^3> })
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
    let truckInfo = truckTypeInfoList |> List.map (fun (x, y) -> x, y.CostPerKm) |> SMap.ofList
    sum (truckInfo .* truckTypesToSelect)
let objective = Objective.create "MinimizeTransportCost" Minimize objectiveExpression

// Constraints.
let refrigeratedStockExpr = 
    [
        for KeyValue(x, decVar) in truckTypesToSelect ->
            let truckInfo = truckTypeInfo.[x]
            truckInfo.RefrigeratedCapacity * decVar
    ] |> List.sum
let refrigeratedStock = Constraint.create "refrigeratedStock" (refrigeratedStockExpr == 3000.<m^3>)

let nonRefrigeratedStockExpr = 
    [
        for KeyValue(x, decVar) in truckTypesToSelect ->
            let truckInfo = truckTypeInfo.[x]
            truckInfo.NonRefrigeratedCapacity * decVar
    ] |> List.sum
let nonRefrigeratedStock = Constraint.create "nonRefrigeratedStock" (nonRefrigeratedStockExpr == 4000.<m^3>)

let model =
    Model.create objective
    |> Model.addConstraint refrigeratedStock
    |> Model.addConstraint nonRefrigeratedStock
