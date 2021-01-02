module NumberOfTrucksToHire

open FSharp.Data.UnitSystems.SI
open Flips
open Flips.Types
open Flips.SliceMap

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
[<Measure>] type m3 = UnitNames.meter^3
type CapacityType = | Refrigerated of float<m3> | NonRefrigerated of float<m3>

type Truck = Truck of string

// Parameters.
let truckTypes = [ "TypeA"; "TypeB" ]
let truckCostPerKm = [ ("TypeA", 30.); ("TypeB", 40.)] |> SMap
let refrigeratedCapacity = [ ("TypeA", 20.); ("TypeB", 30.)] |> SMap
let nonRefrigeratedCapacity = [ ("TypeA", 40.); ("TypeB", 30.)] |> SMap

// Decision vars.
let truckTypesToSelect = 
    [
        for x in truckTypes -> x, Decision.createContinuous x 0. infinity
    ] |> SMap

// Objective expression.
let objectiveExpression = sum (truckCostPerKm .* truckTypesToSelect)
let objective = Objective.create "MinimizeTransportCost" Minimize objectiveExpression

// Constraint.
let refrigeratedStockExpr = 
    [
        for KeyValue(x, decVar) in truckTypesToSelect ->
            refrigeratedCapacity.[x] * decVar
    ] |> List.sum
let refrigeratedStock = Constraint.create "refrigeratedStock" (refrigeratedStockExpr >== 3000.)

let nonRefrigeratedStockExpr = 
    [
        for KeyValue(x, decVar) in truckTypesToSelect ->
            nonRefrigeratedCapacity.[x] * decVar
    ] |> List.sum
let nonRefrigeratedStock = Constraint.create "nonRefrigeratedStock" (nonRefrigeratedStockExpr >== 4000.)

let model =
    Model.create objective
    |> Model.addConstraint refrigeratedStock
    |> Model.addConstraint nonRefrigeratedStock
