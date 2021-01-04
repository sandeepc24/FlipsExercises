module Junk

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure

[<Measure>] type NZD
[<Measure>] type Item

// Parameters.
type ItemDetail = {
    Profit : float<NZD/Item>
    MaxNumber : float
    Weight : float<kg>
}
let items = 
    [
        "Hamburger", { Profit = 1.50<NZD/Item>; MaxNumber = 300.; Weight = 0.5<kg> }
        "HotDog", { Profit = 1.20<NZD/Item>; MaxNumber = 200.; Weight = 0.4<kg> }
    ] |> SMap.ofList
let maxTruckWeight = 200.<kg>

// Decision.
let numberOfItems =
    [
        for KeyValue(item, itemDetail) in items ->
            item, Decision.createContinuous $"NumberOf{item}" 0. infinity
    ] |> SMap

// Objective.
let objective =
    let objectiveExpression =
        [
            for KeyValue(item, itemDetail) in items ->
                itemDetail.Profit * numberOfItems.[item]
        ] |> List.sum
    Objective.create "MaximizeProfit" Maximize objectiveExpression

// Constraint.
let maxItems =
    [
        for KeyValue(item, itemDetail) in items ->
            Constraint.create $"Max{item}" (numberOfItems.[item] <== itemDetail.MaxNumber)
    ]

let maxWeight =
    let maxWeightExpr =
        [
            for KeyValue(item, itemDetail) in items ->
                (numberOfItems.[item] * itemDetail.Weight)
        ] |> List.sum
    Constraint.create "MaxWeight" (maxWeightExpr <== maxTruckWeight)
    
// Model.
let model =
    Model.create objective
    |> Model.addConstraints maxItems
    |> Model.addConstraint maxWeight
