module FoodTruck

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
    Weight : float
}
let itemList = [
    "Hamburger", { Profit = 1.50<NZD/Item>; MaxNumber = 300.; Weight = 0.5 }
    "HotDog", { Profit = 1.20<NZD/Item>; MaxNumber = 200.; Weight = 0.4 }
]
let items = itemList |> SMap.ofList

let maxTruckWeight = 200.

// Decision vars.
let numberOfItem = 
    [
        for KeyValue(name, _) in items -> 
            name, Decision.createContinuous (sprintf "NumberOf%A" name) 0. infinity
    ] |> SMap

let objectiveExpression = 
    let itemProfit = itemList |> List.map (fun (x, y) -> x, y.Profit) |> SMap
    sum (itemProfit .* numberOfItem)
let objective = Objective.create "MaximizeProfit" Maximize objectiveExpression

// Constraints.
let maxItemConstraints =
    [
        for KeyValue(name, item) in items ->
            Constraint.create (sprintf "MaxOf%s" name) (numberOfItem.[name] <== item.MaxNumber)
    ]

let maxWeight =
    let weightExpression =
        [
            for KeyValue(name, item) in items ->
                item.Weight * numberOfItem.[name]
        ] |> List.sum
    Constraint.create "MaxWeight" (weightExpression <== maxTruckWeight)

// Model.
let model =
    Model.create objective
    |> Model.addConstraints maxItemConstraints
    |> Model.addConstraint maxWeight
    