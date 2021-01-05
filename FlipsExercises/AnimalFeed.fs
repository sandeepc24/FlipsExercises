module AnimalFeed

open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure

(*
    https://www.analyzemath.com/linear_programming/linear_prog_applications.html

    A farmer plans to mix two types of food to make a mix of low cost feed for the animals in 
    his farm. A bag of food A costs $10 and contains 40 units of proteins, 20 units of minerals and 
    10 units of vitamins. 
    
    A bag of food B costs $12 and contains 30 units of proteins, 20 units of minerals and 
    30 units of vitamins. 
    
    How many bags of food A and B should the consumed by the animals each day in order to meet the 
    minimum daily requirements of 150 units of proteins, 90 units of minerals and 60 units of 
    vitamins at a minimum cost?
*)

[<Measure>] type NZD
[<Measure>] type Unit

type FeedDetail = {
    Cost : float<NZD>
    Protein : float<Unit>
    Mineral : float<Unit>
    Vitamin : float<Unit>
}

let feeds =
    [
        ("Food A", { Cost = 10.00<NZD>; Protein = 40.0<Unit>; Mineral = 20.0<Unit>; Vitamin = 10.0<Unit> })
        ("Food B", { Cost = 12.00<NZD>; Protein = 30.0<Unit>; Mineral = 20.0<Unit>; Vitamin = 30.0<Unit> })
    ] |> SMap

let minProtein = 150.0<Unit>
let minMineral =  90.0<Unit>
let minVitamin =  60.0<Unit>

// Decision.
let feedToConsume =
    [
        for KeyValue(feed, feedDetail) in feeds ->
            feed, Decision.createContinuous $"Feed Type: {feed}" 0. infinity
    ] |> SMap

// Objective.
let objectve =
    let objectiveExpr =
        [
            for KeyValue(feed, feedDetail) in feeds ->
                feedToConsume.[feed] * feedDetail.Cost
        ] |> List.sum
    Objective.create "MinimizeCost" Minimize objectiveExpr
        
// Constraints.
let minProteinConstraint =
    let minProteinExpr =
        [
            for KeyValue(feed, feedDetail) in feeds ->
                feedToConsume.[feed] * feedDetail.Protein
        ] |> List.sum
    Constraint.create "minProtein" (minProteinExpr >== minProtein)

let minMineralConstraint =
    let minMineralExpr =
        [
            for KeyValue(feed, feedDetail) in feeds ->
                feedToConsume.[feed] * feedDetail.Mineral
        ] |> List.sum
    Constraint.create "minMineral" (minMineralExpr >== minMineral)

let minVitaminConstraint =
    let minVitaminExpr =
        [
            for KeyValue(feed, feedDetail) in feeds ->
                feedToConsume.[feed] * feedDetail.Vitamin
        ] |> List.sum
    Constraint.create "minVitamin" (minVitaminExpr >== minVitamin)
    
let model =
    Model.create objectve
    |> Model.addConstraint minProteinConstraint
    |> Model.addConstraint minMineralConstraint
    |> Model.addConstraint minVitaminConstraint