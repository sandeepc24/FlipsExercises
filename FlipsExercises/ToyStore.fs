module ToyStore

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure


(*
    https://www.analyzemath.com/linear_programming/linear_prog_applications.html

    A store sells two types of toys, A and B. 
    
    The store owner pays $8 and $14 for each one unit of toy A and B respectively. 
    
    One unit of toys A yields a profit of $2 while a unit of toys B yields a profit of $3. 

    The store owner estimates that no more than 2000 toys will be sold every month and he does 
    not plan to invest more than $20,000 in inventory of these toys. 
    
    How many units of each type of toys should be stocked in order to maximize his monthly total 
    profit profit?
*)

[<Measure>] type NZD

type ToyDetail = {
    Cost : float<NZD>
    Profit : float<NZD>
}

let monthlyToySale = 2000.
let stockValue = 20_000.00<NZD>

let toys = 
    [
        ("ToyA", { Cost =  8.00<NZD>; Profit = 2.00<NZD> })
        ("ToyB", { Cost = 14.00<NZD>; Profit = 3.00<NZD> })
    ] |> SMap

// Decision.
let numberOfToysToStock =
    [
        for KeyValue(toy, _) in toys ->
            toy, Decision.createContinuous $"Toy{toy}" 0. infinity
    ] |> SMap

// Objective.
let objective =
    let objectiveExpr =
        [
            for KeyValue(toy, toyDetail) in toys ->
                numberOfToysToStock.[toy] * toyDetail.Profit
        ] |> List.sum
    Objective.create "Maximize" Maximize objectiveExpr

// Constraint.
let monthlyToySaleConstraint =
    let monthlyToySaleExpr =
        [
            for KeyValue(toy, _) in toys ->
                numberOfToysToStock.[toy] * 1.
        ] |> List.sum
    Constraint.create "monthlyToySaleConstraint" (monthlyToySaleExpr <== monthlyToySale)

let maxStockValueConstraint =
    let maxStockValueExpr =
        [
            for KeyValue(toy, toyDetail) in toys ->
                numberOfToysToStock.[toy] * toyDetail.Cost
        ] |> List.sum
    Constraint.create "maxStockValueConstraint" (maxStockValueExpr <== stockValue)

// Model.
let model =
    Model.create objective
    |> Model.addConstraint monthlyToySaleConstraint
    |> Model.addConstraint maxStockValueConstraint

