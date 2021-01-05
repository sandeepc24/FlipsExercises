module PCStore

open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure

(*
    https://www.analyzemath.com/linear_programming/linear_prog_applications.html

    Each month a store owner can spend at most $100,000 on PC's and laptops. 
    
    A PC costs the store owner $1000 and a laptop costs him $1500. 
    Each PC is sold for a profit of $400 while laptop is sold for a profit of $700. 

    The store owner estimates that at least 15 PC's but no more than 80 are sold each month. 
    He also estimates that the number of laptops sold is at most half the PC's. 
    
    How many PC's and how many laptops should be sold in order to maximize the profit?

*)

[<Measure>] type NZD

type StockDetail = {
    Cost : float<NZD>
    Profit : float<NZD>
}

let stock =
    [
        ("PC"    , { Cost = 1000.00<NZD>; Profit = 400.00<NZD> })
        ("Laptop", { Cost = 1500.00<NZD>; Profit = 700.00<NZD> })
    ] |> SMap

let maxSpend = 100_000.00<NZD>

// Decision.
let stockToSell =
    [
        for KeyValue(stock, stockDetail) in stock ->
            stock, Decision.createContinuous $"QtyToSellOf{stock}" 0. infinity
    ] |> SMap

// Objective.
let objective =
    let objExpr =
        [
            for KeyValue(stock, stockDetail) in stock ->
                stockToSell.[stock] * stockDetail.Profit
        ] |> List.sum

    Objective.create "MaximizeProfit" Maximize objExpr

// Constraint.
let maxSpendConstraint =
    let maxSpendExpr =
        [
            for KeyValue(stock, stockDetail) in stock ->
                stockToSell.[stock] * stockDetail.Cost
        ] |> List.sum
    Constraint.create "maxSpendConstraint" (maxSpendExpr <== maxSpend)

let maxPCSoldConstraint =
    Constraint.create "maxPCSoldConstraint" (stockToSell.["PC"] <== 80.)

let maxLaptopSoldConstraint =
    Constraint.create "maxLaptopSoldConstraint" (stockToSell.["Laptop"] <== stockToSell.["PC"] * 0.5 )

let model =
    Model.create objective
    |> Model.addConstraint maxSpendConstraint
    |> Model.addConstraint maxPCSoldConstraint
    |> Model.addConstraint maxLaptopSoldConstraint