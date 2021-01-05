module Fund

open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure

(*
    https://www.analyzemath.com/linear_programming/linear_prog_applications.html

    John has $20,000 to invest in three funds F1, F2 and F3. 
    Fund F1 is offers a return of 2% and has a low risk. 
    Fund F2 offers a return of 4% and has a medium risk. 
    Fund F3 offers a return of 5% but has a high risk. 
    To be on the safe side, John invests no more than $3000 in F3 and at 
    least twice as much as in F1 than in F2. 
    
    Assuming that the rates hold till the end of the year, 
    what amounts should he invest in each fund in order to maximize the year end return?
*)

[<Measure>] type NZD
[<Measure>] type P // Percent

type FundDetail = {
    Return : float<P>
}

let funds = 
    [
        ("F1", { Return = 2.0<P> })
        ("F2", { Return = 4.0<P> })
        ("F3", { Return = 5.0<P> })
    ] |> SMap

let amountToInvest = 20_000.00<NZD>
let maxAmountToInvestInF3 = 3_000.00<NZD>

// Decision.
let fundsToInvest = 
    [
        for KeyValue(fund, fundDetail) in funds ->
            fund, Decision.createContinuous $"MaxInvestmentIn{fund}" 0. infinity
    ] |> SMap

// Objective.
let objective =
    let objectiveExpr =
        [
            for KeyValue(fund, fundDetail) in funds ->
                fundsToInvest.[fund] * fundDetail.Return
        ] |> List.sum
    Objective.create "MaximizeInvestment" Maximize objectiveExpr

// Constraint.
let maxFundConstraint =
    let maxFundExpr =
        [
            for KeyValue(fund, fundDetail) in funds ->
                fundsToInvest.[fund] * 1.<NZD>
        ] |> List.sum
    Constraint.create "maxFundConstraint" (maxFundExpr <== amountToInvest)

let maxF3Constraint =
    Constraint.create "maxF3Constraint" (fundsToInvest.["F3"] * 1.<NZD> <== maxAmountToInvestInF3)

let maxF1F2Constraint =
    let maxF1F2Expr =
        [
            for KeyValue(fund, fundDetail) in funds ->
                let amt =
                    if fund = "F3" then
                        0.<NZD>
                    else
                        1.<NZD>
                fundsToInvest.[fund] * amt
        ] |> List.sum
    let maxF1F2Amount = (amountToInvest - maxAmountToInvestInF3)
    Constraint.create "maxF1F2Constraint" (maxF1F2Expr <== maxF1F2Amount)

let F1TwiceAsMuchAsF2Constraint =
    Constraint.create "F1TwiceAsMuchAsF2Constraint" (fundsToInvest.["F1"] == ( 2. * fundsToInvest.["F2"]))
    

// Model.
let model =
    Model.create objective
    |> Model.addConstraint maxFundConstraint
    |> Model.addConstraint maxF3Constraint
    |> Model.addConstraint maxF1F2Constraint
    |> Model.addConstraint F1TwiceAsMuchAsF2Constraint