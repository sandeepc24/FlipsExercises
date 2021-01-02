module Gadgets

open Flips
open Flips.Types
open Flips.SliceMap

// https://math.libretexts.org/Bookshelves/Applied_Mathematics/Book%3A_Applied_Finite_Mathematics_(Sekhon_and_Bloom)/03%3A_Linear_Programming_-_A_Geometric_Approach/3.01%3A_Maximization_Applications

module Simple =
    let regularGadgetProfit = 20.
    let premiumGadgetProfit = 30.

    // Decision variables.
    let regularGadgetsToMake = Decision.createContinuous "regularGadgetsToMake" 0. infinity
    let premiumGadgetsToMake = Decision.createContinuous "premiumGadgetsToMake" 0. infinity

    // Objective expression.
    let objectiveExpression = regularGadgetProfit * regularGadgetsToMake + premiumGadgetProfit * premiumGadgetsToMake
    let objective = Objective.create "MaximizeProfit" Maximize objectiveExpression

    // Constraint.
    let maxGadget = Constraint.create "maxGadget" (regularGadgetsToMake + premiumGadgetsToMake <== 7.)
    let assemblyTime = Constraint.create "assemblyTime" (2. * regularGadgetsToMake + premiumGadgetsToMake <== 12.)
    let finishingTime = Constraint.create "finishingTime" (regularGadgetsToMake + 2. * premiumGadgetsToMake <== 12.)

    let model =
        Model.create objective
        |> Model.addConstraint maxGadget
        |> Model.addConstraint finishingTime
        |> Model.addConstraint assemblyTime

module Maps =
    let gadgets = [ "regular"; "premium" ]
    let gadgetProfit = [ "regular", 20.; "premium", 30. ] |> Map
    let gadgetAssemblyTime = [ "regular", 2.; "premium", 1. ] |> Map 
    let gadgetFinishingTime = [ "regular", 1.; "premium", 2. ] |> Map

    // Decision variables.
    let gadgetsToMakeDecs = 
        DecisionBuilder "gadgetsToMake" {
            for gadget in gadgets -> Continuous(0., infinity)
        } |> Map

    // Objective expression.
    let objectiveExpression =
        [
            for KeyValue(gadget, decVar) in gadgetsToMakeDecs ->
                gadgetProfit.[gadget] * decVar
        ] |> List.sum
    let objective = Objective.create "MaximizeProfit" Maximize objectiveExpression

    // Constraint.
    let maxDailyCapacityExpression =
        [
            for KeyValue(gadget, decVar) in gadgetsToMakeDecs ->
                1. * decVar
        ] |> List.sum
    let assemblyTimeExpr =
        [
            for KeyValue(gadget, decVar) in gadgetsToMakeDecs ->
                gadgetAssemblyTime.[gadget] * decVar
        ] |> List.sum

    let finishTimeExpr =
        [
            for KeyValue(gadget, decVar) in gadgetsToMakeDecs ->
                gadgetFinishingTime.[gadget] * decVar
        ] |> List.sum

    let maxGadget = Constraint.create "maxGadget" (maxDailyCapacityExpression <== 7.)
    let assemblyTime = Constraint.create "assemblyTime" (assemblyTimeExpr <== 12.)
    let finishingTime = Constraint.create "finishingTime" (finishTimeExpr <== 12.)

    let model =
        Model.create objective
        |> Model.addConstraint maxGadget
        |> Model.addConstraint finishingTime
        |> Model.addConstraint assemblyTime
