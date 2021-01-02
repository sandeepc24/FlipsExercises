module AirelineTickets

open Flips
open Flips.Types

// https://courses.lumenlearning.com/sanjacinto-finitemath1/chapter/reading-meeting-demands-with-linear-programming/
(*

An airline offers coach and first-class tickets. For the airline to be profitable, it must sell a 
minimum of 25 first-class tickets and a minimum of 40 coach tickets. The company makes a profit 
of $225 for each coach ticket and $200 for each first-class ticket. At most, the plane has a 
capacity of 150 travelers. How many of each ticket should be sold in order to maximize profits?

*)

let ticketClass = [ "coach"; "firstClass" ]
let ticketProfit = [ "coach", 225.; "firstClass", 200. ] |> Map
let minTicketsToSell = [ "coach", 40.; "firstClass", 25. ] |> Map

// Decision variables.
let ticketsToSellDecs = 
    DecisionBuilder "ticketsToSell" {
        for ticket in ticketClass -> Continuous(minTicketsToSell.[ticket], infinity)
    } |> Map

// Objective expression.
let objectiveExpression =
    [
        for KeyValue(ticket, decVar) in ticketsToSellDecs ->
            ticketProfit.[ticket] * decVar
    ] |> List.sum
let objective = Objective.create "MaximizeProfit" Maximize objectiveExpression

// Constraints.
let aircraftCapacityExpression =
    [
        for KeyValue(_, decVar) in ticketsToSellDecs ->
            1. * decVar
    ] |> List.sum
let aircraftCapacityConstraint = Constraint.create "aircraftCapacity" (aircraftCapacityExpression <== 150.)

let model =
    Model.create objective
    |> Model.addConstraint aircraftCapacityConstraint
