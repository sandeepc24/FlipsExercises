module NumberOfTrucksToHire

open FSharp.Data.UnitSystems.SI
open Flips
open Flips.Types

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

[<Measure>] type m3 = UnitNames.meter^3
type CapacityType = | Refrigerated of float<m3> | NonRefrigerated of float<m3>

// Decision vars.
let typeATrucks = Decision.createContinuous "typeATrucks" 0. infinity
let typeBTrucks = Decision.createContinuous "typeBTrucks" 0. infinity

// Objective expression.
let objectiveExpression = 30. * typeATrucks + 40. * typeBTrucks
let objective = Objective.create "MinimizeTransportCost" Minimize objectiveExpression

// Constraint.
let refrigeratedStock = Constraint.create "refrigeratedStock" (20. * typeATrucks + 30. * typeBTrucks == 3000.)
let nonRefrigeratedStock = Constraint.create "nonRefrigeratedStock" (40. * typeATrucks + 30. * typeBTrucks == 4000.)

let model =
    Model.create objective
    |> Model.addConstraint refrigeratedStock
    |> Model.addConstraint nonRefrigeratedStock
