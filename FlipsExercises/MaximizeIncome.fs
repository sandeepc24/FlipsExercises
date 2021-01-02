module MaximizeIncome

open Flips
open Flips.Types

// https://math.libretexts.org/Bookshelves/Applied_Mathematics/Book%3A_Applied_Finite_Mathematics_(Sekhon_and_Bloom)/03%3A_Linear_Programming_-_A_Geometric_Approach/3.01%3A_Maximization_Applications

let job1HourlyRate = 40.
let job2hourlyRate = 30.

// Decision variables.
let numberOfHoursAtJob1 = Decision.createContinuous "numberOfHoursAtJob1" 0. infinity
let numberOfHoursAtJob2 = Decision.createContinuous "numberOfHoursAtJob2" 0. infinity

// Objective Expression.
let maximizeIncomeObjectiveExpression = job1HourlyRate * numberOfHoursAtJob1 + job2hourlyRate * numberOfHoursAtJob2
let objective = Objective.create "MaximizeIncome" Maximize maximizeIncomeObjectiveExpression

// Constraint.
let maxWorkPerWeek = Constraint.create "maxWorkPerWeek" (numberOfHoursAtJob1 + numberOfHoursAtJob2 <== 12.)
let maxPreparationPerWeek = Constraint.create "maxPreparationPerWeek" ((2. * numberOfHoursAtJob1) + numberOfHoursAtJob2 <== 16.)

let model = 
    Model.create objective
    |> Model.addConstraint maxWorkPerWeek
    |> Model.addConstraint maxPreparationPerWeek
