module Game.Condition exposing (..)

import Time exposing (Time)

import Game.ActionName as ActionName


type Condition = Pure PureCondition
                 | Chance Float
                 | And Condition Condition
                 | Or Condition Condition
                 | Not Condition

type PureCondition = GameTimePassed Time
                     | Never
                     | Always
                     | ResourceAmountAbove String Int 
                     | ResourceActive String
                     | FireExtinguished
                     | FireStoked
                     | ActionPerformed ActionName.Name
                     | MilestoneReached String
                     | TimeSinceMilestone String Time
                     | MilestoneAtCount String Int
                     | MilestoneGreaterThan String Int
