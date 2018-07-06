module Game.Condition exposing (..)

import Time exposing (Time)


type Condition = P PureCondition
                 | R RandomCondition

type PureCondition = GameTimePassed Time
                     | Never
                     | ResourceAmountAbove String Int 
                     | ResourceActive String
                     | FireExtinguished
                     | FireStoked
                     | ActionPerformed String
                     | MilestoneReached String
                     | TimeSinceMilestone String Time
                     | MilestoneAtCount String Int
                     | MilestoneGreaterThan String Int

type RandomCondition = Chance Float
                       | And Condition Condition
                       | Or Condition Condition
                       | Not Condition

