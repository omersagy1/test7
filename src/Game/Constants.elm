module Game.Constants exposing (..)

import Time exposing (Time)


-- Default delay for enqueued messages.
defaultMessageDelay : Time
defaultMessageDelay = 1.8 * Time.second

-- Delay for the first message in an event.
firstMessageDelay : Time
firstMessageDelay = 0.3 * Time.second

-- Delay for the first message if there are
-- already other events waiting in the queue
-- to be executed. This is long in order to
-- space out events so they don't feel jumbled 
-- together.
firstMessageNonEmptyQueueDelay : Time
firstMessageNonEmptyQueueDelay = 2 * Time.second

-- Delay before the appearance of choice
-- buttons. This is in between the message
-- hinting to the user a choice might be made,
-- and the availability of the choice buttons
-- themselves.
choiceButtonsDelay : Time
choiceButtonsDelay = 0.35 * Time.second

-- The delay after the user makes a choice,
-- before the consequence record's message
-- appears.
-- TODO: This is not being used yet.
postChoiceMessageDelay : Time
postChoiceMessageDelay = 0.5 * Time.second

-- Delay before running the effect of an
-- event.
mutatorDelay : Time
mutatorDelay = 0.5 * Time.second

-- Delay before a story event triggered 
-- by a goto statement.
triggerStoryEventDelay : Time
triggerStoryEventDelay = 1.0 * Time.second


fastForwardFactor : Float
fastForwardFactor = 3