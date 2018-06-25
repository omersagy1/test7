module Game.Constants exposing (..)

import Time


-- Default delay for enqueued messages.
defaultMessageDelay = 1.3 * Time.second

-- Delay for the first message in an event.
firstMessageDelay = 0.3 * Time.second

-- Delay before the appearance of choice
-- buttons. This is in between the message
-- hinting to the user a choice might be made,
-- and the availability of the choice buttons
-- themselves.
choiceButtonsDelay = 0.35 * Time.second

-- The delay after the user makes a choice,
-- before the consequence record's message
-- appears.
postChoiceMessageDelay = 0.5 * Time.second

-- Delay before running the effect of an
-- event.
mutatorDelay = 0.5 * Time.second