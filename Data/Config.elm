config = """

id: init
{ resource wood 10s }
{ resource gold 30s }


id: intro
if time = 0
welcome to the game.
{ gold +100 }
here is some gold to start you off.


if 20s after intro
i'm going to take some of this gold back.
{ gold -20 }
sorry about that.
you'll get more soon.

"""