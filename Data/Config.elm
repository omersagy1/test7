module Data.Config exposing (config)


config = """

name: fakeEvent
Condition: time >= 2s
text:
  first line
  second line
consequence: manConditioned


name: manConditioned
text:
  consequence Conditioned!


name: fakeEvent2
Condition: time >= 10s
text: 
  at least ten seconds passed!


"""