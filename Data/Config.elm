module Data.Config exposing (config)


config = """

name: fakeEvent
trigger: time >= 2s
text:
  first line
  second line
consequence: mantriggered


name: mantriggered
text:
  consequence triggered!


name: fakeEvent2
trigger: time >= 10s
text: 
  at least ten seconds passed!


"""