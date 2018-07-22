#!/bin/bash

echo "Building application..."
elm-make src/App/Main.elm --output=index.html

echo "Running Dev Server..."
elm-live --pushstate