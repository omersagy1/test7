#!/bin/bash

DEPLOYMENT_DIR="advtest-210413"

echo "Building application..."
elm-make src/App/Main.elm --output=$DEPLOYMENT_DIR/www/index.html

echo "Deploying application..."
cd $DEPLOYMENT_DIR
gcloud app deploy
cd -

echo "Done."