#!/bin/bash

DEPLOYMENT_DIR="advtest-210413"

echo "Building application..."
elm-make src/App/Main.elm --output=$DEPLOYMENT_DIR/www/main.js
cp index.html $DEPLOYMENT_DIR/www/index.html
cp styles.css $DEPLOYMENT_DIR/www/styles.css

echo "Deploying application..."
cd $DEPLOYMENT_DIR
gcloud app deploy
cd -

echo "Done."