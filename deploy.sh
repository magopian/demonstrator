#!/bin/bash

rm -rf dist
elm-make src/Main.elm --warn --output=dist/main.js
cp -r static dist
cp index.html dist
sed -i -- 's~/_compile/src/Main.elm~/main.js~' dist/index.html
sed -i -- 's~runElmProgram~Elm.Main.fullscreen~' dist/index.html
cd dist
git init
git add .
git commit -m "Deploy to Github Pages"
git push --force git@github.com:cbenz/demonstrator.git master:gh-pages