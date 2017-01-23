#!/bin/bash

REV=$(git rev-parse HEAD)

rm -rf dist
rm -rf elm-stuff/build-artifacts # To display all the warnings, do not keep old compilation results.
elm make src/Main.elm --warn --output=dist/main.js
cp -r static dist
cp index.html dist

sed -i -- 's~/_compile/src/Main.elm~/main.js~' dist/index.html
sed -i -- 's~runElmProgram~Elm.Main.fullscreen~' dist/index.html
sed -i -- 's~http://localhost:2000/api~https://api.openfisca.fr/api~' dist/index.html

pushd dist
git init
git add .
git commit -m "Build application from commit $REV"
git push --force git@github.com:openfisca/demonstrator.git master:dist
popd

git fetch

echo "On the server do:"
echo "git checkout dist; git fetch; git reset --hard origin/dist"