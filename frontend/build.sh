#!/bin/sh
BUILD_DIR=./static
ENTRY_JS=app.js

rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"
cp ./src/index.html "$BUILD_DIR"
webpack
