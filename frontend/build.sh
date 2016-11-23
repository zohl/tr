#!/bin/sh
BUILD_DIR=./static
ENTRY_JS=app.js

rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"
cp ./src/*.tag "$BUILD_DIR"
cp ./src/*.css "$BUILD_DIR"
# webpack
