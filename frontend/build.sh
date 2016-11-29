#!/bin/sh
BUILD_DIR=./static
ENTRY_JS=app.js

rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"
cp ./src/*.tag "$BUILD_DIR"
cp ./src/*.less "$BUILD_DIR"
cp ./src/*.html "$BUILD_DIR"
# webpack
