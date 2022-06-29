#!/bin/bash

BUNDLE_FILE=$1
shift

BUNDLE_DIR=`mktemp -d`

echo "Extracting bundle archive into $BUNDLE_DIR"

# deletes the temp directory
function cleanup {
  rm -rf "$BUNDLE_DIR"
  echo "Deleted temp working directory $BUNDLE_DIR"
}

# register the cleanup function to be called on the EXIT signal
trap cleanup EXIT

cd $BUNDLE_DIR && tar -xf $BUNDLE_FILE
cd $BUNDLE_DIR/delivery-bundle/

BUNDLE_SCRIPT=$BUNDLE_DIR/delivery-bundle/deliver.lisp
exec sbcl --script $BUNDLE_SCRIPT $@
