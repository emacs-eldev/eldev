#! /bin/sh

SELF_DIR=$(dirname $0)
cd $SELF_DIR

ELDEV_LOCAL=. bin/eldev --setup "(defvar eldev--install-sh t)" install-self "$@"
