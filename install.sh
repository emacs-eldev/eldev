#! /bin/sh

SELF_DIR=$(dirname $0)
cd $SELF_DIR

EMAKE_LOCAL=. bin/emake --setup "(defvar emake--install-sh t)" install-self "$@"
