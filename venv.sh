#!/bin/sh

if [ -z "$SHELL" ]; then
	SHELL=/bin/sh
fi

if [ ! -d _venv ]; then
	virtualenv _venv
	. _venv/bin/activate
	pip install sphinx
	exec $SHELL
else
	. _venv/bin/activate
	exec $SHELL
fi
