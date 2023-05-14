#!/bin/bash

TMP_OUT=$(mktemp)
TMP_ERR=$(mktemp)

for file in ./ExamplePrograms/good/*.prg
do
	./interpreter $file > $TMP_OUT 2> $TMP_ERR

	if diff ${file%.prg}.out $TMP_OUT >/dev/null
	then echo "$file: OK"
	else echo "$file: ERROR IN OUTPUT"
	fi

	if diff ${file%.prg}.err $TMP_ERR >/dev/null
	then echo "$file: OK"
	else echo "$file: ERROR IN ERROR"
	fi

	echo ""
done

for file in ./ExamplePrograms/bad/*.prg
do
	./interpreter $file > $TMP_OUT 2> $TMP_ERR

	if diff ${file%.prg}.out $TMP_OUT >/dev/null
	then echo "$file: OK"
	else echo "$file: ERROR IN OUTPUT"
	fi

	if diff ${file%.prg}.err $TMP_ERR >/dev/null
	then echo "$file: OK"
	else echo "$file: ERROR IN ERROR"
	fi

	echo ""
done