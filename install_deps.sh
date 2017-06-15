#!/bin/bash

while read pkg; do
	echo Installing "$pkg"
	cabal install "$pkg"
done < deps.txt
