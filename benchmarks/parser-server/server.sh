#!/usr/bin/env bash
HASKELL_PORT=5001 

run_haskell() {
	cd haskell ;
	PORT=$1 stack run 
}

echo "Starting servers..." 
echo ""
echo "Haskell Port: $HASKELL_PORT"
echo ""
run_haskell $HASKELL_PORT
