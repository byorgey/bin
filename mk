#!/bin/zsh

DIR=`pwd`

while [[ `print -l Shake.*(.N)` == "" && `pwd` != $HOME ]] do
  cd ..
done

if [[ `pwd` != $HOME ]] 
then
  ghc --make Shake -threaded && ./Shake +RTS -N8 -RTS $1
else
  echo "I can't seem to find a Shake file."
fi

cd $DIR
