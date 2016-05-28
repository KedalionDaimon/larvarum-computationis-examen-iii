#!/bin/bash

# The purpose of this program is to re-link the swarm to a new version.

for i in {1..20} # say how many prototype insects you will want
do
  rm ./pupa$i/examen

  cd ./pupa$i
  ln ../examen ./examen
  cd ..

done
