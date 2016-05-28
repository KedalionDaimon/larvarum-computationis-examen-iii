#!/bin/bash

# The purpose of this program is to initialise a new swarm.
# It also determines the size of the swarm.

rm insects.txt
rm -r pupa*
# redirect errors to /dev/null

for i in {1..20} # say how many prototype insects you will want
do
  echo $i >> insects.txt
  mkdir ./pupa$i
  cp ./swarmdata.txt ./swarmhistory.txt ./swarminsistence.txt ./swarminput.txt ./swarmoutput.txt ./pupa$i

  cd ./pupa$i
  ln ../examen ./examen
  cd ..

  chmod +x ./pupa$i/examen
done

