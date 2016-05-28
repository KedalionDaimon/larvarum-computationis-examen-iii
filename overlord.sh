#!/bin/bash

# activate with: rlwrap overlord.sh

# corrected ps-aux-check in memory

# EMPTY OUTPUT SHOULD ALSO NOT BE PARSED.
# EMPTY IS REALLY "", NOT JUST "()".

# TO-DO: THE TRANSMUTATIONS,
# AS WELL AS "READING" AND "WRITING".

# Assume input is in usertoswarm.txt and
# output is in swarmtouser.txt. Assume in
# transformed form they are mutin.txt and
# mutout.txt (for the undetaken "mutations").

echo " :: SYSTEM OPERATIONAL. ENTER AN EMPTY LINE TO TERMINATE. :: "

# Empty the main files that you will operate upon:
echo "pseudo-userinput" > usertoswarm.txt
echo "pseudo-useroutput" > swarmtouser.txt
echo "(pseudo-mutin)" > mutin.txt
echo "(pseudo-mutout)" > mutout.txt
emptyoutput=""

# MAIN I/O LOOP:
while (true)
do

# NOW THE INPUT SHOULD BE RECORDED, AND MUTIN IS TO BE SET.

# Record the input:
echo -n "READ: "
read usertoswarmvar

if [ "$usertoswarmvar" = "$emptyoutput" ]
then
  echo " :: EMPTY INPUT. INTERACTION TERMINATED. :: "
  exit
fi

# MAKE IT REGARD THE LAST MACHINE-HUMAN PAIR:
usertoswarmvar="$previousreply $usertoswarmvar"

echo $usertoswarmvar > usertoswarm.txt

# the below is just needed so the parser does not complain:
echo "(pseudo-mutout)" > mutout.txt

# Transform usertoswarm.txt into mutin.txt:
./parser
# (side effect: it also transfroms
# mutout.txt into swarmtouser.txt)

# Activate each insect of the swarm:
for i in `cat insects.txt` # i.e. insects in that order.
do

  cp ./mutin.txt ./pupa$i/swarminput.txt

  echo "" > ./pupa$i/swarmoutput.txt # TURN ON AGAIN PLAN CLOBBERING
  echo "(NORUN)" > ./pupa$i/swarminsistence.txt # TURN ON AGAIN INSISTENCE CLOBBERING

  cd ./pupa$i
  ./examen & # The BACKGROUND RUN is important!
  cd ..
done

sleep 10 # Safety time buffer - to give swarm a chance to run.

# FOR THE ABOVE, I COULD SSH OUT.

# If swarm IS running:
# ps -e | grep swarm | wc -l SHOULD BE AT LEAST 1
# ps u | grep swarm | wc -l SHOULD BE AT LEAST 2.

# Check whether the swarm has landed again already.
# There seem to be "more civilized" ways to do this,
# either with "wait" or with "GNU/Parallel".
# But this one is very simple.

# Check whether some swarm element is still operating
# and wait for it to terminate
isitrunning=2
while [ $isitrunning != "1" ]
do
 sleep 1

 isitrunning=`ps aux | grep ' ./examen' | wc -l`
done
# FOR THE ABOVE, I COULD GET SSH INFO BACK.


# Now try to see which insect got the first plan:
sleep 2 # this delay is needed so the machines can write their data
# to disk - otherwise it forces the creation of a new machine.

foundplan="false"
planmatch=0
for i in `cat insects.txt` # i.e. insects in that order
do
  insectoutput=`cat ./pupa$i/swarmoutput.txt| tr -d \n`
  # The tr -d part can be commented out.
  # echo "insectoutput = $insectoutput"
  # echo "emptyouptput = $emptyoutput"

  planmatch=$i

  if [ "$insectoutput" != "$emptyoutput" ]
  then
    foundplan="true"
    break
  fi
done

# echo "foundplan = $foundplan" # THAT SHOULD NOT BE FALSE!!!

if [ "$foundplan" = "false" ]
then

  cat ./pupa1/swarminput.txt | tr [:digit:] ' ' | tr [:punct:] ' ' | tr ' ' '\n' | egrep -v '^$' > eachword.txt
  # No | sort | uniq, as I want a weighted sum for the words below: repeated words are more important.

  mostwords=0
  for i in `cat insects.txt` # i.e. insects in that order
  do
    foundmostwords=0
    for j in `cat eachword.txt` # i.e. words
    do

      wordread=`grep $j ./pupa$i/swarmhistory.txt`

      if [ "$wordread" != "" ]
      then
        foundmostwords=`expr $foundmostwords + 1`
      fi
    done

    if [ $foundmostwords -gt $mostwords ]
    then
      mostwords=$foundmostwords
      echo "MOST WORDS: $mostwords"
      newest=$i
      # planmatch=$i # that went a few lines down, differently.
      unused=`tail -1 insects.txt`

      # If the selected is the "last" used, then
      # then the "new" last used is the next-to-last used.
      if [ "$newest" = "$unused" ]
      then
        unused=`tail -2 insects.txt | head -1`
      fi

      planmatch=$unused
    fi
  done

  if [ "$mostwords" = 0 ]
  then
    unused=`tail -1 insects.txt` # replace the oldest unused insect
    newest=`head -1 insects.txt`
    planmatch=$unused # We force the plan to match here.
  fi

  # DEACTIVATE THIS:
  echo " :: FORCING CONSIDERATION WITH $unused EX $newest :: "

  rm ./pupa$unused/swarmdata.txt
  rm ./pupa$unused/swarmhistory.txt
  rm ./pupa$unused/swarminput.txt
  rm ./pupa$unused/swarmoutput.txt

  cp ./pupa$newest/swarmdata.txt ./pupa$unused
  cp ./pupa$newest/swarmhistory.txt ./pupa$unused
  cp ./pupa$newest/swarminput.txt ./pupa$unused
  cp ./pupa$newest/swarmoutput.txt ./pupa$unused

  echo "(DORUN)" > ./pupa$unused/swarminsistence.txt
  cd ./pupa$unused/
  ./examen
  cd ..

  echo "(NORUN)" > ./pupa$unused/swarminsistence.txt
  foundplan="true"
fi

# Cycle the last activated insect:
cat insects.txt | grep -vx $planmatch > tempinsects.txt
echo $planmatch > insects.txt
cat tempinsects.txt >> insects.txt

echo -n " :: MATCHING MACHINE :: "
echo $planmatch

# Copy the plan to transformation:
cp ./pupa$planmatch/swarmoutput.txt ./mutout.txt

if test -s ./mutout.txt # file exists and is not empty
then

# the below is just needed so the parser does not complain:
echo "pseudo-userinput" > usertoswarm.txt
# Transform mutout.txt into swarmtouser.txt:
./parser
# (side effect: it also transfroms
# usertoswarm.txt into mutin.txt)
else

echo "" > swarmtouser.txt

fi

echo -n "REPLY: "
cat swarmtouser.txt
echo ""

# MAKE IT REGARD THE LAST MACHINE-HUMAN PAIR:
read previousreply < swarmtouser.txt
previousreply=`echo -n $previousreply | tr -d '\n'`


# END OF MAIN I/O LOOP:
done

