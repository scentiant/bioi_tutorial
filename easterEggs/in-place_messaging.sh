#!/bin/bash

# Michael E. Sparks, 10-19-16

if [ -n "$1" ]; then
  iter=$1
else
  iter=1 # default value
fi

declare -a firsts=(H C M J)
declare -a seconds=("World" "Moon" "Sun")
delay="1s" # argument to the sleep utility

for anon in `seq 1 $iter`; do
  for i in "${seconds[@]}"; do
    # Clear the line, print message string and
    # then return the carriage without issuing
    # a line feed command. (It's not coincidental
    # that this seems awfully similar to how a real,
    # mechanical typewriter works:
    # https://en.wikipedia.org/wiki/Teletype_Model_33 )

    echo -en "\e[K ello $i\r"

    for j in `seq 0 3`; do
      echo -en "${firsts[$j]}\r"
      sleep $delay
    done

  done
done

# Ringing the terminal bell provides
# a soundtrack for this ``motion picture."
# (More practically, it helps alert you that
# a scripted procedure has completed, needs
# attention, etc.)

echo -e "Happy Trails!\a"

exit 0
