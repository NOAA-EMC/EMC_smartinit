#!/bin/bash

target=jsmartinit.ecf
for i in {0..9..3}
do 
   ln -s $target jsmartinit_f0${i}.ecf
done

for i in {12..84..3}
do
   ln -s $target jsmartinit_f${i}.ecf
done
