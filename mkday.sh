#! /usr/bin/env bash

for var in $(seq 9 99); do
    mkdir "prob$var"
    cp "prob1/dune" "prob$var/dune"
    # https://stackoverflow.com/questions/16745988/sed-command-with-i-option-in-place-editing-works-fine-on-ubuntu-but-not-mac
    sed -i '' "s/prob1)/prob$var)/g" "prob$var/dune" 
    touch "prob$var/main.ml"
    echo "let () = assert true;;" > "prob$var/main.ml"
done 

