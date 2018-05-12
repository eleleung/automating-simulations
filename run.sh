#!/usr/bin/env bash

dir="/Users/EleanorLeung/Documents/thesis"

num_ring=0

num=$1

x_pos=5

run_simulation() {

    echo ${num}

    python3 gpar_tinker.py ${num}

    cd "$dir/week8/envs/$num/"

    ./runre1

    echo "finished running sim"

    ./ddat

    echo "selected 12th time step"

    ./dens

    echo "converted into 50x50 image"

    cd "$dir/week5/"

    python3 cnn_galaxy_classification.py ${num}

    echo "classified"

    cd "../week8/envs/$num"
    raw_classifications=$(cat "$dir/week8/envs/$num/results_$x_pos/classification_results.txt")
    parsed_classifications=$(echo "${raw_classifications:1:${#raw_classifications}-2}")

    IFS=', ' read -r -a array <<< "$parsed_classifications"

    for element in "${array[@]}"
    do
        if (( $element == 1 ))
            then
                (( num_ring += 1 ))
        fi
    done

    # if majority == ring, stop. If not, continue
    if (( $num_ring > 90 ))
    then
       classification=1

       echo "found ring!!!!"

       python3 cosine_similarity.py ${num}

    else
        echo "0" >> "$dir/week8/envs/$num/results_$x_pos/results.txt"
    fi
}

run_simulation