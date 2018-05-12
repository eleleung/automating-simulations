from scipy.spatial import distance
import numpy as np
import sys


dir_name = "/Users/EleanorLeung/Documents/thesis"

x_pos = 8

image_height = 50
image_width = 50

def get_target_galaxy():
    data = np.zeros(2500)

    with open(f"{dir_name}/week2/ring_example.dat") as sample:
        lines = sample.readlines()

    image_counter = 0
    pixel_counter = -1
    for line in lines:
        pixel_counter += 1

        val = line.strip().split()
        data[pixel_counter] = float(val[0])
        if pixel_counter == (image_height * image_width - 1):
            pixel_counter = -1
            image_counter += 1

    return data


def get_simulation_galaxy(particle_id):
    data = np.zeros(2500)

    counter = 0
    line_num = 0

    with open(f"{dir_name}/week9/envs/{particle_id}/2df.dat") as input_file:
        for line in input_file:
            if counter > 67500 and counter < 70001:
                data[line_num] = (abs(float(line)))
                line_num += 1
            counter += 1

    return data


def calculate_cosine_similarity(particle_id):
    target = get_target_galaxy()

    simulation = get_simulation_galaxy(particle_id)

    dist = distance.cosine(target, simulation)

    print(dist, file=open(f"../week9/envs/{particle_id}/results_{x_pos}/results.txt", "a+"))

    print(dist)

    return dist


if __name__ == '__main__':
    calculate_cosine_similarity()