import sys
import random

dir_name = "/Users/EleanorLeung/Documents/thesis"

x_pos = 8

def change_velocity(particle_num: int, new_x: float, new_y: float, new_z: float):
    with open(f'{dir_name}/week9/envs/{particle_num}/gpar.para', 'r') as file:
        data = file.readlines()

    data[1] = f'{new_x} {new_y} {new_z} ### 3D velocity of a companion galaxy\n'

    data[2] = '0.0 0.0 ### Disk inclination angles (theta,phai)\n'

    with open(f'{dir_name}/week9/envs/{particle_num}/gpar.para', 'w') as file:
        file.writelines(data)

    with open(f'{dir_name}/week9/envs/{particle_num}/results_{x_pos}/params.txt', 'a+') as file:
        file.writelines(data)


if __name__ == '__main__':
    change_velocity(
        particle_num=sys.argv[1],
        new_x=sys.argv[2],
        new_y=sys.argv[3],
        new_z=sys.argv[4]
    )