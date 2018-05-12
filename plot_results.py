from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import numpy as np

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

inputs = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

dir_name = "/Users/EleanorLeung/Documents/thesis/week9/envs"

x = []
y = []
z = []
c = []

def read_data(num):

    with open(f'{dir_name}/{num}/results_5/params.txt') as params_file:
        params_data = params_file.readlines()

    for velocity in params_data[1::3]:
        v_comp = velocity.split(" ")
        x.append(float(v_comp[0]))
        y.append(float(v_comp[1]))
        z.append(float(v_comp[2]))

    with open(f'{dir_name}/{num}/results_5/results.txt') as results_file:
        results_data = results_file.readlines()

    for result in results_data:
        c.append(float(result))

    print(f'finished {num}')


def plot():
    sp = ax.scatter(x, y, z, c=c, cmap=plt.get_cmap("bwr"))
    plt.colorbar(sp)
    plt.show()


if __name__ == '__main__':
    for i in inputs:
        read_data(i)

    plot()
