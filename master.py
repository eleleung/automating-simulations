from multiprocessing import Process
import os

inputs = ['1', '2', '3', '4', '5', '6', '7']
script_path = 'run.sh'

def run(script, name):
    os.system(f'sh {script} {name}')


if __name__ == '__main__':
    processes = []

    for i in range(0, 14):
        for infile in inputs:
            p = Process(target=run, args=(script_path, infile))
            processes.append(p)

        for p in processes:
            p.start()

        for p in processes:
            p.join()

        processes = []

        print(f'finished iteration {i}')


