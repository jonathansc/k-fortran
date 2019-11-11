import pandas
import matplotlib.pyplot as plot
from sklearn.datasets.samples_generator import make_blobs
import sys
import csv
import random

def visualize2d(filename):
    df = pandas.read_csv(filename, sep='\s+', names=['x', 'y', 'cluster', 'type'], header=None)
    markers = {'c': 'X', 'p': '2'}
    for kind in markers:
        d = df[df.type == kind]
        if kind == 'c':
            plot.scatter(x=d.x, y=d.y, c='black', marker=markers[kind], cmap='Dark2')
        else:
            plot.scatter(x=d.x, y=d.y, c=d.cluster, marker=markers[kind], cmap='Dark2')
    plot.show(block=True)

def generate(filename, groundtruth, n_samples, centers, n_features, random_state):
    X, y = make_blobs(n_samples=n_samples, centers=centers, n_features=n_features, random_state=random_state)

    df = pandas.DataFrame(X)
    df.to_csv(filename, sep=' ', header=None, index=False)
    df['groundtruth'] = y
    df['cl'] = 'p'
    df.to_csv(groundtruth, sep=' ', header=None, index=False)

def main(argv):
    if argv[1] == "visual":
        visualize2d("output/out.dat")
    elif argv[1] == "generate":
        generate("input/in.dat", "input/groundtruth.dat", 10000, 5, 2, random.randint(0,101))
    else:
        print(argv[0] + " visual")

if __name__ == "__main__":
    main(sys.argv)
