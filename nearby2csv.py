#!/usr/bin/env python3

import json
import sys
import csv

def main(filename):
    with open(filename, 'r') as fp:
        with open(filename + ".csv", 'w') as op:
            f = json.load(fp)
            writer = csv.writer(op)
            for r in f['elements']:
                writer.writerow([r['id'], r['name']])

if __name__ == "__main__":
    main(sys.argv[1])
