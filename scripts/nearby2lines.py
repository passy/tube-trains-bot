#!/usr/bin/env python3

import json
import sys
import csv

def main(filename):
    with open(filename, 'r') as fp:
        f = json.load(fp)
        lines = set()
        for r in f['elements']:
            lines = lines.union(r['route_ids'])

        with open(filename + "-lines.csv", 'w') as op:
            writer = csv.writer(op, dialect='unix')
            for l in lines:
                writer.writerow([l, l])

if __name__ == "__main__":
    main(sys.argv[1])
