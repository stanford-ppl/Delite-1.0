#! /usr/bin/env python
import csv, sys
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np
from string import *

for filename in sys.argv[1:]:
	try:
		component = split(filename, ".")[0]
		f = open(filename, 'r')

		# Check to see if the file starts with headers or data:
		dialect = csv.Sniffer().has_header(f.read(1024))
		f.seek(0)
		reader = csv.reader(f)

		# Assign the data to series via a dict
		if dialect is True:
			reader.next() # Move down a line to skip headers
		else:
			pass

		series_dict = {}
		for row in reader:
			i = 0
			for column in row:
				i += 1
				if series_dict.has_key(i):
					try:
						series_dict[i].append(float(column))
					except ValueError:
						pass
				else:
					series_dict[i] = [float(column)]

		plt.clf()
		# Plot each data series
		num_cols = len(series_dict)
		i = 1
		while i < num_cols:
			plt.plot(series_dict[i], series_dict[i+1], '-o')
			i += 2 

		plt.title(component)
		plt.ylabel("Execution Time [s]")
		plt.xlabel("Threads")
		plt.ylim(ymin=0)
		plt.savefig(component + ".png", format='png')

		f.close()
	except:
		print "Error plotting", filename
