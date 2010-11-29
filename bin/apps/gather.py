#! /usr/bin/env python
import os, sys, fnmatch
import math,numpy
import csv
from string import *
import datetime
from socket import gethostname
from optparse import OptionParser

def main():
	usage = "usage: %prog [options]."
	parser = OptionParser(usage=usage)
	parser.add_option("--host", dest="hostname", help="set hostname to gather for", metavar="HOST")
	parser.add_option("--apps", dest="app_names", help="set filename containing app names to gather for", metavar="APPS")
	(options, args) = parser.parse_args()

	data_dir = os.getcwd()
	
	if options.hostname is not None:
		hostname = options.hostname
	else:
		hostname = gethostname()

	if options.app_names is not None:
		apps = parse_app_names(options.app_names)
	else:
		apps = parse_app_names('app_names')

	for fname in os.listdir(data_dir):
		if fnmatch.fnmatch(fname, '*.times'):
			fname_comps = fname.split('.')
			app = fname_comps.pop(0).lower()
			proc = fname_comps.pop(0)
			proc = proc[1:len(proc)]

			f = open(fname, 'r')
			app_times = f.readlines()
			f.close()
			(min, mean, max) = calc_times(app_times)
			apps[app].append((proc, min, mean, max))
		
	now = datetime.datetime.now()
	out_dir = os.getcwd() + '/' + str(now) + '/'
	os.mkdir(out_dir)
	save(apps, hostname, out_dir)
    
	# clean up
	os.system('mv ' + data_dir + '/*.times ' + '"' +  out_dir + '/"')
	os.system('mv ' + data_dir + '/*.error ' + '"' +  out_dir + '/"')

def parse_app_names(app_names):
 	apps = {}
	f = open(sys.path[0] + '/' + app_names, 'r')
	for line in f:
		app = line.strip()
		apps[app] = []
	f.close() 
	return apps

def calc_times(app_times):
	# drop the first 5 runs
	times = app_times[5:len(app_times)]
	times = [float(t) for t in times]
	
	mean = numpy.mean(times)
	min = numpy.amin(times)
	max = numpy.amax(times)
	
	return (min, mean, max)

def save(apps, hostname, out_dir):
	out = out_dir + 'stats.csv'
	csvf = csv.writer(open(out, 'w'), delimiter=',')

	# first row is hostname followed by app names
	app_names = apps.keys()
	app_names.sort()
	header = app_names[:]
	header.insert(0, hostname)
	csvf.writerow(header)
  
	# remaining rows are in the format:
	# proc-min,time
	# proc-mean,time
	# proc-max,time
	rows = init_rows_for_procs(hostname)

	for app in app_names:
		found = []
		for entry in apps[app]:
			index = proc2index(float(entry[0]))
			
			rows[index].append(entry[1])
			rows[index+1].append(entry[2])
			rows[index+2].append(entry[3])
			found.append(index)
		
		# missing data elements
		for proc in get_procs(hostname):
			index = proc2index(proc)
			if (index not in found):
				rows[index].append('x')
				rows[index+1].append('x')
				rows[index+2].append('x')
	
	for row in rows:
		csvf.writerow(row)

def proc2index(proc):
 if (proc == -1):
	 index = 0
 else:
	 index = int(round(math.log(proc, 2))+1)*3
 
 return index

def get_procs(hostname):
  if (hostname.startswith('tflop')):
    procs = [-1,1,2,4,8,16]
  elif (hostname.startswith('niagara')):
		procs = [-1,1,2,4,8,16,32,64,128]
  else:
    sys.exit('unsupported hostname found')
    
  return procs
	 
def init_rows_for_procs(hostname):
	procs = get_procs(hostname)

	rows = []
	for p in procs:
		if (p == -1):
			name = 'no-exec'
		else:
			name = str(p)

		rows.append([name + '-min'])
		rows.append([name + '-mean'])
		rows.append([name + '-max'])

	return rows

if __name__ == "__main__":
	main()
