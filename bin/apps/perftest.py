#!/usr/local/bin/python
import csv, sys
from string import *
from optparse import OptionParser
import multiprocessing
import os
import math
from socket import gethostname

params = {}

def main():
	usage = "usage: %prog [options] app1 app2 ..."
	parser = OptionParser(usage=usage)
	parser.add_option("-p", "--procs", dest="procs", help="set processors to test (e.g. 1,2,4,8,16)", metavar="PROCS")
	parser.add_option("-n", "--native", action="store_false", dest="native", help="disables native lib calls", default=True)

	(options, args) = parser.parse_args()

	if len(args) < 1:
		parser.error("must specify at least one application")
		
	if options.procs is not None:
		procnumbers = split(options.procs, ",")
	else:
		procnumbers = range(0, int(round(math.log(multiprocessing.cpu_count(), 2))) + 1)
		for i in range(len(procnumbers)):
			procnumbers[i] = 2**procnumbers[i]
		procnumbers.insert(0, -1)
	
	os.system("rm *.times.csv")
	
	print "Processing for " + `procnumbers` + " processors."

	loadParams()

	for app in args:
		if app.find("gpu") < 0:
			for procnum in procnumbers:
				launchApp(app, int(procnum), options)
		else:
			print launchApp(app, 1, options)

def setApplicationVariables(procnum, nativeOps):
	if procnum == -1:
		optsString = "-DenableDebugOptions=true -DbypassExecutor=true"
		os.putenv("DELITE_NUM_THREADS", `1`)
	else:
		optsString = ""
		os.putenv("DELITE_NUM_THREADS", `procnum`)

	if nativeOps:
		print "Enabling native ops."
		optsString = optsString + " -DuseNativeLibs=true"
	else:
		print "Disabling native ops."
		optsString = optsString + " -DuseNativeLibs=false"
	os.putenv("JAVA_OPTS", os.getenv("JAVA_OPTS", "") + " " + optsString)
		
def launchApp(app, procnum, options):
	setApplicationVariables(procnum, options.native)
	
	
	os.putenv("DELITE_BASE", sys.path[0] + "/../../")
	#print(sys.path[0] + "/" + app + params[app])
	os.system(sys.path[0] + "/" + app + params[app])

def isTflop():
	hostname = gethostname()
	if (hostname.startswith('tflop')):
		return True
	else:
		return False

def loadParams():
	if (isTflop()):
		hostname = 'tflop'
	else:
		hostname = 'niagara'
		
	f = open(sys.path[0] + '/' + 'datasets.' + hostname, 'r')
	for line in f:
		settings = line.split('|')
		app = settings.pop(0)
		app_params = ''
		for param in settings:
			param = expand(param)
			app_params = app_params + ' ' + param
		params[app] = app_params
	f.close()

def expand(param):
	DELITE_BASE = sys.path[0] + '/../../'
	if (param[0] == '$'):
		return DELITE_BASE + param[1:len(param)]
	else:
		return param

if __name__ == "__main__":
	main()
