#!/usr/bin/env python

from __future__ import print_function

import sys, re, os, stat
from os import listdir,chmod, unlink
from os.path import isfile, join
from shutil import copyfile
from itertools import tee, islice, izip_longest
import getopt

#
# Global lists

# files that were marked as read-only
nowrite_files=[]

# Some vars
VERBOSE=1
DRYRUN=False
processed_suf="mattdebug_orig"
FUNC_EXCLUDE_LIST=['mattdebug', 'fastdebug', 'hostnm', 'getpid', 'qqexit', \
                   'mxma8', 'inverse', \
                   'vsexp', 'vssqrt', 'vlog', 'vexp', 'vcos', 'vsin', 'vexp', 'vasin', 'vatan', 'vrec', 'vscos', 'vssin','vsrec', 'vsqrt', 'vslog', 'vatan2', \
                   'hines_intgrl', \
                   'serxst', 'serget' \
                   'newdate', 'datec', \
                   'tmg_stop', 'tmg_stop0', 'tmg_start', 'tmg_start0', 'low2up', 'tmg_init0', 'tmg_init', 'convip', 'splitst' \
                   'dgemm', 'constnt', \
                   'hines_sigma', 'serxst', 'mzonxst', 'gesdict', \
                  ]
# mxma8, inverse: Matrix functions, called 1M times, it's also a bad sign
# vsexp, vssqrt, vlog, vexp: Vector functions
# hines_intgrl: not sure what these is
# servst: Time series in the physics
# newdate: not important
FILE_EXCLUDE_LIST=['set_world_view']
# Set world view has non-mpi parts to it

# Some REs
comment = re.compile('^[\*c!]+.*', re.IGNORECASE)
md = re.compile('.*mattdebug.*', re.IGNORECASE)
p = re.compile('.*\Wcall\s+\W*([^\(]+).*', re.IGNORECASE)
#pn = re.compile('^([\s0-9]+)call ([a-z0-9_]+)\W', re.IGNORECASE)
pn = re.compile('^([\s0-9]{1,5}\s*)call ([a-z0-9_]+)\W', re.IGNORECASE)
pi = re.compile('^(\s+)(if\s*\(.*\))\s*call ([^\(]+)[ ]*(.*)$', re.IGNORECASE)

co = re.compile('^(\s+)do ([a-z]+)=1,\s*COMMON_SIZE.([^)]+).\s*$', re.IGNORECASE)
sd = re.compile("^\s*([a-z0-9_]+)\(.\)\s*=\s*([a-z0-9_]+)\(.*$", re.IGNORECASE)

# Check to see if a processed file
# also exists in the list
def filterFiles(files):
	newfiles=[]
	for f in files:
		pname="%s.%s"%(f,processed_suf)
		if not isfile(pname):
			newfiles.append(f)
	return newfiles


# Iterator for getting next values
# http://stackoverflow.com/questions/4197805/python-for-loop-look-ahead
def get_next(some_iterable, window=1):
	items, nexts = tee(some_iterable, 2)
	nexts = islice(nexts, window, None)
	return izip_longest(items, nexts)

# Simple function to ensure the spaces
# are at least 6 characters long..
# bah...
def padSpaces(spaces):
	return spaces.ljust(6)

def processFile(fname):
	global nowrite_files,VERBOSE

	# Do a last check to make sure the file
	# isn't already processed. (this can happen
	# when the user specifies a file that's
	# already been processed.
	pname="%s.%s"%(fname,processed_suf)
	if isfile(pname):
		raise Exception("Processed file already exists")

	mode=bool(os.stat(fname).st_mode & stat.S_IWUSR)
	if not mode:
		nowrite_files.append(fname)
		
	f = open(fname, 'r')
	prevline=''
	newline=''
	newlines=[]

	SKIP_NEXT_LINE=0
	for line, nextline in get_next(f):
		# Probably a better way to do this with the
		# iterator, but I'm not familiar with it
		if SKIP_NEXT_LINE>0:
			SKIP_NEXT_LINE+=-1
			continue

		if comment.match(line):
			newline=line
		elif md.match(prevline):
			# The line is already modified, so skip it
			newline=line
		else:

			m=p.match(line);
			if m:
				func = m.group(1).lower().strip()
				if VERBOSE > 2:
					print("p matched: %s"%line)
				mn=pn.match(line)
				mi=pi.match(line)

				# Function exclude list
				if VERBOSE > 1:
					print("\nChecking function '%s'"%func)
				if any(func in s for s in FUNC_EXCLUDE_LIST):
					if VERBOSE > 0:
						print("-- Excluding functiion %s"%func)

					# Default
					newline=line
				elif mn:
					if VERBOSE > 1:
						print("Matched (1), func=%s, line=%s"%(mn.group(2), line.strip()))
					# Make sure there are no labels in the spaces
					spaces = re.sub("[0-9]+", " ", mn.group(1)) + "  "
					spaces=padSpaces(spaces)
					newline = "\n%sCALL mattdebug(\"Calling %s - %s\", __LINE__)\n%s"%(spaces, mn.group(2), fname, line)
				elif mi:
					if VERBOSE > 1:
						print("Matched (2) %s: %s"%(line.strip(), mi.group(3)))
						print("Nextline: %s"%nextline)

					# Line continuation crap?
					args = mi.group(4)
					if re.match("^\s{5}[^\s]", nextline):
						args = args + "\n" + nextline
						SKIP_NEXT_LINE+=1

						if VERBOSE>1:
							print("Found a line continuation, args=%s"%args)

					func=mi.group(3)
					func=re.sub("\s+", "", func)
					spaces=padSpaces(mi.group(1))
					newline = "%s%s THEN\n%s   CALL mattdebug(\"Calling %s -- %s\", __LINE__)\n%s   CALL %s%s\n%sENDIF\n"%(\
						spaces, mi.group(2), \
						spaces, func, fname, \
						spaces, func, args, \
						spaces)
				else:
					if VERBOSE > 1:
						print("Do not know how to match %s"%line)
					# default
					newline=line
			else:
				newline = line

		newlines.append(newline)

		# Save the previous line
		prevline=line

	f.close()
	return newlines

def filterCommonSizeLoops(lines, fname, VERBOSE):
	# Though this filter could be run in the loop above,
	# it makes the code more complicated.  Plus, this way
	# it's simpler to en/disable this filter alone

	# Disabling this function
	return lines

	line0='' # line0 is basically last line

	in_common_init = False
	src_var = ""
	dest_var = ""
	main_var = ""
	space=""

	newlines=[]

	for line1, line2 in get_next(lines):

		if not line2:
			line2=""

		#print("Line0: ", line0.strip())
		#print("Line1: ", line1.strip())
		#print("Line2: ", line2.strip(), "\n")

		if in_common_init:

			m=sd.match(line2)

			if re.match("\s*end\s*do.*", line1):

				if len(src_var) == 0:
					raise Exception("While replacing COMMON_SIZE loop, could not find source var")
				if len(dest_var) == 0:
					raise Exception("While replacing COMMON_SIZE loop, could not find destination var")

				in_common_init = False

				if VERBOSE>0:
					print("Replacing a COMMON_SIZE do loop for %s"%main_var)

				# Write out stuff
				newlines.append("\n%scall mattdebug(\"Calling COMMON_SIZE(%s) - %s\", __LINE__)\n"%(space, main_var, fname))
				newlines.append("%s CALL copy_with_offset_1(%s(0), %s(1), COMMON_SIZE(%s))\n"%(space, src_var, dest_var, main_var))

				src_var=""
				dest_var = ""
				main_var = ""
	
			elif m:
				src_var=m.group(2)
				dest_var=m.group(1)
			else:
				if VERBOSE>1:
					print("src_var=%s, dest_var=%s"%(src_var, dest_var))


		else:
			# Second filter, modify the COMMON_SIZE do loops
			m = co.match(line2)
			if m:

				in_common_init = True
				main_var = m.group(3)
				space=m.group(1)

				if VERBOSE>0:
					print("Matched a COMMON_INIT do loop, main_var=%s"%(main_var))

			else:

				newlines.append(line1)

		line0=line1

	return newlines

def outputFile(fname, content):
	global nowrite_files,VERBOSE

	new_file = "%s.%s"%(fname, processed_suf)
	copyfile(fname, new_file)

	changeBack=False
	if any(fname in s for s in nowrite_files):
		if VERBOSE > 0:
			print("%s is not writable, temporarily changing"%fname)
		os.chmod(fname, os.stat(fname).st_mode | stat.S_IWUSR)
		changeBack=True

	f = open(fname, 'w')
	f.write("%s"%"".join(map(str, content)))
	f.close()

	if changeBack:
		if VERBOSE > 0:
			print("Removing write flag on %s"%fname)
		os.chmod(fname, os.stat(fname).st_mode & ~stat.S_IWUSR)


def getFiles(processed=False):
	# Get file listing
	files=[]
	for f in listdir('.'):
		if processed:
			# Only match processed files
			#if re.match("\.%s"%processed_suf, f):
			if re.match("^(.*)\.%s$"%processed_suf, f):
				files.append(f)
		else:
			# Avoid cdk files
			m=re.match("^([a-z0-9_]+)\.(ftn|ftn90|cdk90)+$", f)
			if m and not any(m.group(1) in s for s in FILE_EXCLUDE_LIST):
				files.append(f)
			#print("Pre-filtered Files: %s"%"\n".join(map(str, files)))
			#exit(0)
			files=filterFiles(files)

	return files


def main():

		try:
			opts, args = getopt.getopt(sys.argv[1:], 'uavf:')
		except getopt.GetoptError as err:
			# print help information and exit:
			print(str(err)) # will print something like "option -a not recognized"
			#usage()
			print("Usage: -u for undo, -a for all (default), -v for verbose, -f <file> to specify a file");
			sys.exit(2)

		#print("Opts = ", opts)
		#print("args = ", args)

		files=[]
		OPT_UNDO=False
		OPT_ALL=False
		for o,a in opts:
			if o == "-u":
				OPT_UNDO=True
			elif o == "-f":
				files.append(a)
			elif o == "-a":
				OPT_ALL=True
				files=getFiles(OPT_UNDO)
			else:
				assert False, "unhandled option"

		if OPT_UNDO:
			tfiles=files
			files=[]
			for f in tfiles:
				if re.match("^(.*)\.%s$"%processed_suf, f):
					files.append(f)
				else:
					pf="%s.%s"%(f, processed_suf)
					if not isfile(pf):
						if not OPT_ALL:
							print("File %s was not processed by this script (%s doesn't exist.)"%(f, pf))
					#exit(1)
					else:
						files.append(pf)
			
			if VERBOSE>0:
				print("Files to undo: %s"%" ".join(map(str, files)))
			if not len(files):
				print("No files to undo")

			for pf in files:
				m = re.match("^(.*)\.%s$"%processed_suf, pf)
				if m:
					f = m.group(1)
					if VERBOSE>1:
						print("pf=%s, f=%s"%(pf, f))

					changeBack=False
					mode=bool(os.stat(f).st_mode & stat.S_IWUSR)
					if not mode:
						if VERBOSE > 0:
							print("- Setting %s to writable (temporararily)"%f)
						os.chmod(f, os.stat(f).st_mode | stat.S_IWUSR)
						changeBack=True

					if VERBOSE>0:
						print("Undoing debug to %s"%pf)
					copyfile(pf, f)
					unlink(pf)

					if changeBack:
						if VERBOSE > 0:
							print("- Setting %s to not-writable"%f)
						os.chmod(f, os.stat(f).st_mode & ~stat.S_IWUSR)
				else:
					if VERBOSE>1:
						print("Skipping %s"%pf)
		else:
			if len(files):
				print("Processing Files (%d): %s"%(len(files), "\n".join(map(str, files))))
				for f in files:
					try:
						content = processFile(f)
						#content = filterCommonSizeLoops(content, f, VERBOSE)
						outputFile(f, content)
					except Exception as err:
						print("Could not process file: %s"%err)
					
			else:
				print("No files to process")


if __name__ == "__main__":
	main()
