##
## Kcell Prep (kcellPrep.py)
##
## Author: James McCann (mccannjb (at) gmail.com)
##
## Description: 
## 	This python script uses a csv file containing the location of electrical generation
##	substations to identify matching emissions sources in a CAMx point source emissions
##	file. 
##
## Inputs:
##	CAMx point source emissions input file (UAM-IV format fortran binary file)
##	CSV file containing desired X,Y locations for point source matching
##
## Outputs:
##	 Data file containing kcell (int), X (float), and Y (float)
##		Proper regex form: -{0,1}[0-9]+\.[0-9]{4},-{0,1}[0-9]+\.[0-9]{4},[0-9]{2}
##
## TODO:
##	Implement top_n distance function
##		- incorporate based on user input top_n value
##	Handle job-script inputs rather than command-line arguments
##
##
## FIXME 3: Allow for proper radius selection for sources around submitted lat/lon
## FIXME 4: Allow for creation of n number of outputs based on group size (user submitted value)
## FIXME 6: Change for-range occurences to enumerate for loops
##

from PseudoNetCDF.camxfiles.Memmaps import point_source
import numpy as np
import scipy.spatial
import csv
import sys
import re
from pyproj import Proj


### Projection Parameters and function ###
proj_args1='+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +a=6370997.0'
p = Proj(proj_args1)

########################################
### Load CAMx point source locations ###
########################################

### Check inputs, assign user-submitted point-file name/location
if len(sys.argv)>=7:
	pointfile = sys.argv[1]
	subFile = sys.argv[2]
	ptlat = float(sys.argv[3])
	ptlon = float(sys.arvg[4])
	radius = float(sys.argv[5])
	npts = float(sys.argv[6])
else:
	sys.exit("Proper arguments required: [ptsrc file] [csv file] [lat] [lon] [radius (km)] [num pts]")

## Set conditions/constraints
minNOX=100	#Minimum NOx emission to remove small sources (ppm/hr)
maxDist=750	#Maximum radius to identify point-source matches (m)

## Calculate X,Y coordinates for user-inputted lat/lon values for
## point of interest
ptx,pty=p(ptlon,ptlat)

## Load CAMx Point Source Emissions file with PseudoNetCDF
camxpts=point_source(pointfile)

## Create array of X's and Y's for all points (includes duplicates)
xpts=camxpts.variables['XSTK']
ypts=camxpts.variables['YSTK']

## Calculate the total NOx for each hour in the CAMx file
## (misleading name)
NOXday=camxpts.variables['NO']+camxpts.variables['NO2']

#### Average hourly NOx emissions over the day for each point
NOX=[]
for pt in NOXday.transpose():
	NOX.append(pt.mean())

#### Create an array of x/y coordinates for high NOx
#### 	This is to exclude sources which are most-likely
####	NOT EGU emissions.
pointsXY=[]	
for i in range(0,len(xpts)):
	if NOX[i]>minNOX:
		x,y=xpts[i][0],ypts[i][0]
		pointsXY.append([x,y])

print "Created CAMx point array, closing CAMx file"
camx=np.array(pointsXY)
camxpts.close
del camxpts

######################################
##### Load SUBSTATION locations ######
######################################

f=open(subFile,'rU')
subcsv=csv.reader(f,delimiter=' ')

subXY=[]
for row in subcsv:
	# Recombine row array to string
	row_str=""
	for record in row:
		row_str += str(record)+" "
	# Check each row, if the row contains properly formatted data, add the X,Y coordiantes to the array
	if isData(row_str):
		x,y=row[0],row[1]
		subXY.append([x,y])
	else:
		continue

print "Created substation array, closing file"
f.close()
suba=np.array(subXY)
sub=np.vstack([np.array(u) for u in set([tuple(q) for q in suba])])   #Remove duplicates from sub points list

######################################
####### Compare SUB and CAMX #########
######################################

dist= scipy.spatial.distance.cdist(camx,sub)	# This creates a matrix of distances with dimensions (camx-points)x(sub-points).
						# Distance between the third CAMX point and the 32nd SUB point, the location
						# would be dist[2,31].

######################################

kcells=[]	# Create an array to represent KCELL values for each point
found=0
# Iterate through distance matrix to find close proximity matches
# Then assign new iterative kcell value if not already present
for i in range(0,len(dist)):
	for j in dist[i]:
		if j<=maxDist:
                        k=found
			if k>0:
				if camx[i][0]==kcells[k-1][0]:	#Check if point is identical to previous, if so skip
					continue
			found+=1
                        kcells.append([camx[i][0],camx[i][1],found])

knparray=np.array(kcells)	#Convert kcells array into numpy array (to perform slicing later)

# IMPLEMENT: Method to return top_n numbers of the matches array
# based on distance, minimum ranks. Base on stackoverflow example:
#	>>> seq = [100, 2, 400, 500, 400]
#	>>> heapq.nlargest(2, enumerate(seq), key=lambda x: x[1])
#	[(3, 500), (2, 400)]
#

## Identify distance of points in matching points array (kcells) to user-input central location
locDist = scipy.spatial.distance.cdist(knparray[:,0:2],[[ptx,pty]]).flatten().tolist()
## Identify the closest n (user-input npts) number of locations to central location
closestn= heapq.nsmallest(npts,enumerate(locDist),key=lambda x: x[1])

print "Matching points found: {0}".format(found)
print "Farthest point from center: {0} km".format(closestn[npts-1][1]/1000.)
print "Saving list to text file..."
out=open('pykcell.out','w')
## Print out the X,Y location and K values for each matching point within the list of closest points
for point in closestn:
	i=point[0]
	x,y,k=float(kcells[i][0]),float(kcells[i][1]),int(kcells[i][2])
	string="{0:.4f} {1:.4f} {2}\n".format(x,y,k)
	out.write(string)
out.close()
print "Done"


## Function to determine if a line in the CSV file is valid data (using regular expressions)
## 	Returns True if the line matches the data format
##	Returns False if the line does not match the data format
def isData(line):
	line=line.strip()
	p = re.compile('-{0,1}[0-9]+\.[0-9]+ -{0,1}[0-9]+\.[0-9]+')
	m = p.match(line)
	if m:
		return True
	else:
		return False
