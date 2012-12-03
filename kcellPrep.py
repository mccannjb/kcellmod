##
## Kcell Prep (kcellPrep.py)
##
## Author: James McCann (mccannjb@gmail.com)
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
if len(sys.argv)>=5:
	pointfile = sys.argv[1]
	subFile = sys.argv[2]
	ptlat = float(sys.argv[3])
	ptlon = float(sys.arvg[4])
else:
	sys.exit("Proper arguments required: [ptsrc filepath/filename] [csv filepath/filename] [latitute] [longitude]")


ptx,pty=p(ptlon,ptlat)
camxpts=point_source(pointfile)

xpts=camxpts.variables['XSTK']
ypts=camxpts.variables['YSTK']

NOXday=camxpts.variables['NO']+camxpts.variables['NO2']

minNOX=100	#Minimum NOx emission to remove small sources (ppm/hr)
maxDist=750	#Maximum radius to identify point-source matches (m)
## FIXME 3: Add radius from selected location


#### Average hourly VOC and NOx emissions over the day for each point
NOX=[]
for pt in NOXday.transpose():
	NOX.append(pt.mean())

#### Create an array of x/y coordinates for high NOx
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
				if camx[i][0]==kcells[k-1][0]:
					continue
			found+=1
                        kcells.append([camx[i][0],camx[i][1],found])

print "Matching points found: {0}".format(found)
print "Saving list to text file..."
out=open('pykcell.out','w')
for point in kcells:
	string="{0:.4f} {1:.4f} {2}\n".format(float(point[0]),float(point[1]),int(point[2]))
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
