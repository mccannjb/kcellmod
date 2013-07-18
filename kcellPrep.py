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
## Arguments:
##	kcellPrep.py [CAMx ptsrc file] [CSV pt file] [lat] [lon] [radius (deprecated)] [numpts] [group size]
##		where numpts is the n closest number of points to the chosen lat/lon
##		where group size is the number of closest points per file desired
## 
## Outputs:
##	 Data file containing kcell (int), X (float), and Y (float)
##		Proper regex form: -{0,1}[0-9]+\.[0-9]{4},-{0,1}[0-9]+\.[0-9]{4},[0-9]{2}
##
## TODO:
##	 Change for-range occurences to enumerate for loops
##  

from PseudoNetCDF.camxfiles.Memmaps import point_source
import numpy as np
import scipy.spatial
import heapq
import csv
import sys
import re
from pyproj import Proj

## Function to determine if a line in the CSV file is valid data (using regular expressions)
## 	Returns True if the line matches the data format
##	Returns False if the line does not match the data format
def isData(line):
	line=line.strip()
	p = re.compile('-{0,1}[0-9]+\.[0-9]+,-{0,1}[0-9]+\.[0-9]+')
#	p = re.compile('-{0,1}[0-9]+\.[0-9]+ -{0,1}[0-9]+\.[0-9]+')
	m = p.match(line)
	if m:
		return True
	else:
		return False

# Function to write the header for a Keyhole Markup Language file (Google Earth)
def writeKMLhead(file):
	string="<?xml version='1.0' encoding='UTF-8'?>\n"\
		"<kml xmlns='http://earth.google.com/kml/2.1'>\n"\
		"<Document>\n"\
		"\t<name>CFPP Locations</name>\n"
	file.write(string)

# Function to write the point for a Keyhole Markup Language file (Google Earth)
def writeKMLpt(file,name,lat,lon):
	string="<Placemark>\n"\
		"\t<name>{0}</name>\n"\
		"\t<Point>\n"\
		"\t\t<coordinates>{1},{2},0</coordinates>\n"\
		"\t</Point>\n"\
		"</Placemark>\n".format(name,float(lon),float(lat))
	file.write(string)		

# Function to write the foot for a Keyhole Markup Language file (Google Earth)
def writeKMLfoot(file):
	string="</Document>\n"\
		"</kml>"
	file.write(string)

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
	ptlon = float(sys.argv[4])
	npts = int(sys.argv[5])
	grpsize = int(sys.argv[6])
else:
	sys.exit("Proper arguments required: [ptsrc file] [csv file] [lat] [lon] [num pts] [group size]")

## Open diagnostic file and write inputs
diag=open('kcellPrep.diag','w')
diag.write("Point file:\t{}\n".format(pointfile)+
	"Substation locations:\t{}\n".format(subFile)+
	"Interest Lat:\t{}\n".format(ptlat)+
	"Interest Lon:\t{}\n".format(ptlon)+
	"Number of pts:\t{}\n".format(npts)+
	"Number of groups:\t{}\n".format(grpsize))


## Set conditions/constraints
minNOX=1000	#Minimum NOx emission to remove small sources (mole/hr)
maxDist=750	#Maximum radius to identify point-source matches (m)
diag.write("Minimum NOx emission: {} mole/hr\n".format(minNOX))
diag.write("Maximum point-source radius: {} meters\n\n".format(maxDist))

## Calculate X,Y coordinates for user-inputted lat/lon values for
## point of interest
ptx,pty=p(ptlon,ptlat)
diag.write("Lat: {}, Lon: {} ---> X: {}, Y: {}\n".format(ptlat,ptlon,ptx,pty))

## Load CAMx Point Source Emissions file with PseudoNetCDF
camxpts=point_source(pointfile)

## Create array of X's and Y's for all points (includes duplicates)
xpts=camxpts.variables['XSTK'][:,0]
ypts=camxpts.variables['YSTK'][:,0]

#### Average hourly NOx emissions over the day for each point
NOX=np.mean(camxpts.variables['NO']+camxpts.variables['NO2'],axis=0)

#### Create an array of x/y coordinates for high NOx	
#### 	This is to exclude sources which are most-likely
####	NOT EGU emissions.
camx=np.array([(xpts[i],ypts[i]) for i in np.where(NOX>minNOX)[0]])

camxu=np.vstack([np.array(u) for u in set([tuple(q) for q in camx])])   #Remove duplicates from camx points list

diag.write("Created CAMx point array, closing CAMx file\n")

######################################
##### Load SUBSTATION locations ######
######################################

with open(subFile,'rU') as f:
	suba=np.loadtxt(f)

diag.write("Created substation array, closing file\n")
sub=np.vstack([np.array(u) for u in set([tuple(q) for q in suba])])   #Remove duplicates from sub points list
diag.write("Total substations: {}\n".format(sub.shape[0]))
######################################
####### Compare SUB and CAMX #########
######################################

dist= scipy.spatial.distance.cdist(camx,sub)	# This creates a matrix of distances with dimensions (camx-points)x(sub-points).
						# Distance between the third CAMX point and the 32nd SUB point, the location
						# would be dist[2,31].

######################################

kcells=[]	# Create an array to represent KCELL values for each point
found=0
diag.write("----------------Matches----------------\n")
diag.write("CAMx X, Y \t-----> \tSubstation Lat,Lon\n")
# Iterate through distance matrix to find close proximity matches
# Then assign new iterative kcell value if not already present
#			subfX=sub[np.where(dist[i]==j)[0]][0]
#			subfY=sub[np.where(dist[i]==j)[0]][1]
#			subLon,subLat=p(subfX,subfY,inverse=True)
#			diag.write("{}, {}".format(subLat,subLon))

for i,row in enumerate(dist):
	for j,col in enumerate(row):
		if col<=maxDist:
			k=found
			if k>0:
				if camx[i][0]==kcells[k-1][0]:
					continue
			found+=1
			subfX,subfY=sub[j,0:2]
			camxX,camxY=camx[i,0:2]
			subLon,subLat=p(subfX,subfY,inverse=True)
			diag.write("{0:.4f}\t,\t{1:.4f}\t-->\t{2}\t,\t{3}\n".format(float(camxX),float(camxY),float(subLat),float(subLon)))
			kcells.append([camxX,camxY,found])
#for i in range(0,len(dist)):
#	for j in dist[i]:
#		if j<=maxDist:
#                       k=found
#			if k>0:
#				if camx[i][0]==kcells[k-1][0]:	#Check if point is identical to previous, if so skip
#					continue
#			found+=1
#                       kcells.append([camx[i][0],camx[i][1],found])

knparray=np.array(kcells)	#Convert kcells array into numpy array (to perform slicing later)

## Identify distance of points in matching points array (kcells) to user-input central location
locDist = scipy.spatial.distance.cdist(knparray[:,0:2],[[ptx,pty]]).flatten().tolist()
## Identify the closest n (user-input npts) number of locations to central location
closestn= heapq.nsmallest(npts,enumerate(locDist),key=lambda x: x[1])

diag.write("Total matching points found: {0}\n".format(found))
diag.write("Closest point from center: {0:.4f} km\n".format(closestn[0][1]/1000.))
diag.write("Farthest point from center: {0:.4f} km\n".format(closestn[npts-1][1]/1000.))

out=open('PITTSB_1.out','w')
kml=open('PITTSB.kml','w')
writeKMLhead(kml)	#Write KML head
## Print out the X,Y location and K values for each matching point within the list of closest points
## 	This will split the output into multiple files based on the user-input grpsize
filenum=1
counter=0
totalcount=0
for point in closestn:
	if counter == grpsize:	#Restart counter and open new file when reached max points for group
		out.close()
		filenum +=1
		counter = 0
		out=open("PITTSB_{0}.out".format(filenum),'w')
	i=point[0]
	x,y,k=float(kcells[i][0]),float(kcells[i][1]),int(counter+2)	# Counter+2 to account for DDM error with tracking source "1"
	string="{0:.4f} {1:.4f} {2}\n".format(x,y,k)
	lon,lat=p(x,y,inverse=True)	#Recalculate lat/lon for point
	writeKMLpt(kml,totalcount+1,lat,lon)	#Write KML point
	out.write(string)
	counter += 1
	totalcount += 1
out.close()
writeKMLfoot(kml)	#Write KML footer
kml.close()
diag.write( "Wrote {0} files, for a total of {1} points\n".format(filenum,totalcount))
diag.close()
