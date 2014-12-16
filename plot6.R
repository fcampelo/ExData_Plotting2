#Exploratory Data Analysis
## Project 02
### plot5.png
#===============================================================================
# 1)  Introduction
#
# This analysis assumes that the *NEI_data* folder (obtained after unzipping 
# the *NEI_data.zip* file) is a subfolder of the working directory. In other 
# words, I assume that the folder structure of the dataset is maintained, 
# instead of simply pasting all files to the working directory.
#
#===============================================================================
# 2)  Load data
#
## I'll use the **dplyr** package to load the data as a _data frame tbl_ object 
## (it'll make it easier for me to manipulate the whole thing later on):
library(dplyr,warn.conflicts = F)

# Load files (only if they are not already loaded - these things are *heavy*!)
if (!exists("NEI")){NEI <- tbl_df(readRDS("NEI_data/summarySCC_PM25.rds"))}
if (!exists("SSC")){SCC <- tbl_df(readRDS("NEI_data/Source_Classification_Code.rds"))}

#===============================================================================
# 3)  Precondition data

# Get all codes for motor vehicle-related emissions. My inclusion criteria were:
# - Column SSC$Short.names contains the word "Vehicle"; AND
# - Column SSC$Short.names does not contain the words "Surface Coating"; AND
# - Column SSC$Short.names does not contain the words "Motor Vehicle Fires"; AND
# - Column SSC$Short.names does not contain the words "Chem Manuf"; AND
# - Column SSC$Short.names does not contain the words "Petrol Trans";

# Get indices to the relevant SCCs
indx<-grep("Vehicles",SCC$Short.Name)
indx<-indx[-unique(
                  c(grep("Surface Coating",SCC$Short.Name[indx]),
                    grep("Motor Vehicle Fires",SCC$Short.Name[indx]),
                    grep("Chem Manuf",SCC$Short.Name[indx]),
                    grep("Petrol Trans",SCC$Short.Name[indx])))]

# Get corresponding source classification codes
key<-SCC$SCC[indx]

# Now "filter" the NEI dataset by these codes and by the Baltimore fips, 
# "group_by" year, and finally "summarize" the whole thing.
Baltimore.VehicleEmissions<- as.data.frame(
{ NEI %>%
    filter(SCC %in% key) %>%
    filter(fips==24510) %>%
    group_by(year) %>%
    summarize(sum(Emissions))})

names(Baltimore.VehicleEmissions)[2]<-"Total.Emissions"
print(Baltimore.VehicleEmissions)


#===============================================================================
# 4)  Make plot
## Now we make a plot (I'll use the *base plotting system*) of Total Emissions 
## from motor vehicle-related sources in Baltimore. I'll use some of the tricks 
## suggested by Nathan Yau in his excellent tutorial "Moving Past Default 
## Charts" [1] to give the plot a more pleasant look. ;)
## [1]: http://flowingdata.com/2014/10/23/moving-past-default-charts/

# Open PNG device
png("plot5.png",width = 9.8, height = 6.4, units="in", res=150)

# Set plotting parameters:
par(xpd=FALSE,                    # Clip all plotting to the plotting region
    oma=c(1.5,0,0,2.5),           # Adjust outer margin - right
    mar=c(5,4,4,3)+0.1,           # Adjust inner margin - right
    mgp=c(1.8,.2,0),              # Adjust margin lines
    tck=0.02,                     # Get inward-facing tick marks on the axes
    bty="n",                      # Remove box around the plot
    bg="#DDF0F0")                 # Get a nice background color

# Prepare plot ("empty")
plot(0,0,type="n",
     xlim=range(Baltimore.VehicleEmissions[,1])+c(-1,0),
     ylim=range(pretty(Baltimore.VehicleEmissions[,2]/10^3)),
     las=1, 
     main="Emissions from Motor Vehicle-Related Sources in Baltimore",
     xlab=expression(italic('Year')), 
     ylab=expression(italic('PM'[2.5]*' (in thousands of Tons)')), 
     family="Helvetica")

# Get some cool gridlines
grid(NA, NULL, col="white", lty="solid", lwd=2)

# Get the data to the plot!
points(x = Baltimore.VehicleEmissions[,1],
       y = Baltimore.VehicleEmissions[,2]/10^3,
       type = "b",
       pch=16, cex=2,
       lwd=2,
       col=1)

# Add some more decoration: year name near each point
text(x = Baltimore.VehicleEmissions[,1]+0.1,
     y = Baltimore.VehicleEmissions[,2]/10^3,
     labels = as.character(Baltimore.VehicleEmissions[,1]),
     pos=3,
     cex=0.8,
     col="#666666")

# Authorship marker
mtext("Source: Felipe Campelo | E.P.A.",
      cex=0.75, 
      line=0, 
      side=SOUTH<-1, 
      adj=1, 
      outer=TRUE,
      font=4,
      family="Helvetica")

# Outer box
box("outer", lty="solid", col="black")

# Close the device
dev.off()