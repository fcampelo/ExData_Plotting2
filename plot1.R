#Exploratory Data Analysis
## Project 02
### plot1.png
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

# Load files
NEI <- tbl_df(readRDS("NEI_data/summarySCC_PM25.rds"))
SCC <- tbl_df(readRDS("NEI_data/Source_Classification_Code.rds"))

#===============================================================================
# 3)  Precondition data
#
## To make this plot I'll "group_by" year and then summarize the results. Let's 
## give it a try:
Total.byYear<- as.data.frame({NEI %>%
                  group_by(year) %>%
                  summarize(sum(Emissions))})


print(Total.byYear)

#===============================================================================
# 4)  Make plot
#
## Now we make a plot (using the *base plotting system*) of Total Emissions 
## by year. I'll use some of the tricks suggested by Nathan Yau in his excellent 
## tutorial "Moving Past Default Charts" [1] to give the plot a more pleasant
## look. ;)
## [1]: http://flowingdata.com/2014/10/23/moving-past-default-charts/

# Set plotting parameters:
par(xpd=FALSE,                    # Clip all plotting to the plotting region
    mgp=c(1.8,.2,0),              # Adjust margin lines
    tck=0.02,                     # Get inward-facing tick marks on the axes
    bty="n",                      # Remove box around the plot
    bg="#DDF0F0")                 # Get a nice background color

# Prepare plot ("empty")
plot(0,0,type="n",
     xlim=range(Total.byYear[,1]),
     ylim=range(pretty(Total.byYear[,2]/10^6)),
     las=1, 
     main=expression('Total PM'[2.5]*' by year in the U.S.'),
     xlab=expression(italic('Year')), 
     ylab=expression(italic('Total PM'[2.5]*' (in millions of Tons)')), 
     family="Helvetica")

# Get some cool gridlines
grid(NA, NULL, col="white", lty="solid", lwd=2)

# Get the data to the plot!
points(x = Total.byYear[,1],
       y = Total.byYear[,2]/10^6,
       type = "b",
       pch=16, cex=2,
       lwd=2,
       col=1)