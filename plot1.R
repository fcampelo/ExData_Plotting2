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

# Load files (only if they are not already loaded - these things are *heavy*!)
if (!exists("NEI")){NEI <- tbl_df(readRDS("NEI_data/summarySCC_PM25.rds"))}
if (!exists("SSC")){SCC <- tbl_df(readRDS("NEI_data/Source_Classification_Code.rds"))}

#===============================================================================
# 3)  Precondition data
#
## To make this plot I'll "group_by" year and then summarize the results. Let's 
## give it a try:
Total.byYear<- as.data.frame(
                  {NEI %>%
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

# Open PNG device
png("plot1.png",width = 6.4, height = 4.2, units="in", res=150)

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
     xlim=range(Total.byYear[,1])+c(-1,0),
     ylim=range(pretty(Total.byYear[,2]/10^6)),
     las=1, 
     main=expression('Total PM'[2.5]*' by year in the U.S.'),
     xlab=expression(italic('Year')), 
     ylab=expression(italic('PM'[2.5]*' (in millions of Tons)')), 
     family="Helvetica")

# Get some cool gridlines
grid(NA, NULL, col="white", lty="solid", lwd=2)

# fit linear model and plot regression line
reg<-lm(I(Total.byYear[,2]/10^6)~Total.byYear[,1])
regy<-coefficients(reg)[1]+c(1999,2008)*coefficients(reg)[2]
points(c(1998.5,2008.5),regy,type="l",col="#FFAAAA",lty=2,lwd=2)

# Plot points
points(x = Total.byYear[,1],
       y = Total.byYear[,2]/10^6,
       type = "p",
       pch=16, 
       cex=2,
       col=1)

# Plot vertical lines
points(x = Total.byYear[,1],
       y = Total.byYear[,2]/10^6,
       type = "h",
       lwd=2,
       lty=1,
       col=1)

# Add some more decoration: year name near each point
text(x = Total.byYear[,1],
     y = Total.byYear[,2]/10^6,
     labels = as.character(Total.byYear[,1]),
     pos=3,
     cex=0.8,
     col="#666666")

# Linear regression slope
text(x = 2008,
     y = 5,
     labels = paste0(as.numeric(round(coefficients(reg)[2],3))," MTons/yr"),
     pos=2,
     cex=0.8,
     col="#FF3333",
     font=1,
     family="Helvetica")

# Authorship marker
mtext("Source: Felipe Campelo | E.P.A.",
      cex=0.75, 
      line=0, 
      side=SOUTH<-1, 
      adj=1, 
      outer=TRUE,
      font=3,
      family="Helvetica")

# Outer box
box("outer", lty="solid", col="black")

# Close the device
dev.off()