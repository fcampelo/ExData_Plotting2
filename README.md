Exploratory Data Analysis :: Project 02
==========
### By Felipe Campelo

This file documents the code and resulting plots generated for the Peer Assessment 2 activity of the Exploratory Data Analysis course on Coursera. 

### Plot 1
*Have total emissions from $PM_{2.5}$ decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total $PM_{2.5}$ emission from all sources for each of the years 1999, 2002, 2005, and 2008.*


```r
#                             plot1.R
#===============================================================================
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
```

```
##   year sum(Emissions)
## 1 1999        7332967
## 2 2002        5635780
## 3 2005        5454703
## 4 2008        3464206
```

```r
#===============================================================================
# 4)  Make plot
#
## Now we make a plot (using the *base plotting system*) of Total Emissions 
## by year. I'll use some of the tricks suggested by Nathan Yau in his excellent 
## tutorial "Moving Past Default Charts" [1] to give the plot a more pleasant
## look. ;)
## [1]: http://flowingdata.com/2014/10/23/moving-past-default-charts/

# Open PNG device (uncommented in the original plot1.R file)
# png("plot1.png",width = 9.8, height = 6.4, units="in", res=150)

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

# Add some more decoration: year name near each point
text(x = Total.byYear[,1]+0.1,
     y = Total.byYear[,2]/10^6,
     labels = as.character(Total.byYear[,1]),
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
```

<img src="figure/plot1-1.png" title="plot of chunk plot1" alt="plot of chunk plot1" style="display: block; margin: auto;" />

```r
# Close the device (uncommented in the original plot1.R file)
# dev.off()
```


### Plot 2
*Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
(fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot 
answering this question.*


```r
#                             plot2.R
#===============================================================================
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
## To make this plot I'll "filter" by the Baltimore City, Maryland code (24510), 
## then "group_by" year, and finally "summarize" the results. Let's 
## give it a try:
Baltimore.byYear<- as.data.frame(
                  {NEI %>%
                  filter(fips==24510) %>%
                  group_by(year) %>%
                  summarize(sum(Emissions))})


print(Baltimore.byYear)
```

```
##   year sum(Emissions)
## 1 1999       3274.180
## 2 2002       2453.916
## 3 2005       3091.354
## 4 2008       1862.282
```

```r
#===============================================================================
# 4)  Make plot
#
## Now we make a plot (using the *base plotting system*) of Total Emissions 
## by year in Baltimore. I'll use some of the tricks suggested by Nathan Yau in 
## his excellent tutorial "Moving Past Default Charts" [1] to give the plot a 
## more pleasant look. ;)
## [1]: http://flowingdata.com/2014/10/23/moving-past-default-charts/

# Open PNG device (uncommented in the original plot2.R file)
# png("plot2.png",width = 9.8, height = 6.4, units="in", res=150)

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
     xlim=range(Baltimore.byYear[,1])+c(-1,0),
     ylim=range(pretty(Baltimore.byYear[,2]/10^3)),
     las=1, 
     main=expression('Total PM'[2.5]*' by year in Baltimore City, Mariland'),
     xlab=expression(italic('Year')), 
     ylab=expression(italic('Total PM'[2.5]*' (in thousands of Tons)')), 
     family="Helvetica")

# Get some cool gridlines
grid(NA, NULL, col="white", lty="solid", lwd=2)

# Get the data to the plot!
points(x = Baltimore.byYear[,1],
       y = Baltimore.byYear[,2]/10^3,
       type = "b",
       pch=16, cex=2,
       lwd=2,
       col=1)

# Add some more decoration: year name near each point
text(x = Baltimore.byYear[,1]+0.1,
     y = Baltimore.byYear[,2]/10^3,
     labels = as.character(Baltimore.byYear[,1]),
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
```

<img src="figure/plot2-1.png" title="plot of chunk plot2" alt="plot of chunk plot2" style="display: block; margin: auto;" />

```r
# Close the device (uncommented in the original plot2.R file)
# dev.off()
```

### Plot 3
*Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.*


```r
#                             plot3.R
#===============================================================================
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
## To make this plot I'll "filter" by the Baltimore City, Maryland code (24510), 
## then "group_by" year and type, and finally "summarize" the results. Let's 
## give it a try:
Baltimore.byYearType<- as.data.frame(
{NEI %>%
       filter(fips==24510) %>%
       group_by(year,type) %>%
       summarize(sum(Emissions))})

names(Baltimore.byYearType)[3]<-"Total.Emissions"
print(Baltimore.byYearType)
```

```
##    year     type Total.Emissions
## 1  1999 NON-ROAD       522.94000
## 2  1999 NONPOINT      2107.62500
## 3  1999  ON-ROAD       346.82000
## 4  1999    POINT       296.79500
## 5  2002 NON-ROAD       240.84692
## 6  2002 NONPOINT      1509.50000
## 7  2002  ON-ROAD       134.30882
## 8  2002    POINT       569.26000
## 9  2005 NON-ROAD       248.93369
## 10 2005 NONPOINT      1509.50000
## 11 2005  ON-ROAD       130.43038
## 12 2005    POINT      1202.49000
## 13 2008 NON-ROAD        55.82356
## 14 2008 NONPOINT      1373.20731
## 15 2008  ON-ROAD        88.27546
## 16 2008    POINT       344.97518
```

```r
#===============================================================================
# 4)  Make plot
#
## Now we make a plot (using the *ggplot2 plotting system*) of Total Emissions 
## by type by year in Baltimore. 
library(ggplot2)
library(gridExtra)
```

```
## Loading required package: grid
```

```r
# Open PNG device (uncommented in the original plot3.R file)
# png("plot3.png",width = 9.8, height = 6.4, units="in", res=150, bg="transparent")

# Build the ggplot object
m<-ggplot(Baltimore.byYearType,
          aes(year,
              Total.Emissions,
              group=type,
              colour=type)) +
      geom_line(size=1.5) +
      geom_point(size=5) +
      ggtitle(expression('PM'[2.5]*' by type and year in Baltimore City, Mariland')) +
      xlab(expression(italic('Year'))) + 
      ylab(expression(italic('PM'[2.5]*' (in Tons)'))) +
      theme(axis.title.y = element_text(size = rel(1.5), angle = 90),
            axis.title.x = element_text(size = rel(1.5), angle = 0),
            axis.text = element_text(size = rel(1.25)),
            legend.text = element_text(size = rel(1.25)),
            legend.title = element_text(size = rel(1.25)),
            plot.title = element_text(size = rel(1.75)))

# Flush to the graphics device
print(m)
```

<img src="figure/plot3-1.png" title="plot of chunk plot3" alt="plot of chunk plot3" style="display: block; margin: auto;" />

```r
# Close the device (uncommented in the original plot3.R file)
# dev.off()
```
