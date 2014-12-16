#Exploratory Data Analysis
## Project 02
### plot3.png
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

#===============================================================================
# 4)  Make plot
#
## Now we make a plot (using the *ggplot2 plotting system*) of Total Emissions 
## by type by year in Baltimore. 
library(ggplot2)

# Open PNG device
png("plot3.png",width = 9.8, height = 6.4, units="in", res=150, bg="transparent")

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
      annotate("text", x = 2006.5, y = -100, label = "Source: Felipe Campelo | E.P.A.",) +
      theme(axis.title.y = element_text(size = rel(1.5), angle = 90),
            axis.title.x = element_text(size = rel(1.5), angle = 0),
            axis.text = element_text(size = rel(1.25)),
            legend.text = element_text(size = rel(1.25)),
            legend.title = element_text(size = rel(1.25)),
            plot.title = element_text(size = rel(1.75)))

# Flush to the graphics device
print(m)

# Close the device
dev.off()