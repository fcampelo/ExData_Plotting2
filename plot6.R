#Exploratory Data Analysis
## Project 02
### plot6.png
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






##### PAREI AQUI - problema na filtragem por fips...
# Now "filter" the NEI dataset by these codes and by the Baltimore and Los 
# Angeles fips, then "group_by" year and fips, and finally "summarize" the whole thing.
BaltimorexLA<- as.data.frame(
{ NEI %>%
    filter(SCC %in% key) %>%
    mutate(fips=as.numeric(fips)) %>%
    filter(fips %in% c(24510,06037)) %>%
    mutate(fips=as.factor(fips)) %>%
    group_by(fips,year) %>%
    summarize(sum(Emissions))})

names(BaltimorexLA)[1:3]<-c("Place","Year","Total.Emissions")
levels(BaltimorexLA$fips)<-c("Los Angeles","Baltimore")
print(BaltimorexLA)

# Divide data frame into Baltimore and LA (absolute emissions), and
# include column on relative change from the baseline (1999)
LosAngeles<-BaltimorexLA[1:4,]
Baltimore<-BaltimorexLA[5:8,]

LosAngeles[,4]<-LosAngeles[,3]/LosAngeles[1,3]
Baltimore[,4]<-Baltimore[,3]/Baltimore[1,3]

#===============================================================================
# 4)  Make plot
## I'll use some of the tricks 
## suggested by Nathan Yau in his excellent tutorial "Moving Past Default 
## Charts" [1] to give the plot a more pleasant look. ;)
## [1]: http://flowingdata.com/2014/10/23/moving-past-default-charts/

# Open PNG device
png("plot6.png",width = 7.8, height = 5.2, units="in", res=150)

# I'll actually show 2 panels on this figure: the absolute 
# emissions by year in both Baltimore and LA, each containing info on the 
# (percentual) reductions in emissions.

# Set plotting parameters:
par(mfrow=c(1,2),                 # Two panels, horizontal
    xpd=FALSE,                    # Clip all plotting to the plotting region
    oma=c(1.5,0,0,2.5),           # Adjust outer margin - right
    mar=c(5,4,4,3)+0.1,           # Adjust inner margin - right
    mgp=c(1.8,.2,0),              # Adjust margin lines
    tck=0.02,                     # Get inward-facing tick marks on the axes
    bty="n",                      # Remove box around the plot
    bg="#DDF0F0")                 # Get a nice background color

# First panel: Baltimore
# Prepare plot ("empty")
plot(0,0,type="n",
     xlim=range(Baltimore$Year)+c(-1,0),
     ylim=range(pretty(Baltimore$Total.Emissions)),
     las=1, 
     main="Change in emissions from motor\nvehicle sources: Baltimore",
     xlab=expression(italic('Year')), 
     ylab=expression(italic('PM'[2.5]*' (in Tons)')), 
     family="Helvetica")

# Get some cool gridlines
grid(NA, NULL, col="white", lty="solid", lwd=2)

# Line connecting the values from 1999 and 2008
points(c(1999,2008),Baltimore$Total.Emissions[c(1,4)],type="l",col="black",lty=2,lwd=2)

# Plot points
points(x = Baltimore[,2],
       y = Baltimore[,3],
       type = "p",
       pch=16, 
       cex=2,
       col="black")

# Plot vertical lines
points(x = Baltimore[,2],
       y = Baltimore[,3],
       type = "h",
       lwd=2,
       lty=1,
       col="black")

# Relative reduction
text(x = 2008,
     y = 78,
     labels = paste0("1999-2008 change: ",round(100*(Baltimore$V4[4]-1),2),"%"),
     pos=2,
     cex=0.8,
     col="black",
     font=4,
     family="Helvetica")



# Second panel: Los Angeles
# Prepare plot ("empty")
plot(0,0,type="n",
     xlim=range(LosAngeles$Year)+c(-1,0),
     ylim=range(pretty(LosAngeles$Total.Emissions)),
     las=1, 
     main="Change in emissions from motor\nvehicle sources: Los Angeles",
     xlab=expression(italic('Year')), 
     ylab=expression(italic('PM'[2.5]*' (in Tons)')), 
     family="Helvetica")

# Get some cool gridlines
grid(NA, NULL, col="white", lty="solid", lwd=2)

# Line connecting the values from 1999 and 2008
points(c(1999,2008),LosAngeles$Total.Emissions[c(1,4)],type="l",col="black",lty=2,lwd=2)

# Plot points
points(x = LosAngeles[,2],
       y = LosAngeles[,3],
       type = "p",
       pch=16, 
       cex=2,
       col="black")

# Plot vertical lines
points(x = LosAngeles[,2],
       y = LosAngeles[,3],
       type = "h",
       lwd=2,
       lty=1,
       col="black")

# Relative reduction
text(x = 2008,
     y = 1480,
     labels = paste0("1999-2008 change: ",round(100*(LosAngeles$V4[4]-1),2),"%"),
     pos=2,
     cex=0.8,
     col="black",
     font=4,
     family="Helvetica")

# Last decorations
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