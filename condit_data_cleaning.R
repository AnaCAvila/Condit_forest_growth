## Cleaning data from Condit's Small Plots ##
# from raw data
#Jan 2021

#Intakes
# Raw Condit data (tree census)
# ForestGEO Panama Small Plots description
# WorldClim predictors

# clean up
rm(list = ls())
graphics.off()
setwd("/Users/anavitorino/Desktop/Research/Thesis/condit")

# Libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(NLP)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###### Functions
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#loads an RData file, and returns it
loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

#remove years in which a large tree disappeared, and then reappeared
remove_outlier_years <- function(df){
  for (iter.plot in unique(df$plot)){
    print(iter.plot)
    tmp <- subset(df, plot == iter.plot)
    
    if (length(unique(tmp$year)) >= 3){
      tst <- as.data.frame(tmp %>% group_by(year) %>% slice_max(order_by = dbh, n = 5)) #5 largest DBH values per year
      tst$val <- 1
      tst <- cbind(tst['stemID'], tst['year'], tst['val'])
      df.wide <- reshape(tst, idvar = "stemID", timevar = "year", direction = "wide")
      names(df.wide) <- gsub("val.", "", names(df.wide))
      print(df.wide)
      
      columns <- c()
      for (j in 3:(ncol(df.wide)-1)){
        for (i in 1:nrow(df.wide)){
          if (is.na(df.wide[i,j]) & !is.na(df.wide[i,(j+1)]) & !is.na(df.wide[i,(j-1)])){
            columns  <- c(columns, j)
          }
        }
      }
      if(length(columns) != 0){
        gaps <- unique(columns)
        for (gap in gaps){
          print(as.numeric(colnames(df.wide[gap])))
          df <- df[!(df$year == as.numeric(colnames(df.wide[gap])) & df$plot == iter.plot),]
        }
      }
    }
  }
  return(df)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###### Switch
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# if you'd like to import the processed condit data file directly or remake it from the DRYAD, FGEO and STRI raw data.
import = T


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###### Body
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


if (import == F){
  lf<- list.files("./data/condit_raw_rdata/") # list all file names in rdata format
  lf<- lf[grep("rdata", lf)] #grep search for matches to argument pattern (here "rdata") in vector lf
  fgeo <- read.csv('./data/fgeo_plots.csv') #FGEO coordinates for each plot
  pred <- read.csv('./data/predictorsdf.csv') #STRI WorldClim data
  
  #converting to csv
  dir.create('condit_raw_csv')
  for(i in (1:length(lf))){
    file <- loadRData(paste("./data/condit_raw_rdata/", lf[i], sep = ""))
    path_out <- './data/condit_raw_csv/'
    write.csv(file, paste(path_out, lf[i], ".csv", sep=""), row.names=FALSE)
  }
  
  #now operating with the CSV files.
  path_raw_census <- "./data/condit_raw_csv/"
  unlink(paste(path_raw_census, "marenaRecent.stem1.rdata.csv", sep= "")) #removes plots that were only measured once (this file is not described on Dryad)
  unlink(paste(path_raw_census, "marena1cns.stem1.rdata.csv", sep= "")) #removes plots that were only measured once
  
  lf<- list.files(path_raw_census) # list all file names in DATA folder
  #unite them all under one "condit" dataframe
  condit<- fread(paste(path_raw_census, lf[1], sep= ""), data.table= FALSE) #similar to read.table but faster and more convenient
  cn <- names(condit)
  condit$file <- lf[1]
  str(condit)
  for(i in lf[-1]){
    print(i)
    ## Read file
    tmp <- fread(paste(path_raw_census, i, sep= ""), data.table= FALSE, col.names= cn) # read data file 'i'
    ## combine with previous data
    try(condit <- rbind(condit, cbind(tmp, file= i))) # wrap in 'try' because some files may be empty and generate an error
  }
  
  #remove unnecessary columns:
    #1 - treeID - as per recommendation, we're only identifying stems.
    #3, 4 - tag and StemTag - tag used on tree/stem
    #6 - quadrat - row and column in 20x20 grid
    #12 - DFstatus - redundant, dead or alive.
    #13 - codes - not recommended for analysis by Condit Dryad website
    #19 - plotID - renundant with "plot" column
    #20 - file name
  condit <- condit[,-c(1,3,4,6,12,13,19,20)]
  #remove entries with NA dates and dbh
  condit <- condit[complete.cases(condit), ]
  #since we don't care about variation at a sub-annual resolution, and will get yearly means
  #reduce ExactDate into just the year.
  condit$ExactDate <- round_date(condit$ExactDate,unit = "year",
                                 week_start = getOption("lubridate.week.start", 7))
  condit$ExactDate <- year(condit$ExactDate)
  condit <- rename(condit, year = ExactDate)

  #now we count how many survey points per date (to see if there's homogeneity on data collection times)
  tab <- as.data.frame(table(condit['year'])) #counting how many times each date occurs in the dataset (how many datapoints were surveyed then)
  condit$date_count <- tab$Freq[match(condit$year, tab$Var1)]
  
  condit <- unique(condit) #guarantee no repeats in the data
  
  # add coordinates to plot name
  
  fgeo$Plot <- tolower(fgeo$Plot)

  #fixing naming mismatches
  condit$plot[condit$plot == "plot32"] <- "p32"
  condit$plot[condit$plot == "plot31"] <- "p31"
  condit$plot[condit$plot == "cerropeladocihhutp"] <- "cerro_pelado_cihh_utp"
  fgeo$Plot[fgeo$Plot == "plot 31"] <- "p31"
  fgeo$Plot[fgeo$Plot == "plot 32"] <- "p32"
  #no longitude/latitude coordinates found for this plot
  condit <- subset(condit, condit$plot != "cerro_pelado_cihh_utp")
  
  #note that some plots present in fgeo are not present in condit (not all Panama small plots have DBH readings)
  condit$latitude <- as.numeric(fgeo$Latitude[match(condit$plot, fgeo$Plot)])
  condit$longitude <- as.numeric(fgeo$Longitude[match(condit$plot, fgeo$Plot)])
  condit$elevation <- as.numeric(fgeo$Elevation_m[match(condit$plot, fgeo$Plot)])

  #adding temperature and rainfall from WorldClim data

  pred <- subset(pred, !is.na(pred$bio1) & !is.na(pred$bio12))
  
  count <- 0
  #warning: this next piece of code is very slow.
  for(i in 1:nrow(condit)){
    condit$temperature[i] <- pred$bio1[which(abs(pred$vx - condit$longitude[i]) +
                                          abs(pred$vy - condit$latitude[i]) == 
                                               min(abs(pred$vx - condit$longitude[i]) + 
                                                     abs(pred$vy - condit$latitude[i])))]
  }
  
  for(i in 1:nrow(condit)){
    condit$annual_ppt[i] <- pred$bio12[which(abs(pred$vx - condit$longitude[i]) +
                                               abs(pred$vy - condit$latitude[i]) == 
                                               min(abs(pred$vx - condit$longitude[i]) + 
                                                     abs(pred$vy - condit$latitude[i])))]
  }
  
  
  #saveRDS(condit, "condit_temp_ppt.rds")
}else{
  condit <- readRDS("condit_temp_ppt.rds")
}

#at this point, we have a file with
  # mean annual temperature
  # mean annual precipitation
  # AGB per coordinate/plot



ages <- read.csv('./data/ages.csv')


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###### remove all plots not tagged as secondary
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Sherman is all mature secondary forest
#Cocoli is all early secondary

ages$Plot.Name <- tolower(ages$Plot.Name)
ages <- subset(ages, Age < 3)

#include only the plots that are shown to be secondary.
condit <- rbind(subset(condit, plot %in% ages$Plot.Name),
                subset(condit, plot == 'cocoli' | plot == 'sherman'))
#there were multiple subplots of the cocoli region and sherman region in the 'ages' table,
#so the naming did not match between the two dataframes.
#all those were secondary, so 'cocoli' and 'sherman' were kept in the condit file

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###### remove years in which a large tree disappeared, and then reappeared
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#look at what's happening at plots that have odd changes of max DBH.
by(condit, condit$plot, function(x){return(tapply(x$dbh,x$year,max))})

condit <- remove_outlier_years(condit)

#years in which one of the top 5 trees has disappeared and then reappeared have been removed

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###### get annual mean per plot
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#I could use some help adding a switch in this section.

#get annual sums of each variable per plot
condit_yearly <- aggregate(cbind(dbh,agb,ba)  ~ year + plot, condit, sum)
condit_yearly <- rename(condit_yearly, c(sum_dbh = dbh,
                                         sum_agb = agb,
                                         sum_ba = ba))
condit_yearly <- merge(condit_yearly, condit)
head(condit_yearly)
condit_yearly <- subset(condit_yearly, select=-c(dbh,agb,ba,hom,stemID,date,gx,gy,sp)) #remove columns that considered individual stem-level changes at sub-year resolution
condit_yearly <- condit_yearly[order(condit_yearly$plot),]
condit_yearly <- unique(condit_yearly)



#get annual means of each variable per plot
# condit_yearly <- aggregate(cbind(dbh,agb,ba)  ~ year + plot, condit, mean)
# condit_yearly <- rename(condit_yearly, c(mean_dbh = dbh,
#                                          mean_agb = agb,
#                                          mean_ba = ba))
# condit_yearly <- merge(condit_yearly, condit)
# head(condit_yearly)
# condit_yearly <- subset(condit_yearly, select=-c(dbh,agb,ba,hom,stemID,date,gx,gy,sp)) #remove columns that considered individual stem-level changes at sub-year resolution
# condit_yearly <- condit_yearly[order(condit_yearly$plot),]
# condit_yearly <- unique(condit_yearly)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###### see how number of trees may be changing
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#ongoing

condit_yearly_num_trees <- condit %>%
  group_by(plot) %>%
  group_by(year, .add=TRUE)%>%
  summarise(stemID = n())

condit_yearly_num_trees <- as.data.frame(condit_yearly_num_trees)
condit_yearly_num_trees <- rename(condit_yearly_num_trees, c(trees_num = stemID))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###### plotting the data
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#plot sherman, cocoli and p18 (distribution over time)
sherman <- subset(condit, plot == 'sherman')
cocoli <- subset(condit, plot == 'cocoli')
p18 <- subset(condit, plot == 'p18')

ggplot(sherman, aes(x=dbh)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=15)+
  xlim(0, 500) +
  geom_density(alpha=.2, fill="#FF6666")

ggplot(cocoli, aes(x=dbh)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=15)+
  xlim(0, 500) +
  geom_density(alpha=.2, fill="#FF6666")

#however, all other plots show a bimodal distribution:
ggplot(p18, aes(x=dbh)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=15)+
  xlim(0, 500) +
  geom_density(alpha=.2, fill="#FF6666")

ggplot(condit_yearly, aes(x=year, y=mean_dbh, color = plot, group=plot)) +
  xlab('Date') + ylab('Mean DBH') +
  ggtitle('Mean DBH vs Time')+
  geom_line()+
  geom_point(size=1) +
  geom_text(aes(label=ifelse(plot == "cocoli",as.character(plot),'')),hjust=0,vjust=0)

ggplot(condit_yearly_num_trees, aes(x=year, y=trees_num, color = plot, group=plot)) +
  xlab('Date') + ylab('Total # of trees') +
  ggtitle('Number of trees vs Time')+
  geom_line()+
  geom_point(size=1) +
  geom_text(aes(label=ifelse(plot == "cocoli",as.character(plot),'')),hjust=0,vjust=0)


#just to see if nothing changes

#remove cocoli, sherman and all readings below 100 in dbh

data <- rbind(subset(condit, condit$dbh >= 100 & plot != 'cocoli' & plot != 'sherman'))


condit_yearly <- aggregate(cbind(dbh,agb,ba)  ~ year + plot, data, sum)
condit_yearly <- rename(condit_yearly, c(sum_dbh = dbh,
                                         sum_agb = agb,
                                         sum_ba = ba))
condit_yearly <- merge(condit_yearly, data)
head(condit_yearly)
condit_yearly <- subset(condit_yearly, select=-c(dbh,agb,ba,hom,stemID,date,gx,gy,sp)) #remove columns that considered individual stem-level changes at sub-year resolution
condit_yearly <- condit_yearly[order(condit_yearly$plot),]
condit_yearly <- unique(condit_yearly)


ggplot(condit_yearly, aes(x=year, y=sum_dbh, color = plot, group=plot)) +
  xlab('Date') + ylab('sum DBH') +
  ggtitle('sum DBH vs Time')+
  geom_line()+
  geom_point(size=1)

#nothing significant changed.
