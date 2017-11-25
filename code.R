#MADELINE M. WARNDORF HON400 THESIS 2017
#IMPORT DATA:
# USE importData.R file
#
# This is the dataframe pulled from the full dataset from the Excel Sheet. #
# VARIABLES extracted from Excel Sheet:
#   for case years 07-09:
#     casenum, statenum, citynum, countynum, latitude, longitude,
#     vnumber (vehicle number), vfatcount (vehicle fatal count), accday,
#     accmon, accyr, accdate, modelyr, make, drf1 (driver related factors),
#     drf2, drf3, drf4
#   for case years 10-15:
#     casenum, statenum, citynum, countynum, latitude, longitude,
#     vnumber (vehicle number), vfatcount (vehicle fatal count), accday,
#     accmon, accyr, accdate, modelyr, make, drf1 (driver related factors),
#     drf2, drf3, drf4, dridistract1, dridistract2

# PACKAGES USED -----------------------------------------------------------
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("knitr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("gridExtra", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("png", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("pander", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("car", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

# SETTING UP DATAFRAME FROM EXCEL DATAFRAME ------------------------------
# df = FARS_YY dataframe (df) (ex: FARS_07) 
# YYYY = case year of the Excel dataframe (ex: 2007)
fatal_YYsetup <- function(df, YYYY) {
  #df = FARS_YY
  #YYYY = case year
  y = YYYY
  if (y >= 2007 & y <= 2009) {
    fataldf <- as.data.frame(cbind.data.frame(df$icasenum, df$istatenum, df$icity, df$icounty,
                                   df$latitude, df$longitude, df$ivnumber, df$vfatcount,
                                   df$iaccday, df$iaccmon, df$iaccyr, df$saccdate, 
                                   df$imodelyr, df$imake, df$idrf1, df$idrf2, 
                                   df$idrf3, df$idrf4), header = TRUE, as.is = TRUE)
    colnames(fataldf) <- c("casenum", "statenum", "citynum","countynum","lat","long",
                           "vehnum", "numfatalveh", "day", "month", "year","date", "modelyr", "make",
                           "drf1", "drf2", "drf3", "drf4")
    #START Getting Date to be a Date
    fataldf$date <- df$saccdate
    fataldf$date <-format.POSIXct(fataldf$date, format = "%m/%Y")
    #END
    #Fixing city and county as num
    fataldf$citynum <- as.integer(fataldf$citynum)
    fataldf$countynum <- as.integer(fataldf$countynum)
    fataldf$lat <- as.character(fataldf$lat)
    fataldf$long <- as.character(fataldf$long) 
    fataldf <- unique(fataldf)
    }
  else {
    fataldf <- as.data.frame(cbind.data.frame(df$casenum, df$statenum, df$city, df$county, df$latitude,
                                   df$longitude, df$vnumber, df$vfatcount, df$accday, df$accmon,
                                   df$caseyear,df$accdate, df$modelyr, df$make, 
                                   df$dridistract1, df$dridistract2), 
                             header = TRUE, as.is = TRUE, stringsAsFactor=FALSE)
    colnames(fataldf) <- c("casenum", "statenum", "citynum", "countynum", "lat", "long",
                           "vehnum", "numfatalveh", "day", "month", 
                           "year","date", "modelyr", "make", "dridistract1", "dridistract2")
    #START Getting Date to be a Date
    fataldf$date <- df$accdate
    fataldf$date <-format.POSIXct(fataldf$date, format = "%m/%Y")
    #END
    #Fixing city and county as num
    fataldf$citynum <- as.integer(fataldf$citynum)
    fataldf$countynum <- as.integer(fataldf$countynum)
    fataldf$lat <- as.character(fataldf$lat)
    fataldf$long <- as.character(fataldf$long)
    fataldf <- unique(fataldf)
    }
  return(fataldf)
}

## STORE RESULT OF FUNCTION IN MAIN DATA FRAME ##
#INPUT EXAMPLE:   fatal_07 <- fatal_YYsetup(X2007txtToExcel, 2007) #
fatal_07 <- fatal_YYsetup(FARS_07, 2007)
fatal_08 <- fatal_YYsetup(FARS_08, 2008)
fatal_09 <- fatal_YYsetup(FARS_09, 2009)
fatal_10 <- fatal_YYsetup(FARS_10, 2010)
fatal_11 <- fatal_YYsetup(FARS_11, 2011)
fatal_12 <- fatal_YYsetup(FARS_12, 2012)
fatal_13 <- fatal_YYsetup(FARS_13, 2013)
fatal_14 <- fatal_YYsetup(FARS_14, 2014)
fatal_15 <- fatal_YYsetup(FARS_15, 2015)


# MAKING UNIQUE DF AND FINDING CELL CASES ---------------------------------
#Function for making unique dataframe based on case number and state number
#INPUT: df = fatal_YY
#OUTPUT: unique data frame by case number and state number
makingUniqueCases <- function(df) {
  #df = casedf
  ndf <- df
  ndf$duplicate <- "no"
  lengthcases <- length(df$casenum)
  rownames(df) <- c(1:length(df$casenum))
  i = 1
  for (w in 1:lengthcases) {
    testrow <- ndf[i,]
    if (w != i){
      comparerow <- df[w,]
      if (testrow[1,1] == comparerow[1,1]) {
        if(testrow[1,2] == comparerow[1,2]){
          ndf[i,]$duplicate <- "yes"
        } else {
          ndf[i,]$duplicate <- "no"
        }
      } else {
        ndf[i,]$duplicate <- "no"
      }
    } else {
      next
    }
    i = i + 1
  } #close for loop
  ndf <- ndf[ndf$duplicate != "yes",]
  return(ndf)
}

#Function for returning distracted driving subdataframe
#INPUT: df = fatal_YY YY = case year
#OUTPUT: fataldisdf
findingCellCases <- function(df, YY) {
  #df = fatal_YY
  #YY = case year
  if (YY >= 07 & YY <= 09) {
    setupfataldis0709 <- function(df) {
      #df = fatal_YY
      fataldisYY <- df
      setupdrf <- function(fataldisYY){
        ndf <- fataldisYY
        ndf$drf <- ifelse(ndf$drf1 == 93, 93, 
                          ifelse(ndf$drf2 == 93, 93, 
                                 ifelse(ndf$drf3 == 93, 93,
                                        ifelse(ndf$drf4 == 93, 93,
                                               ifelse(ndf$drf1 == 94, 94,
                                                      ifelse(ndf$drf2 == 94, 94,
                                                             ifelse(ndf$drf3 == 94, 94,
                                                                    ifelse(ndf$drf4 == 94, 94, 0)))))))) 
        #93 = cellular telephone present in vehicle
        #94 = cellular telephone in use in vehicle
        ndf <- ndf[ndf$drf != 0, ]
        drop <- c("drf1", "drf2", "drf3", "drf4")
        ndf = ndf[,!(names(ndf) %in% drop)]
        return(ndf)
      }
      fataldisYY <- setupdrf(fataldisYY)
      return(fataldisYY)
    } 
    fataldisYY <- setupfataldis0709(df) }
  else {
    setupfataldis1015 <- function(df) {
      setupdrf <- function(df){
        df$drf <- ifelse(df$dridistract1 == 5, 5,
                         ifelse(df$dridistract2 == 5, 5,
                                ifelse(df$dridistract1 == 6, 6,
                                       ifelse(df$dridistract2 == 6, 6, 
                                              ifelse(df$dridistract1 == 15, 15,
                                                     ifelse(df$dridistract2 == 15, 15, 0))))))
        #5 = Talking or listening to cell phone
        #6 = While dialing Cell phone
        #15 = Other cell phone distraction
        df <- df[df$drf != 0, ]
        drop <- c("dridistract1", "dridistract2")
        df = df[,!(names(df) %in% drop)]
        return(df)
      }
      fataldisYY <- setupdrf(df)
      return(fataldisYY)
    }
    fataldisYY <- setupfataldis1015(df)
  }
  return(fataldisYY)
}

drf_07 <- findingCellCases(fatal_07, 07)
drf_08 <- findingCellCases(fatal_08, 08)
drf_09 <- findingCellCases(fatal_09, 09)
drf_10 <- findingCellCases(fatal_10, 10)
drf_11 <- findingCellCases(fatal_11, 11)
drf_12 <- findingCellCases(fatal_12, 12)
drf_13 <- findingCellCases(fatal_13, 13)
drf_14 <- findingCellCases(fatal_14, 14)
drf_15 <- findingCellCases(fatal_15, 15)

# FINDING NUM OF CASES and NUM OF DISTRACTED ------------------------------
#counting total number of cases in the calendar year by case
#INPUT: df = fatal_YY
#OUTPUT: number of total cases 
countOfCases <- function(df) {
  #df = fatal_YY
  casedf <- as.data.frame(cbind.data.frame(df$casenum, df$statenum), 
                          header = TRUE, as.is = TRUE)
  colnames(casedf) <- c("casenum", "statenum")
  casedf <- unique(casedf)
  casedf <- makingUniqueCases(casedf)
  #for storing the total number of cases that were fatal
  casetotalsum <- 0
  x <- count(casedf)
  casetotalsum <- sum(x$freq)
  return(casetotalsum)
}

#Function for counting distracted driving cases
#INPUT: df = fatal_YY YY = YY
#OUTPUT: number of distracted driving cases by vehicle involving cellphones 
countOfDisCases <- function(df, YY) {
  fataldisYY <- findingCellCases(df, YY)
  #df is the new dataframe
  df <- fataldisYY
  casedf <- as.data.frame(cbind.data.frame(df$casenum, df$statenum), 
                          header = TRUE, as.is = TRUE)
  colnames(casedf) <- c("casenum", "statenum")
  casedf <- unique(casedf)
  #for storing the total number of cases that were fatal
  casedissum <- 0
  x <- count(casedf)
  casedissum <- sum(x$freq)
  return(casedissum)
}


# NHTSA TECHNOLOGY --------------------------------------------------------

#Function for counting number of cases with fatalitites in the car
#INPUT: df = fatal_YY
#OUTPUT: number of cases with fatalities in the car
countingFatalInCars <- function(df) {
  #df = fatal_YY
  casedf <- as.data.frame(cbind.data.frame(df$casenum, df$statenum, df$numfatalveh), 
                          header = TRUE, as.is = TRUE)
  colnames(casedf) <- c("casenum", "statenum","numfatalveh")
  casedf <- casedf[casedf$numfatalveh != 0,]
  casedf <- unique(casedf)
  newcasedf <- makingUniqueCases(casedf)
  casetotalsum <- 0
  x <- count(casedf)
  casetotalsum <- sum(x$freq)
  return(casetotalsum)
}

#Function for creating data frame FatalCases (FC) o.f. TotalCases (TC) with Distracted Cases o.f. TC
#INPUT: df = fatal_YY
#OUTPUT: data frame with year, numFC, numDC, numTC, FCofTC <- as %, DCofTC <- as %
makingNHTSAdf <- function(df, YY){
  #df = fatal_YY
  #YY = YY
  #FC = fatal cases
  #TC = total cases
  year <- unique(df$year)
  FCvsCP <- as.data.frame(year)
  colnames(FCvsCP) <- "caseyear"
  fatalcases <- countingFatalInCars(df)
  FCvsCP$numFC <- fatalcases
  numdistract <- countOfDisCases(df, YY)
  FCvsCP$numCP <- numdistract
  totalcases <- countOfCases(df)
  FCvsCP$numTC <- totalcases
  FCvsCP$FCofTC <- round(((fatalcases/totalcases) * 100), digits = 4)
  FCvsCP$CPofTC <- round(((numdistract/totalcases)*100), digits = 4)
  return(FCvsCP)
}

#making combined data frame to pull into table
combinedFC_vs_CP <- function(){
  df07 <- makingNHTSAdf(fatal_07, 07)
  df07$technology <- "ESC"
  df08 <- makingNHTSAdf(fatal_08, 08)
  df08$technology <- "ESC"
  df09 <- makingNHTSAdf(fatal_09, 09)
  df09$technology <- "ESC"
  df10 <- makingNHTSAdf(fatal_10, 10)
  df10$technology <- "ESC"
  df11 <- makingNHTSAdf(fatal_11, 11)
  df11$technology <- "ESC"
  df12 <- makingNHTSAdf(fatal_12, 12)
  df12$technology <- "EM"
  df13 <- makingNHTSAdf(fatal_13, 13)
  df13$technology <- "EM"
  df14 <- makingNHTSAdf(fatal_14, 14)
  df14$technology <- "EM"
  df15 <- makingNHTSAdf(fatal_15, 15)
  df15$technology <- "EM"
  #combining into one df
  combineddf <- rbind(df07,df08, df09, df10, df11, df12, df13, 
                      df14, df15)
  return(combineddf)
}
combinedFC_vs_CP <- combinedFC_vs_CP()

#Table CSV combinedFC_vs_DC To Paste into .txt File to Paste into Word for Table
#Function for making and saving combinedFC_vs_DC Table 
#INPUT: df = combinedFC_vs_DC
#OUTPUT: saved .CSV file of table
tableToCSV <- function(df) {
  #df = combinedFC_vs_DC
  row.names(df)<- NULL
  colnames(df) <- c("caseYear","numFatalCase(FC)","numCellPhone(CP)","numTotalCase(TC)", 
                    "FCofTC","CPofTC","TechnologyPhase")
  write.csv(df, file = "combinedFatalvsDistractTable.csv")
}
tableToCSV(combinedFC_vs_CP)

#Plot
NHTSAFCplot <- function() {
  #df = combinedFC_vs_CP
  mydata <- read.table("fcdc.txt",header = TRUE,stringsAsFactors = FALSE)
  par(mfrow=c(1,1))
  x = mydata$caseYear
  xmin = min(mydata$caseYear)
  xmax = max(mydata$caseYear)
  ymin2 = min(mydata$FCofTC) - 0.5
  ymax2 = max(mydata$FCofTC) + 0.5
  y2 = mydata$FCofTC
  plot(x,y2, type = "o", main = "Cases with Fatalities in Vehicle (%)", xlab = "Case Year", ylab ="Fatalities in Vehicle Cases(%)", xlim = c(xmin, xmax),
       ylim = c(ymin2, ymax2))
}
NHTSAFCplot()

NHTSACPplot <- function(){
  mydata <- read.table("fcdc.txt",header = TRUE,stringsAsFactors = FALSE)
  x = mydata$caseYear
  y1 = mydata$CPofTC
  xmin = min(mydata$caseYear)
  xmax = max(mydata$caseYear)
  ymin1 = min(mydata$CPofTC) - 0.10
  ymax1 = max(mydata$CPofTC) + 0.10
  plot(x,y1, type = "o", main = "Cases with Cell Phone Usage (%)", xlab = "Case Year", ylab ="Cell Phone Cases (%)", xlim = c(xmin, xmax),
       ylim = c(ymin1, ymax1))
}
NHTSACPplot()

NHTSAplot <- function(){
  mydata <- read.table("fcdc.txt",header = TRUE,stringsAsFactors = FALSE)
  par(mfrow = c(2,1))
  x = mydata$caseYear
  y1 = mydata$CPofTC
  xmin = min(mydata$caseYear)
  xmax = max(mydata$caseYear)
  ymin1 = min(mydata$CPofTC) - 0.10
  ymax1 = max(mydata$CPofTC) + 0.10
  
  y2 = mydata$FCofTC
  ymin2 = min(mydata$FCofTC) - 0.5
  ymax2 = max(mydata$FCofTC) + 0.5
  
  plot(x,y1, type = "o", main = "Cases with Cell Phone Usage (%)", xlab = "Case Year", ylab ="Cell Phone Cases (%)", xlim = c(xmin, xmax),
       ylim = c(ymin1, ymax1))

  plot(y1,y2,main = "Cases with Cell Phone Usage (%) vs. Fatalities in Vehicle Cases (%)", xlab = "Cell Phone Cases (%)",
       ylab ="Fatalities in Vehicle Cases(%)", xlim = c(ymin1,ymax1),
       ylim = c(ymin2, ymax2))
  abline(lm(y2~y1),col="red")
  lines(lowess(y1,y2),col="blue")
}
NHTSAplot()

mydata2 <- read.table("fcdcraw.txt",header = TRUE,stringsAsFactors = FALSE)
lm(mydata2)
#Intercept -8.1896 FCofTC 0.2025
asex <- lm(mydata2)
summary(asex)
sd(mydata2$CPofTC)
sd(mydata2$FCofTC)

#FOR CREATING SINGLE DF OF CASE YEAR, CASE DISTRACT TOTAL, CASE TOTAL
#This function is for creating df by years
creatingCaseTotaldf1 <- function(df, YY) { 
  #df = fatal_YY
  #YY = case year
  year <- unique(df$year)
  casedf <- as.data.frame(year)
  #sum of distracted driving cases
  numdistract <- countOfDisCases(df, YY)
  casedf$numdistract <- numdistract
  #sum of total fatal cases for the year
  numtotalcases <- countOfCases(df)
  casedf$numtotalcases <- numtotalcases
  #distracted divided by total case
  CPofTC <- (numdistract/numtotalcases)*100
  casedf$CPofTC <- CPofTC
  return(casedf)
}



#FOR CREATING COMBINED DISTRACTED VS. TOTAL CASES DF
creatingCombinedCaseTotaldf <- function(){
  df07 <- creatingCaseTotaldf1(fatal_07, 07)
  df08 <- creatingCaseTotaldf1(fatal_08, 08)
  df09 <- creatingCaseTotaldf1(fatal_09, 09)
  df10 <- creatingCaseTotaldf1(fatal_10, 10)
  df11 <- creatingCaseTotaldf1(fatal_11, 11)
  df12 <- creatingCaseTotaldf1(fatal_12, 12)
  df13 <- creatingCaseTotaldf1(fatal_13, 13)
  df14 <- creatingCaseTotaldf1(fatal_14, 14)
  df15 <- creatingCaseTotaldf1(fatal_15, 15)
  
  #combining into one df
  combineddf <- rbind(df07, df08, df09, df10, df11, df12, df13, 
                      df14, df15)
  return(combineddf)
}
combinedCP_vs_TC <- creatingCombinedCaseTotaldf()



# MAKES ANALYSIS -----------------------------------------------------------
#MAKES: FARS CODE : MAKE NAME
#   12 : Ford
#   19 : Cadillac
#   42 : Mercedes-Benz
#   51 : Volvo
#CREATING MAKES TABLE
creatingMakestable <- function(){
  makesFARScode <- c(12,19,42,51)
  makeNames <- c("Ford", "Cadillac", "Mercedes-Benz", "Volvo")
  makestable <- as.data.frame(cbind(as.numeric(makesFARScode), 
                                    as.character(makeNames)), 
                              stringAsFactors = FALSE)
  colnames(makestable) <- c("FARScode", "makename")
  return(makestable)
}

#CP vs. TC by MAKE
countOfCasesMake <- function(df, makenum) {
  #df = fatal_YY
  mn <- makenum
  df$makenump <- ifelse(df$make == mn, 1, 0)
  df <- df[df$makenump != 0, ]
  casedf <- as.data.frame(cbind.data.frame(df$casenum, df$statenum, df$make), 
                          header = TRUE, as.is = TRUE)
  colnames(casedf) <- c("casenum", "statenum", "make")
  casedf <- unique(casedf)
  #for storing the total number of cases that were fatal
  casetotalsum <- 0
  x <- count(casedf)
  casetotalsum <- sum(x$freq)
  return(casetotalsum)
}

countOfDisCasesMake <- function(df, YY, makenum) {
  mn <- makenum
  fataldisYY <- findingCellCases(df, YY)
  #df is the new dataframe
  df <- fataldisYY
  df$makenump <- ifelse(df$make == mn, 1, 0)
  df <- df[df$makenump != 0, ]
  casedf <- as.data.frame(cbind.data.frame(df$casenum, df$statenum, df$make), 
                          header = TRUE, as.is = TRUE)
  colnames(casedf) <- c("casenum", "statenum", "make")
  casedf <- unique(casedf)
  #for storing the total number of cases that were fatal
  casedissum <- 0
  x <- count(casedf)
  casedissum <- sum(x$freq)
  return(casedissum)
}

countingFatalMakeInCars <- function(df, makenum) {
  #df = fatal_YY
  mn <- makenum
  df$makenump <- ifelse(df$make == mn, 1, 0)
  df <- df[df$makenump != 0, ]
  casedf <- as.data.frame(cbind.data.frame(df$casenum, df$statenum, df$numfatalveh), 
                          header = TRUE, as.is = TRUE)
  colnames(casedf) <- c("casenum", "statenum","numfatalveh")
  casedf <- casedf[casedf$numfatalveh != 0,]
  casedf <- unique(casedf)
  casetotalsum <- 0
  x <- count(casedf)
  casetotalsum <- sum(x$freq)
  return(casetotalsum)
}

creatingMakeCaseTotaldf <- function(df, YY, makenum) {
  #df = fatal_YY
  #YY = year
  #makenum = FARS make code number
  df1 <- df
  numMakeTC <- countOfCasesMake(df1, makenum)
  numMakeCP <- countOfDisCasesMake(df1, YY, makenum)
  numMakeFC <- countingFatalMakeInCars(df1,makenum)
  year <- unique(df1$year)
  casedf <- as.data.frame(year)
  #sum of fatal driving cases
  casedf$numMakeFC <- numMakeFC
  numFCTot <- countingFatalInCars(df1)
  casedf$numFCTotal <- numFCTot
  #sum of distracted driving cases
  casedf$numMakeCP <- numMakeCP
  numDistTot <- countOfDisCases(df1, YY)
  casedf$numCPTotal <- numDistTot

  #sum of total fatal cases for the year
  casedf$numMakeTC <- numMakeTC
  numTotalCase <- countOfCases(df1)
  casedf$numTotalCase <- numTotalCase
  # make distracted divided by make total cases (%)
  #MakeCPofMakeTC <- round(((numMakeCP/numMakeTC)*100), digits = 4)
  #if(MakeCPofMakeTC == "NaN"){
  #  MakeCPofMakeTC <- 0
  #}
  #casedf$MakeCPofMakeTC <- MakeCPofMakeTC
  
  #make fatal divided by total fatal (%)
  MakeFCofTC <- round(((numMakeFC/numTotalCase)*100),digits = 4)
  casedf$MakeFCofTC <- MakeFCofTC
  # make distracted divided by total distracted (%)
  MakeCPofTC <- round(((numMakeCP/numTotalCase)*100), digits = 4)
  casedf$MakeCPofTC <- MakeCPofTC
  


  # make total divided by total cases (%)
  MakeTCofTC <- round(((numMakeTC/numTotalCase)*100), digits = 4)
  casedf$MakeTCofTC <- MakeTCofTC
  
  return(casedf)
}
  
#Dataframe of Make
#VARIABLES
# year (calendar year), makedistract, totaldistract, maketotal, totalcases, makeDofDisC (make distract of 
# total distract), makeTCofTC (make total cases out of total cases)
makedistractdf <- function(makenum) {
  m <- makenum
  df07 <- creatingMakeCaseTotaldf(fatal_07, 07, m)
  df08 <- creatingMakeCaseTotaldf(fatal_08, 08, m)
  df09 <- creatingMakeCaseTotaldf(fatal_09, 09, m)
  df10 <- creatingMakeCaseTotaldf(fatal_10, 10, m)
  df11 <- creatingMakeCaseTotaldf(fatal_11, 11, m)
  df12 <- creatingMakeCaseTotaldf(fatal_12, 12, m)
  df13 <- creatingMakeCaseTotaldf(fatal_13, 13, m)
  df14 <- creatingMakeCaseTotaldf(fatal_14, 14, m)
  df15 <- creatingMakeCaseTotaldf(fatal_15, 15, m)
  
  makedf <- rbind(df07, df08, df09, df10, df11, df12, df13, 
                  df14, df15)
  return(makedf)
}
#for Ford (FARS Code 12)
fordCPdf <- makedistractdf(12)

#for Cadillac (FARS Code 19)
cadillacCPdf <- makedistractdf(19)

#for Mercedes-Benz (FARS Code 42)
mercedesCPdf <- makedistractdf(42)

#for Volvo (FARS Code 51)
volvoCPdf <- makedistractdf(51)

#Table CSV makeCPdf To Paste into .txt File to Paste into Word for Table
#Function for making and saving makedisdf Table 
#INPUT: df = makeCPdf
#OUTPUT: saved .CSV file of table
tableToMakeCSV <- function(df) {
  #df = makeCPdf
  row.names(df)<- NULL
  colnames(df) <- c("caseYear","numMakeFatalCase(FC)","numFCTotal","numMakeCellPhone(CP)",
                    "numCPTotal","numMakeTotalCase(TC)", 
                    "numTCTotal","MakeFCofTC","MakeCPofTC","MakeTCofTC")
  write.csv(df, file = "makevolvo.csv")
}
tableToMakeCSV(volvoCPdf)

#To get the Mean, SD, Linear Regression Formula, R-Value, T-Value, and P-value
myforddata <- read.table("makefordFCCP.txt",header = TRUE,stringsAsFactors = FALSE)
mycadillacdata <- read.table("makecadillacFCCP.txt",header = TRUE,stringsAsFactors = FALSE)
mymercedesdata <- read.table("makemercedesFCCP.txt",header = TRUE,stringsAsFactors = FALSE)
myvolvodata <- read.table("makevolvoFCCP.txt",header = TRUE,stringsAsFactors = FALSE)

#FORD
summary(myforddata)
sd(myforddata$MakeCPofTC)
sd(myforddata$MakeFCofTC)
lm(myforddata)
#Intercept: 1.5035 MakeFCofTC: -0.1891
asex <- lm(myforddata)
summary(asex)

#CADILLAC
summary(mycadillacdata)
sd(mycadillacdata$MakeCPofTC)
sd(mycadillacdata$MakeFCofTC)
lm(mycadillacdata)
#Intercept: -0.004785 MakeFCofTC: 0.03477
asex <- lm(mycadillacdata)
summary(asex)

#MERCEDES
summary(mymercedesdata)
sd(mymercedesdata$MakeCPofTC)
sd(mymercedesdata$MakeFCofTC)
lm(mymercedesdata)
#Intercept: 0.0504 MakeFCofTC: -0.05505
asex <- lm(mymercedesdata)
summary(asex)

#VOLVO
summary(myvolvodata)
sd(myvolvodata$MakeCPofTC)
sd(myvolvodata$MakeFCofTC)
lm(myvolvodata)
#Intercept: -0.007633 MakeFCofTC: 0.125155
asex <- lm(myvolvodata)
summary(asex)


MakeFCandCPplots <- function(makename) {
  #makename = "Ford" or "Cadillac" etc.
  mn <- makename
  if(mn == "Ford"){
    mydata <- read.table("makefordfull.txt",header = TRUE,stringsAsFactors = FALSE)
  } else{
    if(mn == "Cadillac"){
      mydata <- read.table("makecadillacfull.txt",header = TRUE,stringsAsFactors = FALSE)
    } else {
      if(mn == "Mercedes"){
        mydata <- read.table("makemercedesfull.txt",header = TRUE,stringsAsFactors = FALSE)
      } else {
        mydata <- read.table("makevolvofull.txt",header = TRUE,stringsAsFactors = FALSE)
      }
    }
  }

  par(mfrow=c(2,1),mar=c(5.5,4,4,2)+0.1)
  x = mydata$caseYear
  xmin = min(mydata$caseYear)
  xmax = max(mydata$caseYear)
  ymin2 = min(mydata$MakeFCofTC)
  ymax2 = max(mydata$MakeFCofTC)
  y2 = mydata$MakeFCofTC

  plot(x,y2, type = "o", main = "Cases with Fatalities in Vehicle (%)",sub = makename, xlab = "Case Year", ylab ="Fatalities in Vehicle Cases(%)", xlim = c(xmin, xmax),
       ylim = c(ymin2, ymax2))
  
  y1 = mydata$MakeCPofTC
  ymin1 = min(mydata$MakeCPofTC) - 0.10
  ymax1 = max(mydata$MakeCPofTC) + 0.10
  plot(x,y1, type = "o", main = "Cases with Cell Phone Usage (%)", sub = makename,xlab = "Case Year", ylab ="Cell Phone Cases (%)", xlim = c(xmin, xmax),
       ylim = c(ymin1, ymax1))
}
MakeFCandCPplots("Ford")
MakeFCandCPplots("Cadillac")
MakeFCandCPplots("Mercedes")
MakeFCandCPplots("Volvo")


MakeFCCPplot <- function(){

  
  par(mfrow = c(2,2))
  #Ford
  x1 = myforddata$MakeCPofTC
  y1 = myforddata$MakeFCofTC
  xmin1 = min(myforddata$MakeCPofTC)
  xmax1 = max(myforddata$MakeCPofTC)
  ymin1 = min(myforddata$MakeFCofTC)
  ymax1 = max(myforddata$MakeFCofTC)
  
  plot(x1,y1,main = "Ford", 
       xlab = "Cell Phone Cases (%)",
       ylab ="Fatalities in Vehicle Cases(%)", xlim = c(xmin1,xmax1),
       ylim = c(ymin1, ymax1))
  abline(lm(y1~x1),col="red")
  lines(lowess(x1,y1),col="blue")
  
  #Cadillac
  x2 = mycadillacdata$MakeCPofTC
  y2 = mycadillacdata$MakeFCofTC
  xmin2 = min(mycadillacdata$MakeCPofTC)
  xmax2 = max(mycadillacdata$MakeCPofTC)
  ymin2 = min(mycadillacdata$MakeFCofTC)
  ymax2 = max(mycadillacdata$MakeFCofTC)
  
  plot(x2,y2,main = "Cadillac", 
       xlab = "Cell Phone Cases (%)",
       ylab ="Fatalities in Vehicle Cases(%)", xlim = c(xmin2,xmax2),
       ylim = c(ymin2, ymax2))
  abline(lm(y2~x2),col="red")
  lines(lowess(x2,y2),col="blue")
  
  #Mercedes
  x3 = mymercedesdata$MakeCPofTC
  y3 = mymercedesdata$MakeFCofTC
  xmin3 = min(mymercedesdata$MakeCPofTC)
  xmax3 = max(mymercedesdata$MakeCPofTC)
  ymin3 = min(mymercedesdata$MakeFCofTC)
  ymax3 = max(mymercedesdata$MakeFCofTC)
  
  plot(x3,y3,main = "Mercedes", 
       xlab = "Cell Phone Cases (%)",
       ylab ="Fatalities in Vehicle Cases(%)", xlim = c(xmin3,xmax3),
       ylim = c(ymin3, ymax3))
  abline(lm(y3~x3),col="red")
  lines(lowess(x3,y3),col="blue")
  
  #Volvo
  x4 = myvolvodata$MakeCPofTC
  y4 = myvolvodata$MakeFCofTC
  xmin4 = min(myvolvodata$MakeCPofTC)
  xmax4 = max(myvolvodata$MakeCPofTC)
  ymin4 = min(myvolvodata$MakeFCofTC)
  ymax4 = max(myvolvodata$MakeFCofTC)
  
  plot(x4,y4,main = "Volvo", 
       xlab = "Cell Phone Cases (%)",
       ylab ="Fatalities in Vehicle Cases(%)", xlim = c(xmin4,xmax4),
       ylim = c(ymin4, ymax4))
  abline(lm(y4~x4),col="red")
  lines(lowess(x4,y4),col="blue")
}
MakeFCCPplot()


# CELL PHONE ANALYSIS BY LOCATION -----------------------------------------
#STATES AND YEARS FOR CAMPAIGNS:
# NY and CT 2009
# Nationwide 2014

#State Table
statenumb <- c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56)
#removes Puerto Rico and Virgin Islands
stateabbFARS <- c("AL","AK","AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS","KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT","NE", "NV", "NH", "NJ", "NM", "NY","NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

statetable <- as.data.frame(cbind(as.numeric(statenumb),as.character(stateabbFARS)), stringsAsFactors = FALSE)
colnames(statetable) <- c("statenum","stateabbFARS")
statetable$statenum <- as.numeric(statetable$statenum)
statetable$stateabbFARS <- as.character(statetable$stateabbFARS)

#Counting Cases by State (TotalCasesByState)
countOfCasesByState <- function(df) {
  #df = fatal_YY
  dfs <- df
  year <- unique(dfs$year)
  caseYear = numeric()
  statename = character()
  statenum = numeric()
  totalCases = numeric()
  listcount <- 1
  for(i in statenumb){
    caseYear[listcount] <- year
    statename[listcount] <- stateabbFARS[listcount]
    statenum[listcount] <- i
    addingToTable <- function(i){
      dfn <- dfs
      dfn$statenumbp <- ifelse(df$statenum == i, 1, 0)
      dfn <- dfn[dfn$statenumbp != 0, ]
      dft <- as.data.frame(cbind.data.frame(dfn$casenum, dfn$statenum), 
                              header = TRUE, as.is = TRUE)
      colnames(dft) <- c("casenum", "statenum")
      dft <- unique(dft)
      casetotalsum <- 0
      x <- count(dft)
      casetotalsum <- sum(x$freq)
      return(casetotalsum)
    }
    totalCases[listcount] <- addingToTable(i)
    listcount = listcount + 1
  }
  dfstate <- data.frame(caseYear,statename,statenum,totalCases, stringsAsFactors = FALSE)
  return(dfstate)
}

countOfDisCasesByState <- function(df, YY) {
  #df = fatal_YY
  fataldisYY <- findingCellCases(df, YY)
  #dfn is the new dataframe
  dfn <- fataldisYY
  year <- unique(dfn$year)
  caseYear = numeric()
  statename = character()
  statenum = numeric()
  StateCP = numeric()
  listcount <- 1
  for(i in statenumb){
    caseYear[listcount] <- year
    statename[listcount] <- stateabbFARS[listcount]
    statenum[listcount] <- i
    addingToTable <- function(i){
      dfs <- dfn
      dfs$statenumbp <- ifelse(dfn$statenum == i, 1, 0)
      dfs <- dfs[dfs$statenumbp != 0, ]
      dft <- as.data.frame(cbind.data.frame(dfs$casenum, dfs$statenum), 
                           header = TRUE, as.is = TRUE)
      colnames(dft) <- c("casenum", "statenum")
      dft <- unique(dft)
      casetotalsum <- 0
      x <- count(dft)
      casetotalsum <- sum(x$freq)
      return(casetotalsum)
    }
    StateCP[listcount] <- addingToTable(i)
    listcount = listcount + 1
  }
  dfstate <- data.frame(caseYear,statename,statenum,StateCP, stringsAsFactors = FALSE)
  return(dfstate)
}


creatingCaseTotalByStatedf <- function(df, YY) {
  #df = fatal_YY
  #YY = year
  df1 <- df
  
  #cell phone cases
  numCPdf <- countOfDisCasesByState(df1, YY)
  casedf <- numCPdf
  numDistTot <- countOfDisCases(df1, YY)
  casedf$CPTotal <- numDistTot
  
  #total fatal cases for the year
  numTCdf <- countOfCasesByState(df1)
  casedf$numStateTC <- numTCdf$totalCases
  numTotalCase <- countOfCases(df1)
  casedf$numTotalCase <- numTotalCase
  
  # state distracted divided by total distracted (%)
  casedf$StateCPofTC <- round(((casedf$StateCP/numTotalCase)*100), digits = 4)
  
  
  # state total divided by total cases (%)
  casedf$StateTCofTC <- round(((casedf$numStateTC/numTotalCase)*100), digits = 4)
  
  return(casedf)
}

#FCvsCP cases by states dataframes
FCvsCP_07 <- creatingCaseTotalByStatedf(fatal_07,07)
FCvsCP_08 <- creatingCaseTotalByStatedf(fatal_08,08)
FCvsCP_09 <- creatingCaseTotalByStatedf(fatal_09,09)
FCvsCP_10 <- creatingCaseTotalByStatedf(fatal_10,10)
FCvsCP_11 <- creatingCaseTotalByStatedf(fatal_11,11)
FCvsCP_12 <- creatingCaseTotalByStatedf(fatal_12,12)
FCvsCP_13 <- creatingCaseTotalByStatedf(fatal_13,13)
FCvsCP_14 <- creatingCaseTotalByStatedf(fatal_14,14)
FCvsCP_15 <- creatingCaseTotalByStatedf(fatal_15,15)



pullingStatesNYandCT <- function(state){
  #state = "NY" or "CT"
  if(state == "CT"){
    df07 <- FCvsCP_07[7,]
    df08 <- FCvsCP_08[7,]
    df09 <- FCvsCP_09[7,]
    df10 <- FCvsCP_10[7,]
    df11 <- FCvsCP_11[7,]
    df12 <- FCvsCP_12[7,]
    df13 <- FCvsCP_13[7,]
    df14 <- FCvsCP_14[7,]
    df15 <- FCvsCP_15[7,]
  } else {
    df07 <- FCvsCP_07[33,]
    df08 <- FCvsCP_08[33,]
    df09 <- FCvsCP_09[33,]
    df10 <- FCvsCP_10[33,]
    df11 <- FCvsCP_11[33,]
    df12 <- FCvsCP_12[33,]
    df13 <- FCvsCP_13[33,]
    df14 <- FCvsCP_14[33,]
    df15 <- FCvsCP_15[33,]
  }
  statedf <- rbind(df07, df08, df09, df10, df11, df12, df13, 
                  df14, df15)
  return(statedf)
}

ctCPdf <- pullingStatesNYandCT("CT")
nyCPdf <- pullingStatesNYandCT("NY")

#Table CSV stateCPdf To Paste into .txt File to Paste into Word for Table
#Function for making and saving stateCPdf Table 
#INPUT: df = ctCPdf or nyCPdf
#OUTPUT: saved .CSV file of table
tableToCTorNYYearCSV <- function(df) {
  #df = ctCPdf or nyCPdf
  row.names(df)<- NULL
  colnames(df) <- c("caseYear","stateName", "stateNum"
                    ,"StateCP", "CPTotal",
                    "StateTC", 
                    "TotalCase(CP)","StateCPofTC","StateTCofTC")
  write.csv(df, file = "nyFCvsCP.csv")
}
#tableToCTorNYYearCSV(ctCPdf)
tableToCTorNYYearCSV(nyCPdf)

plotCTandLR <- function(){
  par(mfrow=c(2,1),mar=c(5.5,4,4,2)+0.1)
  x = ctCPdf$caseYear
  xmin = min(ctCPdf$caseYear)
  xmax = max(ctCPdf$caseYear)
  ymin1 = min(ctCPdf$StateCPofTC) 
  ymax1 = max(ctCPdf$StateCPofTC)+ 0.005
  y1 = ctCPdf$StateCPofTC
  
  plot(x,y1, type = "o", 
       main = "Cases with Cell Phone Usage (%)",
       sub = "CT", xlab = "Case Year", 
       ylab ="Cell Phone Cases (%)", xlim = c(xmin, xmax),
       ylim = c(ymin1, ymax1))
  
  x2 = ctCPdf$StateCPofTC
  y2 = ctCPdf$StateTCofTC
  xmin2 = min(ctCPdf$StateCPofTC)
  xmax2 = max(ctCPdf$StateCPofTC)
  ymin2 = min(ctCPdf$StateTCofTC)
  ymax2 = max(ctCPdf$StateTCofTC)
  
  plot(x2,y2,main = "Cell Phones vs. Total Cases", 
       sub = "CT",
       xlab = "Cell Phone Cases (%)",
       ylab ="Total Cases (%)", xlim = c(xmin2,xmax2),
       ylim = c(ymin2, ymax2))
  abline(lm(y2~x2),col="red")
  lines(lowess(x2,y2),col="blue")
}
plotCTandLR()

plotNYandLR <- function(){
  par(mfrow=c(2,1),mar=c(5.5,4,4,2)+0.1)
  x = nyCPdf$caseYear
  xmin = min(nyCPdf$caseYear)
  xmax = max(nyCPdf$caseYear)
  y1 = nyCPdf$StateCPofTC
  ymin1 = min(nyCPdf$StateCPofTC)
  ymax1 = max(nyCPdf$StateCPofTC)+ 0.005
  plot(x,y1, type = "o", 
       main = "Cases with Cell Phone Usage (%)", 
       sub = "NY",xlab = "Case Year", 
       ylab ="Cell Phone Cases (%)", xlim = c(xmin, xmax),
       ylim = c(ymin1, ymax1))
  
  x2 = nyCPdf$StateCPofTC
  y2 = nyCPdf$StateTCofTC
  xmin2 = min(nyCPdf$StateCPofTC)
  xmax2 = max(nyCPdf$StateCPofTC)
  ymin2 = min(nyCPdf$StateTCofTC)
  ymax2 = max(nyCPdf$StateTCofTC)
  
  plot(x2,y2,main = "Cell Phones vs. Total Cases", 
       sub = "NY",
       xlab = "Cell Phone Cases (%)",
       ylab ="Total Cases (%)", xlim = c(xmin2,xmax2),
       ylim = c(ymin2, ymax2))
  abline(lm(y2~x2),col="red")
  lines(lowess(x2,y2),col="blue")
}
plotNYandLR()

#To get the Mean, SD, Linear Regression Formula, R-Value, T-Value, and P-value
myctdata <- read.table("ctCPvsTCsingle.txt",header = TRUE,stringsAsFactors = FALSE)
mynydata <- read.table("nyCPvsTCsingle.txt",header = TRUE,stringsAsFactors = FALSE)

#CT
summary(myctdata)
sd(myctdata$StateCPofTC)
sd(myctdata$StateTCofTC)
lm(myctdata)
#Intercept: 0.00977 StateTCofTC: -0.006055
asex <- lm(myctdata)
summary(asex)

#NY
summary(mynydata)
sd(mynydata$StateCPofTC)
sd(mynydata$StateTCofTC)
lm(mynydata)
#Intercept: -0.02300 StateTCofTC: 0.007711
asex <- lm(mynydata)
summary(asex)


creatingAveragesForNationwide <- function(){
  creatingAveragesdf <- function(df) {
    #df = FCvsCP_YY
    caseYear <- unique(df$caseYear)
    AverageCP = numeric()
    CPTotal <- unique(df$CPTotal)
    TCTotal <- unique(df$numTotalCase)
    AverageCPofTC = numeric()
    
    #cell phone cases
    numCPdf <- df$StateCP
    cpsum <- 0
    for(i in numCPdf){
      cpsum <- cpsum + i
    }
    AverageCP <- round((cpsum)/51, digits = 4)
    AverageCPofTC <- round((AverageCP/TCTotal)*100,digits = 4)
    ndf <- data.frame(caseYear,AverageCP,CPTotal,TCTotal,AverageCPofTC)
    return(ndf)
  }
  df07 <- creatingAveragesdf(FCvsCP_07)
  df08 <- creatingAveragesdf(FCvsCP_08)
  df09 <- creatingAveragesdf(FCvsCP_09)
  df10 <- creatingAveragesdf(FCvsCP_10)
  df11 <- creatingAveragesdf(FCvsCP_11)
  df12 <- creatingAveragesdf(FCvsCP_12)
  df13 <- creatingAveragesdf(FCvsCP_13)
  df14 <- creatingAveragesdf(FCvsCP_14)
  df15 <- creatingAveragesdf(FCvsCP_15)
  statedf <- rbind(df07, df08, df09, df10, df11, df12, df13, 
                   df14, df15)
  return(statedf)
}
nationwideCPdf <- creatingAveragesForNationwide()

#Table CSV nationwideCPdf To Paste into .txt File to Paste into Word for Table
#Function for making and saving stateCPdf Table 
#INPUT: df = nationwideCPdf
#OUTPUT: saved .CSV file of table
tableToNationwideCSV <- function(df) {
  #df = nationwideCPdf
  row.names(df)<- NULL
  colnames(df) <- c("caseYear","AverageCP", "CPTotal",
                    "TCTotal", 
                    "AverageCPofTC")
  write.csv(df, file = "nationwideCP.csv")
}
tableToNationwideCSV(nationwideCPdf)

plotNationwideandLR <- function(){
  par(mfrow=c(1,1),mar=c(5.5,4,4,2)+0.1)
  x = nationwideCPdf$caseYear
  xmin = min(nationwideCPdf$caseYear)
  xmax = max(nationwideCPdf$caseYear)
  ymin1 = min(nationwideCPdf$AverageCPofTC) 
  ymax1 = max(nationwideCPdf$AverageCPofTC)+ 0.005
  y1 = nationwideCPdf$AverageCPofTC
  
  plot(x,y1, type = "o", 
       main = "Cases with Cell Phone Usage (%)",
       xlab = "Case Year", 
       ylab ="Cell Phone Cases (%)", xlim = c(xmin, xmax),
       ylim = c(ymin1, ymax1))
  abline(lm(y1~x),col="red")
  lines(lowess(x,y1),col="blue")
}
plotNationwideandLR()
