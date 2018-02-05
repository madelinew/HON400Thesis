#MADELINE M. WARNDORF HON400 THESIS 2018
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
        
        drop <- c("citynum","countynum","lat","long","day","month","date","drf1", "drf2", "drf3", "drf4")
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
        drop <- c("citynum","countynum","lat","long","day","month","date","dridistract1", "dridistract2")
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


tableToCSV(combinedMYdf)

fatal_07 <- findingCellCases(fatal_07, 07)
fatal_08 <- findingCellCases(fatal_08, 08)
fatal_09 <- findingCellCases(fatal_09, 09)
fatal_10 <- findingCellCases(fatal_10, 10)
fatal_11 <- findingCellCases(fatal_11, 11)
fatal_12 <- findingCellCases(fatal_12, 12)
fatal_13 <- findingCellCases(fatal_13, 13)
fatal_14 <- findingCellCases(fatal_14, 14)
fatal_15 <- findingCellCases(fatal_15, 15)
# FINDING NUM OF CASES and NUM OF DISTRACTED ------------------------------
#counting total number of cases in the calendar year by model year
#INPUT: df = fatal_YY 
#OUTPUT: data frame of number of total cases by model year
countOfCasesByMY <- function(df) {
  #df = fatal_YY
  dfs <- df
  modelyearList <- unique(dfs$modelyr)
  modelYear = numeric()
  totalCases = numeric()
  listcount <- 1
  for(i in modelyearList){
    modelYear[listcount] <- i
    addingToTable <- function(i){
      dfn <- dfs
      dfn$modelyrP <- ifelse(df$modelyr == i, 1, 0)
      dfn <- dfn[dfn$modelyrP != 0, ]
      dft <- as.data.frame(cbind.data.frame(dfn$casenum, dfn$statenum), 
                           header = TRUE, as.is = TRUE)
      colnames(dft) <- c("casenum", "statenum")
      dft <- unique(dft)
      dft <- makingUniqueCases(dft)
      casetotalsum <- 0
      x <- count(dft)
      casetotalsum <- sum(x$freq)
      return(casetotalsum)
    }
    totalCases[listcount] <- addingToTable(i)
    listcount = listcount + 1
  }
  dfmy <- data.frame(modelYear,totalCases, stringsAsFactors = FALSE)
  return(dfmy)
}

#Function for counting distracted driving cases
#INPUT: df = fatal_YY 
#OUTPUT: data frame of number of distracted driving cases involving cellphones by model year
countOfDisCasesByMY <- function(df) {
  #df = fatal_YY
  fataldisYY <- df[df$drf != 0, ]
  #dfn is the new dataframe
  dfn <- fataldisYY
  modelyearList <- unique(dfn$modelyr)
  modelYear = numeric()
  modelYearCellPhone = numeric()
  listcount <- 1
  for(i in modelyearList){
    modelYear[listcount] <- i
    addingToTable <- function(i){
      dfs <- dfn
      dfs$modelyrP <- ifelse(dfn$modelyr == i, 1, 0)
      dfs <- dfs[dfs$modelyrP != 0, ]
      dft <- as.data.frame(cbind.data.frame(dfs$casenum, dfs$statenum), 
                           header = TRUE, as.is = TRUE)
      colnames(dft) <- c("casenum", "statenum")
      dft <- unique(dft)
      dft <- makingUniqueCases(dft)
      casetotalsum <- 0
      x <- count(dft)
      casetotalsum <- sum(x$freq)
      return(casetotalsum)
    }
    modelYearCellPhone[listcount] <- addingToTable(i)
    listcount = listcount + 1
  }
  dfmy <- data.frame(modelYear,modelYearCellPhone, stringsAsFactors = FALSE)
  return(dfmy)
}

# NHTSA TECHNOLOGY --------------------------------------------------------

#Function for counting number of cases with fatalitites in the car
#INPUT: df = fatal_YY
#OUTPUT: number of cases with fatalities in the car by model year
countOfFatalInCarByMY <- function(df) {
  #df = fatal_YY
  #dfn is the new dataframe
  dfn <- df[df$numfatalveh != 0,]
  modelyearList <- unique(df$modelyr)
  modelYear = numeric()
  modelYearFatalVeh = numeric()
  listcount <- 1
  for(i in modelyearList){
    modelYear[listcount] <- i
    addingToTable <- function(i){
      dfs <- dfn
      dfs$modelyrP <- ifelse(dfn$modelyr == i, 1, 0)
      dfs <- dfs[dfs$modelyrP != 0, ]
      
      dft <- as.data.frame(cbind.data.frame(dfs$casenum, dfs$statenum), 
                           header = TRUE, as.is = TRUE)
      colnames(dft) <- c("casenum", "statenum")
      
      dft <- makingUniqueCases(dft)
      casetotalsum <- 0
      x <- count(dft)
      casetotalsum <- sum(x$freq)
      return(casetotalsum)
    }
    modelYearFatalVeh[listcount] <- addingToTable(i)
    listcount = listcount + 1
  }
  dfmy <- data.frame(modelYear,modelYearFatalVeh, stringsAsFactors = FALSE)
  return(dfmy)
}


#Function for creating data frame FatalCases (FC) o.f. TotalCases (TC) with Distracted Cases o.f. TC
#INPUT: none
#OUTPUT: data frame with modelyr, numFatalVeh, numCellPhone, numTotalCases
makingNHTSAdf <- function(){
  
  #fatalities in vehicle cases combined data frame
  combinedFatalVehdf <- function(){
    df07 <- countOfFatalInCarByMY(fatal_07)
    df08 <- countOfFatalInCarByMY(fatal_08)
    df09 <- countOfFatalInCarByMY(fatal_09)
    df10 <- countOfFatalInCarByMY(fatal_10)
    df11 <- countOfFatalInCarByMY(fatal_11)
    df12 <- countOfFatalInCarByMY(fatal_12)
    df13 <- countOfFatalInCarByMY(fatal_13)
    df14 <- countOfFatalInCarByMY(fatal_14)
    df15 <- countOfFatalInCarByMY(fatal_15)
    #combining into one df
    combineddf <- rbind(df07,df08, df09, df10, df11, df12, df13, 
                        df14, df15)
    return(combineddf)
  }
  numFatalVehdf <- combinedFatalVehdf()
  modelyear <- sort(unique(numFatalVehdf$modelYear))
  fatalVehTotal <- as.vector(tapply(numFatalVehdf$modelYearFatalVeh, numFatalVehdf$modelYear, sum))
  casedf <- data.frame(list(modelYear = modelyear, fatalVehTotal = fatalVehTotal), stringsAsFactors = FALSE)
  casedf$fatalVehTotal <- as.integer(casedf$fatalVehTotal)
  
  
  #cell phone cases combined data frame
  combinedCellPhonedf <- function(){
    df07 <- countOfDisCasesByMY(fatal_07, 07)
    df08 <- countOfDisCasesByMY(fatal_08, 08)
    df09 <- countOfDisCasesByMY(fatal_09, 09)
    df10 <- countOfDisCasesByMY(fatal_10, 10)
    df11 <- countOfDisCasesByMY(fatal_11, 11)
    df12 <- countOfDisCasesByMY(fatal_12, 12)
    df13 <- countOfDisCasesByMY(fatal_13, 13)
    df14 <- countOfDisCasesByMY(fatal_14, 14)
    df15 <- countOfDisCasesByMY(fatal_15, 15)
    #combining into one df
    combineddf <- rbind(df07,df08, df09, df10, df11, df12, df13, 
                        df14, df15)
    return(combineddf)
  }
  #adding cell phone data frame into main combined data frame
  numCellPhonedf <- combinedCellPhonedf()
  modelyear <- sort(unique(numCellPhonedf$modelYear))
  cellPhoneTotal <- as.vector(tapply(numCellPhonedf$modelYearCellPhone, numCellPhonedf$modelYear, sum))
  CPcasedf <- data.frame(list(modelYear = modelyear, cellPhoneTotal = cellPhoneTotal), stringsAsFactors = FALSE)
  casedf$cellPhoneTotal <- as.integer(CPcasedf$cellPhoneTotal)
  
  #total cases combined data frame
  combinedTotalCasesdf <- function(){
    df07 <- countOfCasesByMY(fatal_07)
    df08 <- countOfCasesByMY(fatal_08)
    df09 <- countOfCasesByMY(fatal_09)
    df10 <- countOfCasesByMY(fatal_10)
    df11 <- countOfCasesByMY(fatal_11)
    df12 <- countOfCasesByMY(fatal_12)
    df13 <- countOfCasesByMY(fatal_13)
    df14 <- countOfCasesByMY(fatal_14)
    df15 <- countOfCasesByMY(fatal_15)
    #combining into one df
    combineddf <- rbind(df07,df08, df09, df10, df11, df12, df13, 
                        df14, df15)
    return(combineddf)
  }
  #combining into main data frame
  numTotalCasesdf <- combinedTotalCasesdf()
  modelyear <- sort(unique(numTotalCasesdf$modelYear))
  TotalCasesMY <- as.vector(tapply(numTotalCasesdf$totalCases, numTotalCasesdf$modelYear, sum))
  TCcasedf <- data.frame(list(modelYear = modelyear, totalCases = TotalCasesMY), stringsAsFactors = FALSE)
  casedf$totalCases <- as.integer(TCcasedf$totalCases)
  
  #fatal in vehicle divided by total cases (%)
  FatalVehofTotalCases <- round(((casedf$fatalVehTotal/casedf$totalCases)*100),digits = 4)
  casedf$FatalVehofTotalCases <- FatalVehofTotalCases
  # cell phone divided by total cases (%)
  CellPhoneofTotalCases <- round(((casedf$cellPhoneTotal/casedf$totalCases)*100), digits = 4)
  casedf$CellPhoneofTotalCases <- CellPhoneofTotalCases
  
  techPhase <- c("ESC", "ESC", "ESC","ESC","EM","EM","EM","EM","EM")
  
  ndf <- as.data.frame(cbind(casedf$modelYear, casedf$fatalVehTotal,casedf$cellPhoneTotal,
                             casedf$totalCases,casedf$FatalVehofTotalCases,casedf$CellPhoneofTotalCases,
                             techPhase), header = TRUE, as.is = TRUE, stringsAsFactors = FALSE)
  colnames(ndf) <- c("modelYear", "fatalVehTotal", "cellPhoneTotal", "totalCases",
                     "FatalVehofTotalCases","CellPhoneofTotalCases","techPhase")
  ndf$modelYear <- casedf$modelYear 
  ndf$fatalVehTotal <- casedf$fatalVehTotal
  ndf$cellPhoneTotal <- casedf$cellPhoneTotal
  ndf$totalCases <- casedf$totalCases
  ndf$FatalVehofTotalCases <- casedf$FatalVehofTotalCases
  ndf$CellPhoneofTotalCases <- casedf$CellPhoneofTotalCases
  return(ndf)
}

combinedMYdf <- makingNHTSAdf()

#Table CSV combinedMYdf To Paste into .txt File to Paste into Word for Table
#Function for making and saving combinedMYdf Table 
#INPUT: df = combinedMYdf
#OUTPUT: saved .CSV file of table
tableToCSV <- function(df) {
  #df = combinedMYdf
  row.names(df)<- NULL
  colnames(df) <- c("modelYear", "fatalVehTotal", "cellPhoneTotal", "totalCases",
                    "FatalVehofTotalCases","CellPhoneofTotalCases","techPhase")
  write.csv(df, file = "combinedMY.csv")
}
tableToCSV(combinedMYdf)

#NHTSA PLOT MY
NHTSAplot <- function() {
  #df = combinedMYdf
  mydata <- combinedMYdf
  par(mfrow=c(2,1))
  x = mydata$modelYear
  xmin = min(mydata$modelYear)
  xmax = max(mydata$modelYear)
  
  y1 = mydata$CellPhoneofTotalCases
  ymin1 = min(mydata$CellPhoneofTotalCases) - 0.2
  ymax1 = max(mydata$CellPhoneofTotalCases) + 0.1
  
  ymin2 = min(mydata$FatalVehofTotalCases)-0.5
  ymax2 = max(mydata$FatalVehofTotalCases)+1
  y2 = mydata$FatalVehofTotalCases
  
  plot(x,y2, main = "Cases with Fatalities in Vehicle (%)", xlab = "Model Year", ylab ="Fatalities in Vehicle Cases(%)", xlim = c(xmin, xmax),
       ylim = c(ymin2, ymax2), col = "mediumblue")
  lines(x, y2, type = "h", col = "mediumblue")
  
  plot(x,y1, main = "Cases with Cell Phone Usage (%)", xlab = "Model Year", ylab ="Cell Phone Cases (%)", xlim = c(xmin, xmax),
       ylim = c(ymin1, ymax1), col = "darkviolet")
  lines(x, y1, type = "h", col = "darkviolet")
  
}
NHTSAplot()

#TECHNOLOGY T-TEST
technologyTtest <- function(df){
  #df = combinedMYdf
  before11 <- subset(df, modelYear < 2011, select = FatalVehofTotalCases)
  
  after11 <- subset(df, modelYear > 2011, select = FatalVehofTotalCases)
  print(sd(before11$FatalVehofTotalCases))
  print(sd(after11$FatalVehofTotalCases))
  ttest <- t.test(before11,after11)
  return(ttest)
}
technologyTtest(combinedMYdf)

#CELL PHONE T-TEST
cellphoneTtest <- function(df, MY){
  #df = combinedMYdf
  #MY = model year break. either 2010 or 2014
  if(MY == 2010){
    before10 <- subset(df, modelYear < 2010, select = CellPhoneofTotalCases)
    
    after10 <- subset(df, modelYear > 2010, select = CellPhoneofTotalCases)
    print(sd(before10$CellPhoneofTotalCases))
    print(sd(after10$CellPhoneofTotalCases))
    ttest <- t.test(before10,after10)
  } else {
    before14 <- subset(df, modelYear < 2014, select = CellPhoneofTotalCases)
    
    after14 <- subset(df, modelYear > 2014, select = CellPhoneofTotalCases)
    print(sd(before14$CellPhoneofTotalCases))
    print(sd(after14$CellPhoneofTotalCases))
    ttest <- t.test(before14,after14)
  }
  return(ttest)
}
cellphoneTtest(combinedMYdf,2010)
cellphoneTtest(combinedMYdf,2014)


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

#Count of Total Cases for each Make by Model Year
#INPUT: df = fatal_YY makenum = Make number
#OUTPUT: data frame with count of cases by model year
countOfCasesMakeByMY <- function(df, makenum) {
  #df = fatal_YY
  #makenum = make number
  modelyearList <- unique(df$modelyr)
  mn <- makenum
  dfs <- df
  dfs$makenump <- ifelse(dfs$make == mn, 1, 0)
  dfs <- dfs[dfs$makenump != 0, ]
  modelYear = numeric()
  totalCases = numeric()
  listcount <- 1
  for(i in modelyearList){
    modelYear[listcount] <- i
    addingToTable <- function(i){
      dfn <- dfs
      dfn$modelyrP <- ifelse(dfn$modelyr == i, 1, 0)
      dfn <- dfn[dfn$modelyrP != 0, ]
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
  dfmy <- data.frame(modelYear,totalCases, stringsAsFactors = FALSE)
  return(dfmy)
}

#Count of Cases with CP for Each Make by Model Year
#INPUT: df = fatal_YY makenum = Make number
#OUTPUT: data frame with count of distracted driving cases by model year
countOfDisCasesMakeByMY <- function(df, makenum) {
  #df = fatal_YY
  
  fataldisYY <- df[df$drf != 0, ]
  #dfn is the new dataframe
  dfn <- fataldisYY
  modelyearList <- unique(df$modelyr)
  mn <- makenum
  dfn$makenump <- ifelse(dfn$make == mn, 1, 0)
  dfn <- dfn[dfn$makenump != 0, ]
  modelYear = numeric()
  modelYearCellPhone = numeric()
  listcount <- 1
  for(i in modelyearList){
    modelYear[listcount] <- i
    addingToTable <- function(i){
      dfs <- dfn
      dfs$modelyrP <- ifelse(dfn$modelyr == i, 1, 0)
      dfs <- dfs[dfs$modelyrP != 0, ]
      dft <- as.data.frame(cbind.data.frame(dfs$casenum, dfs$statenum), 
                           header = TRUE, as.is = TRUE)
      colnames(dft) <- c("casenum", "statenum")
      dft <- unique(dft)
      
      casetotalsum <- 0
      x <- count(dft)
      casetotalsum <- sum(x$freq)
      return(casetotalsum)
    }
    modelYearCellPhone[listcount] <- addingToTable(i)
    listcount = listcount + 1
  }
  dfmy <- data.frame(modelYear,modelYearCellPhone, stringsAsFactors = FALSE)
  return(dfmy)
}

#Counting Fatalities in Vehicle cases by Make and MY
#INPUT: df = fatal_YY makenum = Make number
#OUTPUT: data frame with count of cases with fatalities in vehicles by model year 
countingFatalMakeInCarsByMY <- function(df, makenum) {
  #df = fatal_YY
  mn <- makenum
  modelyearList <- unique(df$modelyr)
  df$makenump <- ifelse(df$make == mn, 1, 0)
  df <- df[df$makenump != 0, ]
  dfn <- df[df$numfatalveh != 0,]
  
  modelYear = numeric()
  modelYearFatalVeh = numeric()
  listcount <- 1
  for(i in modelyearList){
    modelYear[listcount] <- i
    addingToTable <- function(i){
      dfs <- dfn
      dfs$modelyrP <- ifelse(dfn$modelyr == i, 1, 0)
      dfs <- dfs[dfs$modelyrP != 0, ]
      
      dft <- as.data.frame(cbind.data.frame(dfs$casenum, dfs$statenum), 
                           header = TRUE, as.is = TRUE)
      colnames(dft) <- c("casenum", "statenum")
      
      dft <- unique(dft)
      casetotalsum <- 0
      x <- count(dft)
      casetotalsum <- sum(x$freq)
      return(casetotalsum)
    }
    modelYearFatalVeh[listcount] <- addingToTable(i)
    listcount = listcount + 1
  }
  dfmy <- data.frame(modelYear,modelYearFatalVeh, stringsAsFactors = FALSE)
  return(dfmy)
}

#Dataframe of Make
#INPUT:makenum = Make number
#OUTPUT: combined data frame of counts of cases by model number
makingMakedf <- function(makenum){
  mn <- makenum
  #fatalities in vehicle cases
  combinedFatalVehdf <- function(){
    df07 <- countingFatalMakeInCarsByMY(fatal_07, mn)
    df08 <- countingFatalMakeInCarsByMY(fatal_08, mn)
    df09 <- countingFatalMakeInCarsByMY(fatal_09, mn)
    df10 <- countingFatalMakeInCarsByMY(fatal_10, mn)
    df11 <- countingFatalMakeInCarsByMY(fatal_11, mn)
    df12 <- countingFatalMakeInCarsByMY(fatal_12, mn)
    df13 <- countingFatalMakeInCarsByMY(fatal_13, mn)
    df14 <- countingFatalMakeInCarsByMY(fatal_14, mn)
    df15 <- countingFatalMakeInCarsByMY(fatal_15, mn)
    #combining into one df
    combineddf <- rbind(df07,df08, df09, df10, df11, df12, df13, 
                        df14, df15)
    return(combineddf)
  }
  numFatalVehdf <- combinedFatalVehdf()
  modelyear <- sort(unique(numFatalVehdf$modelYear))
  fatalVehTotal <- as.vector(tapply(numFatalVehdf$modelYearFatalVeh, numFatalVehdf$modelYear, sum))
  casedf <- data.frame(list(modelYear = modelyear, fatalVehTotal = fatalVehTotal), stringsAsFactors = FALSE)
  casedf$fatalVehTotal <- as.integer(casedf$fatalVehTotal)
  
  
  
  #cell phone cases
  combinedCellPhonedf <- function(){
    df07 <- countOfDisCasesMakeByMY(fatal_07, mn)
    df08 <- countOfDisCasesMakeByMY(fatal_08, mn)
    df09 <- countOfDisCasesMakeByMY(fatal_09, mn)
    df10 <- countOfDisCasesMakeByMY(fatal_10, mn)
    df11 <- countOfDisCasesMakeByMY(fatal_11, mn)
    df12 <- countOfDisCasesMakeByMY(fatal_12, mn)
    df13 <- countOfDisCasesMakeByMY(fatal_13, mn)
    df14 <- countOfDisCasesMakeByMY(fatal_14, mn)
    df15 <- countOfDisCasesMakeByMY(fatal_15, mn)
    #combining into one df
    combineddf <- rbind(df07,df08, df09, df10, df11, df12, df13, 
                        df14, df15)
    return(combineddf)
  }
  numCellPhonedf <- combinedCellPhonedf()
  modelyear <- sort(unique(numCellPhonedf$modelYear))
  cellPhoneTotal <- as.vector(tapply(numCellPhonedf$modelYearCellPhone, numCellPhonedf$modelYear, sum))
  CPcasedf <- data.frame(list(modelYear = modelyear, cellPhoneTotal = cellPhoneTotal), stringsAsFactors = FALSE)
  casedf$cellPhoneTotal <- as.integer(CPcasedf$cellPhoneTotal)
  
  #total cases 
  combinedTotalCasesdf <- function(){
    df07 <- countOfCasesMakeByMY(fatal_07, mn)
    df08 <- countOfCasesMakeByMY(fatal_08, mn)
    df09 <- countOfCasesMakeByMY(fatal_09, mn)
    df10 <- countOfCasesMakeByMY(fatal_10, mn)
    df11 <- countOfCasesMakeByMY(fatal_11, mn)
    df12 <- countOfCasesMakeByMY(fatal_12, mn)
    df13 <- countOfCasesMakeByMY(fatal_13, mn)
    df14 <- countOfCasesMakeByMY(fatal_14, mn)
    df15 <- countOfCasesMakeByMY(fatal_15, mn)
    #combining into one df
    combineddf <- rbind(df07,df08, df09, df10, df11, df12, df13, 
                        df14, df15)
    return(combineddf)
  }
  numTotalCasesdf <- combinedTotalCasesdf()
  modelyear <- sort(unique(numTotalCasesdf$modelYear))
  TotalCasesMY <- as.vector(tapply(numTotalCasesdf$totalCases, numTotalCasesdf$modelYear, sum))
  TCcasedf <- data.frame(list(modelYear = modelyear, totalCases = TotalCasesMY), stringsAsFactors = FALSE)
  casedf$totalCases <- as.integer(TCcasedf$totalCases)
  
  #fatal in vehicle divided by total cases (%)
  FatalVehofTotalCases <- round(((casedf$fatalVehTotal/casedf$totalCases)*100),digits = 4)
  casedf$FatalVehofTotalCases <- FatalVehofTotalCases
  # cell phone divided by total cases (%)
  CellPhoneofTotalCases <- round(((casedf$cellPhoneTotal/casedf$totalCases)*100), digits = 4)
  casedf$CellPhoneofTotalCases <- CellPhoneofTotalCases
  
  
  ndf <- as.data.frame(cbind(casedf$modelYear, casedf$fatalVehTotal,casedf$cellPhoneTotal,
                             casedf$totalCases,casedf$FatalVehofTotalCases,casedf$CellPhoneofTotalCases), 
                       header = TRUE, as.is = TRUE, stringsAsFactors = FALSE)
  colnames(ndf) <- c("modelYear", "fatalVehTotal", "cellPhoneTotal", "totalCases",
                     "FatalVehofTotalCases","CellPhoneofTotalCases")
  ndf$modelYear <- casedf$modelYear 
  ndf$fatalVehTotal <- casedf$fatalVehTotal
  ndf$cellPhoneTotal <- casedf$cellPhoneTotal
  ndf$totalCases <- casedf$totalCases
  ndf$FatalVehofTotalCases <- casedf$FatalVehofTotalCases
  ndf$CellPhoneofTotalCases <- casedf$CellPhoneofTotalCases
  return(ndf)
}

#for Ford (FARS Code 12)
fordMakedf <- makingMakedf(12)

#for Cadillac (FARS Code 19)
cadillacMakedf <- makingMakedf(19)
cadillacMakedf[is.na(cadillacMakedf)] <- 0

#for Mercedes-Benz (FARS Code 42)
mercedesMakedf <- makingMakedf(42)

#for Volvo (FARS Code 51)
volvoMakedf <- makingMakedf(51)
volvoMakedf[is.na(volvoMakedf)] <- 0

#Table CSV makeCPdf To Paste into .txt File to Paste into Word for Table
#Function for making and saving makedisdf Table 
#INPUT: df = makeCPdf
#OUTPUT: saved .CSV file of table
tableToMakeCSV <- function(df) {
  #df = makeMakedf
  row.names(df)<- NULL
  colnames(df) <- c("modelYear", "fatalVehTotal", "cellPhoneTotal", "totalCases",
                    "FatalVehofTotalCases","CellPhoneofTotalCases")
  write.csv(df, file = "makevolvo.csv")
}
tableToMakeCSV(volvoMakedf)


#FORD
Fordplot <- function() {
  mydata <- fordMakedf
  par(mfrow=c(2,1))
  x = mydata$modelYear
  xmin = min(mydata$modelYear)
  xmax = max(mydata$modelYear)
  
  y1 = mydata$CellPhoneofTotalCases
  ymin1 = min(mydata$CellPhoneofTotalCases) 
  ymax1 = max(mydata$CellPhoneofTotalCases) + 0.2
  
  ymin2 = min(mydata$FatalVehofTotalCases)-1
  ymax2 = max(mydata$FatalVehofTotalCases)+1.5
  y2 = mydata$FatalVehofTotalCases
  
  plot(x,y2, main = "Ford Cases with Fatalities in Vehicle (%)", xlab = "Model Year", ylab ="Fatalities in Vehicle Cases(%)", xlim = c(xmin, xmax),
       ylim = c(ymin2, ymax2), col = "mediumblue")
  lines(x, y2, type = "h", col = "mediumblue")
  
  plot(x,y1, main = "Ford Cases with Cell Phone Usage (%)", xlab = "Model Year", ylab ="Cell Phone Cases (%)", xlim = c(xmin, xmax),
       ylim = c(ymin1, ymax1), col = "darkviolet")
  lines(x, y1, type = "h", col = "darkviolet")
  
}
Fordplot()

#TECHNOLOGY T-TEST
FordFatalVehTtest <- function(df){
  #df = fordMakedf
  before11 <- subset(df, modelYear < 2011, select = FatalVehofTotalCases)
  
  after11 <- subset(df, modelYear > 2011, select = FatalVehofTotalCases)
  print(sd(before11$FatalVehofTotalCases))
  print(sd(after11$FatalVehofTotalCases))
  ttest <- t.test(before11,after11)
  return(ttest)
}
FordFatalVehTtest(fordMakedf)

#FORD CELL PHONE T-TEST
FordCellPhoneTtest <- function(df, MY){
  #df = fordMakedf
  #MY = model year break. either 2010 or 2013
  if(MY == 2010){
    before10 <- subset(df, modelYear < 2010, select = CellPhoneofTotalCases)
    
    after10 <- subset(df, modelYear > 2010, select = CellPhoneofTotalCases)
    print(sd(before10$CellPhoneofTotalCases))
    print(sd(after10$CellPhoneofTotalCases))
    ttest <- t.test(before10,after10)
  } else {
    before13 <- subset(df, modelYear < 2013, select = CellPhoneofTotalCases)
    after13 <- subset(df, modelYear > 2013, select = CellPhoneofTotalCases)
    print(sd(before13$CellPhoneofTotalCases))
    print(sd(after13$CellPhoneofTotalCases))
    ttest <- t.test(before13,after13)
    
  }
  return(ttest)
}
FordCellPhoneTtest(fordMakedf, 2010)
FordCellPhoneTtest(fordMakedf, 2013)

#CADILLAC
Cadillacplot <- function() {
  mydata <- cadillacMakedf
  par(mfrow=c(2,1))
  x = mydata$modelYear
  xmin = min(mydata$modelYear)
  xmax = max(mydata$modelYear)
  
  y1 = mydata$CellPhoneofTotalCases
  ymin1 = min(mydata$CellPhoneofTotalCases) - 0.1
  ymax1 = max(mydata$CellPhoneofTotalCases) + 0.2
  
  ymin2 = min(mydata$FatalVehofTotalCases)-0.1
  ymax2 = max(mydata$FatalVehofTotalCases)+1
  y2 = mydata$FatalVehofTotalCases
  
  plot(x,y2, main = "Cadillac Cases with Fatalities in Vehicle (%)", xlab = "Model Year", ylab ="Fatalities in Vehicle Cases(%)", xlim = c(xmin, xmax),
       ylim = c(ymin2, ymax2), col = "mediumblue")
  lines(x, y2, type = "h", col = "mediumblue")
  
  plot(x,y1, main = "Cadillac Cases with Cell Phone Usage (%)", xlab = "Model Year", ylab ="Cell Phone Cases (%)", xlim = c(xmin, xmax),
       ylim = c(ymin1, ymax1), col = "darkviolet")
  lines(x, y1, type = "h", col = "darkviolet")
  
}
Cadillacplot()

#TECHNOLOGY T-TEST
CadillacFatalVehTtest <- function(df, MY){
  #df = cadillacMakedf
  if(MY == 2011){
    before11 <- subset(df, modelYear < 2011, select = FatalVehofTotalCases)
    after11 <- subset(df, modelYear > 2011, select = FatalVehofTotalCases)
    print(sd(before11$FatalVehofTotalCases))
    print(sd(after11$FatalVehofTotalCases))
    ttest <- t.test(before11,after11)
  } else {
    if(MY == 2012) {
      before12 <- subset(df, modelYear < 2012, select = FatalVehofTotalCases)
      after12 <- subset(df, modelYear > 2012, select = FatalVehofTotalCases)
      print(sd(before12$FatalVehofTotalCases))
      print(sd(after12$FatalVehofTotalCases))
      ttest <- t.test(before12,after12)
    } else {
      before13 <- subset(df, modelYear < 2013, select = FatalVehofTotalCases)
      after13 <- subset(df, modelYear > 2013, select = FatalVehofTotalCases)
      print(sd(before13$FatalVehofTotalCases))
      print(sd(after13$FatalVehofTotalCases))
      ttest <- t.test(before13,after13)
    }
  }
  return(ttest)
}
CadillacFatalVehTtest(cadillacMakedf, 2011)
CadillacFatalVehTtest(cadillacMakedf, 2012)
CadillacFatalVehTtest(cadillacMakedf, 2013)

#CADILLAC CELL PHONE T-TEST
CadillacCellPhoneTtest <- function(df, MY){
  #df = cadillacMakedf
  #MY = model year, either 2011 or 2013
  if(MY == 2011){
    before11 <- subset(df, modelYear < 2011, select = CellPhoneofTotalCases)
    after11 <- subset(df, modelYear > 2011, select = CellPhoneofTotalCases)
    print(sd(before11$CellPhoneofTotalCases))
    print(sd(after11$CellPhoneofTotalCases))
    ttest <- t.test(before11,after11)
  } else {
    before13 <- subset(df, modelYear < 2013, select = CellPhoneofTotalCases)
    after13 <- subset(df, modelYear > 2013, select = CellPhoneofTotalCases)
    print(sd(before13$CellPhoneofTotalCases))
    print(sd(after13$CellPhoneofTotalCases))
    ttest <- t.test(before13,after13)
  }
  
  return(ttest)
}
CadillacCellPhoneTtest(cadillacMakedf, 2011)
CadillacCellPhoneTtest(cadillacMakedf, 2013)


#MERCEDES
Mercedesplot <- function() {
  mydata <- mercedesMakedf
  par(mfrow=c(2,1))
  x = mydata$modelYear
  xmin = min(mydata$modelYear)
  xmax = max(mydata$modelYear)
  
  y1 = mydata$CellPhoneofTotalCases
  ymin1 = min(mydata$CellPhoneofTotalCases) - 0.1
  ymax1 = max(mydata$CellPhoneofTotalCases) + 0.2
  
  ymin2 = min(mydata$FatalVehofTotalCases)-0.1
  ymax2 = max(mydata$FatalVehofTotalCases)+1
  y2 = mydata$FatalVehofTotalCases
  
  plot(x,y2, main = "Mercedes Cases with Fatalities in Vehicle (%)", xlab = "Model Year", ylab ="Fatalities in Vehicle Cases(%)", xlim = c(xmin, xmax),
       ylim = c(ymin2, ymax2), col = "mediumblue")
  lines(x, y2, type = "h", col = "mediumblue")
  
  plot(x,y1, main = "Mercedes Cases with Cell Phone Usage (%)", xlab = "Model Year", ylab ="Cell Phone Cases (%)", xlim = c(xmin, xmax),
       ylim = c(ymin1, ymax1), col = "darkviolet")
  lines(x, y1, type = "h", col = "darkviolet")
  
}
Mercedesplot()

#TECHNOLOGY T-TEST
MercedesFatalVehTtest <- function(df, MY){
  #df = mercedesMakedf
  #MY = model year, either 2010 or 2013
  if(MY == 2010){
    before10 <- subset(df, modelYear < 2010, select = FatalVehofTotalCases)
    after10 <- subset(df, modelYear > 2010, select = FatalVehofTotalCases)
    print(sd(before10$FatalVehofTotalCases))
    print(sd(after10$FatalVehofTotalCases))
    ttest <- t.test(before10,after10)
  } else {
    before13 <- subset(df, modelYear < 2013, select = FatalVehofTotalCases)
    
    after13 <- subset(df, modelYear > 2013, select = FatalVehofTotalCases)
    print(sd(before13$FatalVehofTotalCases))
    print(sd(after13$FatalVehofTotalCases))
    ttest <- t.test(before13,after13)
  }
  
  return(ttest)
}
MercedesFatalVehTtest(mercedesMakedf, 2010)
MercedesFatalVehTtest(mercedesMakedf, 2013)

#MERCEDES CELL PHONE T-TEST
MercedesCellPhoneTtest <- function(df, MY){
  #df = mercedesMakedf
  if(MY == 2010){
    before10 <- subset(df, modelYear < 2010, select = CellPhoneofTotalCases)
    after10 <- subset(df, modelYear > 2010, select = CellPhoneofTotalCases)
    print(sd(before10$CellPhoneofTotalCases))
    print(sd(after10$CellPhoneofTotalCases))
    ttest <- t.test(before10,after10)
  } else {
    before14 <- subset(df, modelYear < 2014, select = CellPhoneofTotalCases)
    after14 <- subset(df, modelYear > 2014, select = CellPhoneofTotalCases)
    print(sd(before14$CellPhoneofTotalCases))
    print(sd(after14$CellPhoneofTotalCases))
    ttest <- t.test(before14,after14)
  }
  
  return(ttest)
}
MercedesCellPhoneTtest(mercedesMakedf, 2010)
MercedesCellPhoneTtest(mercedesMakedf, 2014)

#VOLVO
Volvoplot <- function() {
  mydata <- volvoMakedf
  par(mfrow=c(2,1))
  x = mydata$modelYear
  xmin = min(mydata$modelYear)
  xmax = max(mydata$modelYear)
  
  y1 = mydata$CellPhoneofTotalCases
  ymin1 = min(mydata$CellPhoneofTotalCases) - 0.1
  ymax1 = max(mydata$CellPhoneofTotalCases) + 0.2
  
  ymin2 = min(mydata$FatalVehofTotalCases)-0.1
  ymax2 = max(mydata$FatalVehofTotalCases)+1
  y2 = mydata$FatalVehofTotalCases
  
  plot(x,y2, main = "Volvo Cases with Fatalities in Vehicle (%)", xlab = "Model Year", ylab ="Fatalities in Vehicle Cases(%)", xlim = c(xmin, xmax),
       ylim = c(ymin2, ymax2), col = "mediumblue")
  lines(x, y2, type = "h", col = "mediumblue")
  
  plot(x,y1, main = "Volvo Cases with Cell Phone Usage (%)", xlab = "Model Year", ylab ="Cell Phone Cases (%)", xlim = c(xmin, xmax),
       ylim = c(ymin1, ymax1), col = "darkviolet")
  lines(x, y1, type = "h", col = "darkviolet")
  
}
Volvoplot()

#TECHNOLOGY T-TEST
VolvoFatalVehTtest <- function(df, MY){
  #df = volvoMakedf
  if(MY == 2010) {
    before10 <- subset(df, modelYear < 2010, select = FatalVehofTotalCases)
    after10 <- subset(df, modelYear > 2010, select = FatalVehofTotalCases)
    print(sd(before10$FatalVehofTotalCases))
    print(sd(after10$FatalVehofTotalCases))
    ttest <- t.test(before10,after10)
  } else {
    before12 <- subset(df, modelYear < 2012, select = FatalVehofTotalCases)
    after12 <- subset(df, modelYear > 2012, select = FatalVehofTotalCases)
    print(sd(before12$FatalVehofTotalCases))
    print(sd(after12$FatalVehofTotalCases))
    ttest <- t.test(before12,after12)
  }
  
  return(ttest)
}
VolvoFatalVehTtest(volvoMakedf, 2010)
VolvoFatalVehTtest(volvoMakedf, 2012)