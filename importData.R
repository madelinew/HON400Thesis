#IMPORTING AND SAVING DATA
#2007
library(readxl)
X2007txtToExcel <- read_excel("~/Desktop/Thesis/RProject/Hon400Code/FARS Data/2007txtToExcel.xlsx",
                              col_types = c("numeric", "numeric", "numeric",
                                            "numeric", "numeric", "text",
                                            "text", "date", "numeric", "numeric",
                                            "numeric", "numeric", "text", "text",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric"))
FARS_07 <- as.data.frame(X2007txtToExcel, header = TRUE,as.is=TRUE, stringAsFactors = FALSE)
rm(X2007txtToExcel)

#2008
X2008txtToExcel <- read_excel("~/Desktop/Thesis/RProject/Hon400Code/FARS Data/2008txtToExcel.xlsx",
                              col_types = c("numeric", "numeric", "numeric",
                                            "numeric", "numeric", "text",
                                            "text", "date", "numeric", "numeric",
                                            "numeric", "numeric", "text", "text",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric"))
FARS_08 <- as.data.frame(X2008txtToExcel, header = TRUE,as.is=TRUE)
rm(X2008txtToExcel)

#2009
X2009txtToExcel <- read_excel("~/Desktop/Thesis/RProject/Hon400Code/FARS Data/2009txtToExcel.xlsx",
                              col_types = c("numeric", "numeric", "numeric",
                                            "numeric", "numeric", "text",
                                            "text", "date", "numeric", "numeric",
                                            "numeric", "numeric", "text", "text",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric"))
FARS_09 <- as.data.frame(X2009txtToExcel, header = TRUE,as.is=TRUE)
rm(X2009txtToExcel)

#2010
X2010txtToExcel <- read_excel("~/Desktop/Thesis/RProject/Hon400Code/FARS Data/2010txtToExcel.xlsx", 
                                col_types = c("numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "text", "text", 
                                              "date", "numeric", "numeric", "numeric", 
                                              "numeric", "text", "text", 
                                              "numeric", "numeric", "numeric",
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric",
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric",
                                              "numeric", "numeric", "numeric",
                                              "numeric", "numeric", "numeric"))
FARS_10 <- as.data.frame(X2010txtToExcel, header = TRUE, as.is=TRUE)
rm(X2010txtToExcel)

#2011
X2011txtToExcel <- read_excel("~/Desktop/Thesis/RProject/Hon400Code/FARS Data/2011txtToExcel.xlsx",
                              col_types = c("numeric", "numeric", "numeric",
                                            "numeric", "numeric", "text", "text",
                                            "date", "numeric", "numeric", "numeric",
                                            "numeric", "text", "text",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric"))
FARS_11 <- as.data.frame(X2011txtToExcel, header = TRUE, as.is=TRUE)
rm(X2011txtToExcel)

#2012
X2012txtToExcel <- read_excel("~/Desktop/Thesis/RProject/Hon400Code/FARS Data/2012txtToExcel.xlsx",
                              col_types = c("numeric", "numeric", "numeric",
                                            "numeric", "numeric", "text", "text",
                                            "date", "numeric", "numeric", "numeric",
                                            "numeric", "text", "text",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric"))

FARS_12 <- as.data.frame(X2012txtToExcel, header = TRUE, as.is=TRUE)
rm(X2012txtToExcel)

#2013
X2013txtToExcel <- read_excel("~/Desktop/Thesis/RProject/Hon400Code/FARS Data/2013txtToExcel.xlsx",
                              col_types = c("numeric", "numeric", "numeric",
                                            "numeric", "numeric", "text", "text",
                                            "date", "numeric", "numeric", "numeric",
                                            "numeric", "text", "text",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric"))

FARS_13 <- as.data.frame(X2013txtToExcel, header = TRUE, as.is=TRUE)
rm(X2013txtToExcel)

#2014
X2014txtToExcel <- read_excel("~/Desktop/Thesis/RProject/Hon400Code/FARS Data/2014txtToExcel.xlsx",
                              col_types = c("numeric", "numeric", "numeric",
                                            "numeric", "numeric", "text", "text",
                                            "date", "numeric", "numeric", "numeric",
                                            "numeric", "text", "text",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric"))
FARS_14 <- as.data.frame(X2014txtToExcel, header = TRUE, as.is=TRUE)
rm(X2014txtToExcel)

#2015
X2015txtToExcel <- read_excel("~/Desktop/Thesis/RProject/Hon400Code/FARS Data/2015txtToExcel.xlsx",
                              col_types = c("numeric", "numeric", "numeric",
                                            "numeric", "numeric", "text",
                                            "text", "date", "numeric", "numeric",
                                            "numeric", "numeric", "text",
                                            "text", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric"))
FARS_15 <- as.data.frame(X2015txtToExcel, header = TRUE, as.is=TRUE)
rm(X2015txtToExcel)

