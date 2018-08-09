cat("\014")
getwd()
setwd("G:/My Drive/Thesis/BGs") # FINLAB/Personal PC
setwd("D:/Thesis/BGs") # LAB4 PC

# PACKAGES

pkgs = c("data.tabler", "tidyverse", "zoo") # package names
install.packages(pkgs)

inst = lapply(pkgs, library, character.only = TRUE) # load them

# install.packages("data.table")
# install.packages("dplyr")
# install.packages("zoo")

library(data.table)
library(dplyr)
library(zoo)

# rm(list = ls(pattern = "^tmp"))
# rm(list = ls())
# IPO  <-  read.csv(file = "Input/Fund IPO 1.csv", header = TRUE)


#INPUTTING OWNERSHIP DATA----------------------------------------------------------------------------------------------------------------------------------


OWD <- read.table("Input/Ownership data/23128_1_7_20180403_010717_dat.txt", header = TRUE, sep = "|",quote="\"")
ISIND <- read.table("Input/Ownership data/23128_1_5_20180403_010717_dat.txt", header = TRUE, sep = "|",quote="\"")

colnames(OWD) <- c("co_code","company_name","date","report","owner_code", "owner_name")

OWD <- merge(OWD, ISIND, by = c("co_code","company_name"), all = TRUE)
setnames(OWD, "isin_code_equity", "isin")
OWD <- data.table(OWD)[order(OWD$co_code, OWD$date), ][!is.na(OWD$isin),]
# OWD1 <- OWD[order(OWD$owner_gp_name),][,{x=1;grp_count = sum(x)}, by = owner_gp_name ]
OWD <- OWD[, date := 1*OWD$date][, `:=` (ym = floor(date/100)) ] #day = date%%100
OWD <- OWD[!duplicated(OWD[,c("co_code", "ym", "owner_code")]), ]

year <- 2000:2018
month <- 1:12
ym <- merge(year,month)
ym <- data.table(ym)[,.(ym = x*100 + y)][ym<201801 & ym>=200401,]
compym <- merge(OWD[!duplicated(OWD$co_code), co_code], ym , by = NULL)
setnames(compym, "x", "co_code")
compym <- compym[order(compym$co_code, compym$ym),]
rm(year, month, ym)


OWD <- OWD[ym<201801 & ym>=200401,]
OWD1 <- merge(OWD, compym, by = c("co_code", "ym"), all = TRUE)
# test <- OWD[!duplicated(OWD[,c("co_code", "ym", "owner_code")]), ]
# test1 <- test[duplicated(test[,c("co_code", "ym")]), ]
# rm(test,test1)
OWD1 <- OWD1[, c('report','date') := NULL]
OWD1 <- OWD1[order(OWD1$co_code, OWD1$ym),]

OWD2 <- OWD1 %>%  group_by(co_code) %>%  mutate(owner_name = na.locf(owner_name, na.rm = F),
                                                owner_code = na.locf(owner_code, na.rm = F),
                                                company_name = na.locf(company_name, na.rm = F),
                                                isin = na.locf(isin, na.rm = F))

OWD2 <- OWD2 %>%  group_by(co_code) %>%  mutate(owner_name = na.locf(owner_name, na.rm = F, fromLast = T),
                                                owner_code = na.locf(owner_code, na.rm = F, fromLast = T),
                                                company_name = na.locf(company_name, na.rm = F, fromLast = T),
                                                isin = na.locf(isin, na.rm = F, fromLast = T))

# test <- OWD
# test <- test[!duplicated(test[,c("co_code", "company_name","owner_code" )]),]
# test1 <- test[duplicated(test[,c("co_code", "company_name")]), ]
# str(test)
# rm(test,test1)

rm(ISIND, compym, OWD, OWD1)
# save.image("Rdata/OWD2.RData")
# load("Rdata/OWD2.RData")


# Shortlisting of funds which I will be analysing further---------------------------------------------------------------------------------------------------

rm(list = ls())

list <- read.csv(file = "Input/list/list.csv", header = TRUE)
list1 <- data.table(list)[Asset.Universe == "Mutual Funds"][Geographical.Focus == "India"][Domicile == "India"][is.na(Closed.End.Fund)] # c1 = 25073; c2 = 25073; c3 = 25073; c4 = 6922
list1 <- list1[is.na(Institutional.Fund)][is.na(Index.Tracking)][is.na(Fund.of.Funds.Internal)][is.na(Fund.of.Funds.External)] #c8 = 6081
list1 <- list1[Asset.Type != "Bond"][Asset.Type != "Commodity"][Asset.Type != "Money Market"][is.na(Insurance.Fund)] #c10 = 3508 c11 = 2390 c12 = 2352
list1 <- list1[, c("Asset.Universe", "Geographical.Focus", "Domicile", "Closed", "Closed.End.Fund", "Exchange.Traded.Fund", "Institutional.Fund", 
                   "Index.Tracking", "Insurance.Fund", "Private.Fund", "REITS", "Legal.Structure.Name", "Pre.IPO","Fund.of.Funds.Internal",
                   "Fund.of.Funds.External","Fund.of.Funds", "Countries.Notified.for.Sale", "Fund.Currency", "Name.1", "Asset.Status", "Exchange.Listed",
                   "Primary.Flag", "ISIN.Currency.Class","Co.Administrator.Name", "Co.Advisor.Name") := NULL ]
list1 <- list1[order(Primary.Code,Portfolio.Code,Lipper.ID),]
list1 <- list1[!duplicated(list1[,c("Primary.Code","Portfolio.Code")]),] # there are 575 unique portfolios but only 566 primary fund
list2 <- list1[, c("Lipper.ID","Name","Closed.Date","Qualified.History.Date","Schemes.India.AMFI.Category.Wise.Class", "Portfolio.Code", "Primary.Code")]

write.csv(list2, file = "Output/Fund list.csv")
rm(list,list1)
# save.image("Rdata/List.RData")
# load("Rdata/List.RData") # Contains files - list2

# Importing portfolio data of Selected Funds---------------------------------------------------------------------------------------------------------------

rm(list = ls())
file.names <- list.files(path = "Input/Fund Portfolio data", full.names = TRUE)
# list(file.names)
# length(file.names)
ldf <- lapply(file.names, read.csv)

if(length(file.names) >= 2) {
  for(i in 1:(length(file.names)-1)){ if (i == 1) { port <- rbind(ldf[[i]],ldf[[i+1]]) } else {port <- rbind(port,ldf[[i+1]])}}
} else { print ("Error") }
rm(file.names,i,ldf)

port$Date = as.Date(port$Date,"%d-%m-%Y")
port <- port[!is.na(port$Date),]
port$ym = year(port$Date) *100 + month(port$Date)

# save.image("Rdata/Portfolio.RData")
load("Rdata/Portfolio.RData")
# port = contains portfolio data of all the funds


#Importing IPO and SEo data-------------------------------------------------------------------------------------------------------------------------------

rm(list = ls())

cap <- read.table("Input/Capital issues/26824_1_155_20180727_020658_dat.txt", header = TRUE, sep = "|",quote="\"")
cap <- data.table(cap)[issue_date>=20040101]
# cap1 <- cap[!is.na(isin_code)]
cap1 <- cap[!grepl("non-convertible debentures", cap$sectype_name, ignore.case = TRUE)]
cap1 <- cap1[!grepl("Commercial paper", cap1$sectype_name, ignore.case = TRUE)]
cap1 <- cap1[!grepl("Debentures / Bonds / notes / bills", cap1$sectype_name, ignore.case = TRUE)]
cap1 <- cap1[!grepl("Secured debentures/ bonds/notes/bills", cap1$sectype_name, ignore.case = TRUE)]


issue_type <- cap1[!duplicated(cap1[,c("sectype_name","issue_type")]) ,c("sectype_name","issue_type")]
write.csv(issue_type, file = "Output/issue_type.csv")
