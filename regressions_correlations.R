# Computing 
# 1. Equity Returns & Volatility
# 2. FX Returns & Volatility
# 3. Regressions of Equity & FX
# 4. Regressions of VIX & Momentum
# 5. Correlations of Equity & FX

# Template to install any package
# install.packages('magrittr', dependencies=TRUE, repos='http://cran.rstudio.com/')

# Import required libraries after installation using above command template
suppressWarnings(library(xts))
suppressWarnings(library(quantmod))
suppressWarnings(library(plyr))
suppressWarnings(library(dplyr))
suppressWarnings(library(magrittr))

# Import FX Data files downloaded from HistData
# Saranya's Mac 
# setwd("/Users/SaranyaGanapa/Dropbox/vishu-sharu/R/HistData/1minute-2010-15/")
# Saranya's PC
setwd("C:\\Users\\sharu\\Dropbox\\vishu-sharu\\R\\HistData\\1minute-2010-15\\")
getwd()

# Use same start and end years for FX and equity
# Note : Runtime 5minutes / year
startYear <- 2012
endYear <- 2015
version <- 4.3

# ===========================
# ===========================
# ===========================
# =========EQUITY============
# ===========================
# ===========================

# Helper function to fetch equity data
# Fetch Equity data from Yahoo Finance using quantmod library
getDataForSymbols <- function(tickers, dataSource) {
  for (i in 1:length(tickers)) {
    cat(tickers[i],i,"\n")
    
    # getSymbols will save the fetched data in a variable automatically.
    getSymbols(tickers[i],
               src = dataSource,
               auto.assign = getOption("getSymbols.auto.assign",TRUE),
               env = parent.frame())
  }
}

# Equity : 21 days Rolling returns & Standard deviation
getEquityReturnsAndStdDev <- function() {
  # convert to data frame
  equity <<- data.frame(date=index(equity), coredata(equity))
  colnames(equity) <- equityColumnNames
  
  equityTemp <- equity %>%
    mutate(Year = as.numeric(substr(as.character(Date), 1, 4))) %>%
    mutate(Month = as.numeric(substr(as.character(Date), 6, 7))) %>%
    mutate(Day = as.numeric(substr(as.character(Date), 9, 10)))
  equity <- equityTemp
  rownames(equity) <- NULL
  
  # Required data only from startYear-endYear
  equityTemp <- equity %>%
    filter(Year>=startYear, Year<=endYear) %>%
    select(Date, Open, High, Low, Close, Volume, Year, Month, Day) %>%
    mutate(Date = as.numeric(paste(substr(as.character(Date), 1, 4),
                                   substr(as.character(Date), 6, 7),
                                   substr(as.character(Date), 9, 10)
                                   ,sep="")))
  equity <- equityTemp
  
  # Creating empty Vectors - will fill in values later
  equityReturns <- data.frame()
  equityDailyReturns <- data.frame()
  equityDailyStandardDev <- data.frame()
  
  # Equity Return Calculations
  # Start from 22nd day
  for(i in 22:length(unique(equity$Date))) {
    equityReturns[i, 'Equity_Returns'] <- as.numeric((equity$Open[i] - equity$Open[i-21]) / equity$Open[i-21]);
    equityReturns[i, 'Date'] <- as.numeric(equity$Date[i]);
  }
  
  tail(equityReturns)
  # Equity Standard Deviation-Volatility Calculations
  # Step 1 : Daily returns for equity
  for(i in 2:length(unique(equity$Date))) {
    equityDailyReturns[i, 'Equity_Daily_Returns'] <- as.numeric((equity$Open[i] - equity$Open[i-1]) / equity$Open[i-1]);
    equityDailyReturns[i, 'Date'] <- as.numeric(equity$Date[i]);
  }
  
  # Step 2- Rolling 21 Day Standard Deviation for equity
  for(i in 22:length(unique(equityDailyReturns[, 'Date']))) {
    startIndex <- i-21
    equityDailyStandardDev[i, 'Equity_Standard_Deviation'] <- sd(equityDailyReturns[startIndex:i, 'Equity_Daily_Returns'])
    equityDailyStandardDev[i, 'Date'] <- as.numeric(equity$Date[i]);
  }
  # Create returns and standard deviation data frame
  equityDF <- merge(equityReturns, equityDailyStandardDev, by="Date")
  
  print("Returning equityDF from func")
  print(head(equityDF))

  return(equityDF)
}

# Fetch Equity data by Symbols
equitySource <- c("yahoo")
equityIndices <- c("^GSPC", "^GDAXI", "^SSMI", "^N225", "^GSPTSE", "^AORD", "^NZ50", "^OMX", "^FTSE", "^MXX")
equityColumnNames <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
suppressWarnings(getDataForSymbols(equityIndices, equitySource))

# Compute returns
equity <- GSPC
GSPC_RV <- getEquityReturnsAndStdDev()
equity <- GDAXI
GDAXI_RV <- getEquityReturnsAndStdDev()
equity <- SSMI
SSMI_RV <- getEquityReturnsAndStdDev()
equity <- N225
N225_RV <- getEquityReturnsAndStdDev()
equity <- GSPTSE
GSPTSE_RV <- getEquityReturnsAndStdDev()
equity <- AORD
AORD_RV <- getEquityReturnsAndStdDev()
equity <- NZ50
NZ50_RV <- getEquityReturnsAndStdDev()
equity <- OMX
OMX_RV <- getEquityReturnsAndStdDev()
equity <- FTSE
FTSE_RV <- getEquityReturnsAndStdDev()
equity <- MXX
MXX_RV <- getEquityReturnsAndStdDev()

# ===========================
# ===========================
# ===========================
# ============FX=============
# ===========================
# ===========================

# snapshot of the csv file : 20070101 200000;1.323700;1.323700;1.323600;1.323700;0  (Date, O,H,L,C,V)
# FX Data Helpers
# Read data from 4 years and merge into one data frame
# apppending 2013 to 2012 data and so on.....using rbind
getCurrencyMergedData <- function(currencyPair) {
  mergedCurrencyPair <- data.frame()
  for(i in startYear:endYear) {
    dynamicYearCurrencyPair <- read.table(paste(currencyPair, "/HISTDATA_COM_ASCII_", currencyPair, "_M1", i, "/DAT_ASCII_", currencyPair, "_M1_", i, ".csv", sep=""), sep=";")
    mergedCurrencyPair <- rbind(mergedCurrencyPair, dynamicYearCurrencyPair)
  }
  
  return(mergedCurrencyPair)
}

formatDateTime <- function(currencyPairData, flipCurrency) {
  colnames(currencyPairData) <- c("Date", "Open", "High", "Low", "Close", "Volume");
  
  # Format Date column into Time, Year, Month, Day
  # Example Date column initial value : 20120102 020000
  # Make a copy of the global variable into temp local variable to edit the data frames
  currencyPairTemp  <- currencyPairData %>%
    select(Date, Open, High, Low, Close) %>%
    mutate(Time = as.numeric(ldply(strsplit(as.character(Date), " "))[, 2])) %>%
    mutate(Date = as.numeric(ldply(strsplit(as.character(Date), " "))[, 1])) %>%
    mutate(Year = as.numeric(substr(as.character(Date), 1, 4))) %>%
    mutate(Month = as.numeric(substr(as.character(Date), 5, 6))) %>%
    mutate(Day = as.numeric(substr(as.character(Date), 7, 8)))
  
  # flip the currencies into USD terms if its not already in USD.
  if(flipCurrency == TRUE) {
    currencyPairTemp <- currencyPairTemp %>%
     mutate(Open = 1/Open) %>%
     mutate(High = 1/High) %>%
     mutate(Low = 1/Low) %>%
     mutate(Close = 1/Close)
  }
  
  currencyPairData <- currencyPairTemp
  print("currencyPairData")
  print(head(currencyPairData))
  print("currencyPairTemp")
  print(head(currencyPairTemp))
  return(currencyPairData)
}


getFXReturnsAndStdDev <- function(currencyPair, flipCurrency) {
  # get csv's merged from startyear till endyear
  currencyPairData <- getCurrencyMergedData(currencyPair)
  # format the datetime string to separate columns
  currencyPairData <- formatDateTime(currencyPairData, flipCurrency)
  
  print('FX data fetched, merged & formatted')
  
  # PART 1 : Compute FX Returns
  # Group by Date and compute (11am - 10am / 10am)
  # To handle discrepancy between count of 10oClock values & 11oClock values
  # Take 10oClocks separately, divide and update fxReturns values
  # Group by Date and subtract 10oClock from 11oClock
  fxReturns <- currencyPairData %>%
    select(everything()) %>%
    filter(Time >= 100000 & Time <= 110000) %>%
    group_by(Date) %>%
    summarise(fxReturns = (sum(Close[which(Time == 110000)], -Close[which(Time == 100000)])))
  
  # Get only 10oClock rows
  fxReturnsTen <- currencyPairData %>%
    select(everything()) %>%
    filter(Time == 100000)
  
  # merge by Date to ignore the dates where either 11/10oClock are missing
  mergedFxReturns <- merge(fxReturns, fxReturnsTen, by="Date")
  # To the Grouped by Date values divide by 10oClock to get fxReturns
  fxReturns <- mergedFxReturns %>%
    mutate(Date = Date, fxReturns = fxReturns / Close) %>%
    select(Date, fxReturns)

  colnames(fxReturns) <- c("Date", "FX_Returns")

  tail(fxReturns)
  
  # PART 2 : Compute FX Standard Deviation
  # STEP 1 : Compute 5 minute rolling returns for time between 10am - 11:30am
  fx5MinuteRollingReturns <- c(1)
  fx5MinuteRollingReturnsDate <- c(1)
  
  currencyPair10_1130 <- currencyPairData %>% filter(Time >= 095600 & Time <= 113000)
  head(currencyPair10_1130)
  print(paste("Computing 5 minute rolling returns for FXSD...Row Count : ", length(currencyPair10_1130$Time)), sep="")
  currencyPair10_1130_Close <- currencyPair10_1130$Close
  currencyPair10_1130_Date <- currencyPair10_1130$Date
  currencyPair10_1130_Time <- currencyPair10_1130$Time
  for(i in 1:length(currencyPair10_1130$Time)) {
    if(currencyPair10_1130_Time[i] < 100000) {
      # Store zeros from (9:56am - 9:59am)
      fx5MinuteRollingReturns[i] <- 0
      fx5MinuteRollingReturnsDate[i] <- 0
    } else {
      # NOTE : In certain scenarios 9:57 / 58 / 59 might be missing in the data, so check and proceed
      # Example snapshot from USDMXN
      #    Date       Open       High        Low      Close   Time Year Month Day
      #  1 20120102 0.07181200 0.07181200   0.07181200  95600 2012     1   2
      #  2 20120102 0.07182205 0.07181380 0.07182231 0.07182231  95700 2012     1   2
      #  3 20120102 0.07182334 0.07182334 0.07182334 0.07182334  95800 2012     1   2
      #  4 20120102 0.07182257 0.07181457 0.07182334 0.07181457 100000 2012     1   2
      #  5 20120102 0.07181329 0.07181303 0.07181561 0.07181406 100100 2012     1   2
      if(!identical(currencyPair10_1130_Close[i-4], numeric(0))) {
        fiveMinuteRollingClose <- currencyPair10_1130_Close[i-4]
        fx5MinuteRollingReturns[i] <- (currencyPair10_1130_Close[i] - fiveMinuteRollingClose) / fiveMinuteRollingClose
        fx5MinuteRollingReturnsDate[i] <- currencyPair10_1130_Date[i]
      }
    }
  }

  fx5MinuteRollingReturnsDF <- data.frame(fx5MinuteRollingReturnsDate)
  fx5MinuteRollingReturnsDF$Five_Minute_Returns <- fx5MinuteRollingReturns
  colnames(fx5MinuteRollingReturnsDF) <- c("Date", "Five_Minute_Returns")
  head(fx5MinuteRollingReturnsDF)

  # STEP 2 : Standard deviation from 5 minute rolling returns
  # Group by Date and compute standard deviation from 5 minute returns between 10am - 11:30am
  fxStandardDeviation <- data.frame()
  fxDFReturnsStandardDeviation <- data.frame()
  
  fxStandardDeviation<- fx5MinuteRollingReturnsDF %>%
    group_by(Date) %>%
    summarise(FX_Standard_Deviation = sd(as.numeric(Five_Minute_Returns)))

  # Merge FX Returns and FX Standard deviation data frames
  fxDFReturnsStandardDeviation <- merge(fxReturns, fxStandardDeviation, by="Date")
  print("Tail : FX 5 minute rolling return + SD")
  print(tail(fxDFReturnsStandardDeviation))
  
  return(fxDFReturnsStandardDeviation)
}


# Get FX Returns data for 10 currencies
EURUSD_RV <- getFXReturnsAndStdDev("EURUSD", FALSE)
USDCHF_RV <- getFXReturnsAndStdDev("USDCHF", TRUE)
USDJPY_RV <- getFXReturnsAndStdDev("USDJPY", TRUE)
USDCAD_RV <- getFXReturnsAndStdDev("USDCAD", TRUE)
AUDUSD_RV <- getFXReturnsAndStdDev("AUDUSD", FALSE)
NZDUSD_RV <- getFXReturnsAndStdDev("NZDUSD", FALSE)
USDSEK_RV <- getFXReturnsAndStdDev("USDSEK", TRUE)
USDNOK_RV <- getFXReturnsAndStdDev("USDNOK", TRUE)
GBPUSD_RV <- getFXReturnsAndStdDev("GBPUSD", FALSE)
USDMXN_RV <- getFXReturnsAndStdDev("USDMXN", TRUE)

# Global list of Equity & FX RVs
equityRVs <- list(GSPC_RV, GDAXI_RV, SSMI_RV, N225_RV, GSPTSE_RV, AORD_RV, NZ50_RV, OMX_RV, FTSE_RV, MXX_RV)
fxRVs <- list(EURUSD_RV, USDCHF_RV, USDJPY_RV, USDCAD_RV, AUDUSD_RV, NZDUSD_RV, USDSEK_RV, USDNOK_RV, GBPUSD_RV, USDMXN_RV)
fxNameList <- list("EURUSD", "USDCHF", "USDJPY", "USDCAD", "AUDUSD", "NZDUSD", "USDSEK", "USDNOK", "GBPUSD", "USDMXN")

getwd()
# Check Returns and Volatility for a single Equity and FX
# print('Check Returns % SD for Equity & FX for one pair')
# tail(EURUSD_RV)
# head(GSPC_RV)

# ===========================
# ===========================
# ===========================
# =========REGRESSION========
# ===========================
# ===========================
# ===========================

#Regression Helpers
mergeAndFormatRVs <- function(equityRV, fxRV) {
  fxEquityReturnsVolatility <- merge(fxRV, equityRV, by="Date")
  
  # Add YTD
  fxEquityReturnsVolatility$Year <- as.numeric(substr(as.character(fxEquityReturnsVolatility$Date), 1, 4));
  fxEquityReturnsVolatility$Month <- as.numeric(substr(as.character(fxEquityReturnsVolatility$Date), 5, 6));
  fxEquityReturnsVolatility$Day <- as.numeric(substr( (fxEquityReturnsVolatility$Date), 7, 8));
  
  return(fxEquityReturnsVolatility)
}

# Regressions of FX Returns & Equity Returns
# do.call : converts list to vector
# as.list : converts numeric, string, etc into list type
# lm accepts only vectors.
fillFXR_EQR_Regression <- function(fxEquityReturnsVolatility, equityIndex, fx) {
  fxReturns <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Returns)))
  eqReturns <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Returns)))
  reg_fxr_eqr <- lm(fxReturns~eqReturns, data=fxEquityReturnsVolatility)
  reg_fxr_eqr_summary <- summary(reg_fxr_eqr)
  print(reg_fxr_eqr_summary)
  
  alpha <- reg_fxr_eqr_summary$coefficients[1]
  alpha_t_stat <- reg_fxr_eqr_summary$coefficients[1, 3]
  beta <- reg_fxr_eqr_summary$coefficients[2]
  beta_t_stat <- reg_fxr_eqr_summary$coefficients[2, 3]
  adjusted_r_squared <- reg_fxr_eqr_summary$adj.r.squared
  
  FXR_EQR_DF[equityIndex, fx] <<- alpha
  FXR_EQR_DF[equityIndex+1, fx] <<- alpha_t_stat
  FXR_EQR_DF[equityIndex+2, fx] <<- beta
  FXR_EQR_DF[equityIndex+3, fx] <<- beta_t_stat
  FXR_EQR_DF[equityIndex+4, fx] <<- adjusted_r_squared
  
  return(FXR_EQR_DF)
}

# Regressions of FX Returns & Equity Volatility (SD) 
fillFXR_EQV_Regression <- function(fxEquityReturnsVolatility, equityIndex, fx) {
  fxReturns <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Returns)))
  eqVolatility <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Standard_Deviation)))
  reg_fxr_eqv <- lm(fxReturns~eqVolatility, data=fxEquityReturnsVolatility)
  reg_fxr_eqv_summary <- summary(reg_fxr_eqv)
  # print(reg_fxr_eqv_summary)
  
  alpha <- reg_fxr_eqv_summary$coefficients[1]
  alpha_t_stat <- reg_fxr_eqv_summary$coefficients[1, 3]
  beta <- reg_fxr_eqv_summary$coefficients[2]
  beta_t_stat <- reg_fxr_eqv_summary$coefficients[2, 3]
  adjusted_r_squared <- reg_fxr_eqv_summary$adj.r.squared
  
  FXR_EQV_DF[equityIndex, fx] <- alpha
  FXR_EQV_DF[equityIndex+1, fx] <- alpha_t_stat
  FXR_EQV_DF[equityIndex+2, fx] <- beta
  FXR_EQV_DF[equityIndex+3, fx] <- beta_t_stat
  FXR_EQV_DF[equityIndex+4, fx] <- adjusted_r_squared
  
  return(FXR_EQV_DF)
}

# Regressions of FX Volatility & Equity Returns (SD) 
fillFXV_EQR_Regression <- function(fxEquityReturnsVolatility, equityIndex, fx) {
  fxVolatility <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Standard_Deviation)))
  eqReturns <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Returns)))
  reg_fxv_eqr <- lm(fxVolatility~eqReturns, data=fxEquityReturnsVolatility)
  reg_fxv_eqr_summary <- summary(reg_fxv_eqr)
  # print(reg_fxv_eqr_summary)
  
  alpha <- reg_fxv_eqr_summary$coefficients[1]
  alpha_t_stat <- reg_fxv_eqr_summary$coefficients[1, 3]
  beta <- reg_fxv_eqr_summary$coefficients[2]
  beta_t_stat <- reg_fxv_eqr_summary$coefficients[2, 3]
  adjusted_r_squared <- reg_fxv_eqr_summary$adj.r.squared
  
  FXV_EQR_DF[equityIndex, fx] <- alpha
  FXV_EQR_DF[equityIndex+1, fx] <- alpha_t_stat
  FXV_EQR_DF[equityIndex+2, fx] <- beta
  FXV_EQR_DF[equityIndex+3, fx] <- beta_t_stat
  FXV_EQR_DF[equityIndex+4, fx] <- adjusted_r_squared
  
  return(FXV_EQR_DF)
}

# Regressions of FX Volatility & Equity Volatility (SD) 
fillFXV_EQV_Regression <- function(fxEquityReturnsVolatility, equityIndex, fx) {
  fxVolatility <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Standard_Deviation)))
  eqVolatility <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Standard_Deviation)))
  reg_fxv_eqv <- lm(fxVolatility~eqVolatility, data=fxEquityReturnsVolatility)
  reg_fxv_eqv_summary <- summary(reg_fxv_eqv)
  # print(reg_fxv_eqv_summary)
  
  alpha <- reg_fxv_eqv_summary$coefficients[1]
  alpha_t_stat <- reg_fxv_eqv_summary$coefficients[1, 3]
  beta <- reg_fxv_eqv_summary$coefficients[2]
  beta_t_stat <- reg_fxv_eqv_summary$coefficients[2, 3]
  adjusted_r_squared <- reg_fxv_eqv_summary$adj.r.squared
  
  FXV_EQV_DF[equityIndex, fx] <<- alpha
  FXV_EQV_DF[equityIndex+1, fx] <<- alpha_t_stat
  FXV_EQV_DF[equityIndex+2, fx] <<- beta
  FXV_EQV_DF[equityIndex+3, fx] <<- beta_t_stat
  FXV_EQV_DF[equityIndex+4, fx] <<- adjusted_r_squared
  
  return(FXV_EQV_DF)
}


getCoEffs <- function(equityRV, fxRV, equityIndex, fx) {
  # Merge RV data frames by Date
  fxEquityReturnsVolatility <<- mergeAndFormatRVs(equityRV, fxRV)
  
  # Regressions of FX Volatility & Equity Returns (SD)
  FXV_EQR_DF <<- fillFXV_EQR_Regression(fxEquityReturnsVolatility, equityIndex, paste(fx, "Volatility", sep="_"))
  
  # Regressions of FX Volatility & Equity Volatility (SD) 
  FXV_EQV_DF <<- fillFXV_EQV_Regression(fxEquityReturnsVolatility, equityIndex, paste(fx, "Volatility", sep="_"))
  
  # Regressions of FX Returns & Equity Returns
  FXR_EQR_DF <<- fillFXR_EQR_Regression(fxEquityReturnsVolatility, equityIndex, paste(fx, "Returns", sep="_"))
  
  # Regressions of FX Returns & Equity Volatility (SD) 
  FXR_EQV_DF <<- fillFXR_EQV_Regression(fxEquityReturnsVolatility, equityIndex, paste(fx, "Returns", sep="_"))
}

# To view the regression summary of one single pair, uncomment this section and run it.
# fxEquityReturnsVolatility <<- mergeAndFormatRVs(GSPC_RV, EURUSD_RV)
# EURUSDReturns <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Returns)))
# GSPCReturns <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Returns)))
# reg_fxr_eqr <- lm(EURUSDReturns~GSPCReturns, data=fxEquityReturnsVolatility)
# reg_fxr_eqr_summary <- summary(reg_fxr_eqr)
# print(reg_fxr_eqr_summary)

# Build empty coefficient+p-values tables
# ===========================
# ======== Snapshot =========
# ===========================
#              EURUSD  USDCHF  ...
# GSPC  Alpha   ....    ....
#       t-stat  ....    ....
#       beta    ....    ....
#       tstat   ....    ....
# GDAXI 
# ...   ...     ...
# ===========================

prepareRegressionTable <- function(colSuffix, rowSuffix) {
  # Symbols & Currencies vectors for column and row names
  EquitySymbols <- c(paste('GSPC', rowSuffix, sep="_"),'','','','',paste('GDAXI', rowSuffix, sep="_"),'','','','', paste('SSMI', rowSuffix, sep="_"),
                     '','','','', paste('N225', rowSuffix, sep="_"),'','','','', paste('GSPTSE', rowSuffix, sep="_"),'','','','', paste('AORD', rowSuffix, sep="_"),
                     '','','','', paste('NZ50', rowSuffix, sep="_"),'','','','', paste('OMX', rowSuffix, sep="_"),'','','','', paste('FTSE', rowSuffix, sep="_"),
                     '','','','', paste('MXX', rowSuffix, sep="_"),'','','','')
  FxCurrencies <- c('Equities', 'Variables', paste('EURUSD', colSuffix, sep="_"), paste('USDCHF', colSuffix, sep="_"), paste('USDJPY', colSuffix, sep="_"),
                    paste('USDCAD', colSuffix, sep="_"), paste('AUDUSD', colSuffix, sep="_"), paste('NZDUSD', colSuffix, sep="_"), paste('USDSEK', colSuffix, sep="_"),
                    paste('USDNOK', colSuffix, sep="_"), paste('GBPUSD', colSuffix, sep="_"), paste('USDMXN', colSuffix, sep="_"))
  
  # 5 Regression variables for each equity
  RegressionVariables <- c('Alpha', 't-stat', 'Beta', 't-stat', 'R^2')
  # 5 regression variables * 10 equities
  regressionTable <- data.frame(matrix(0, ncol = 12, nrow = 10*5))
  colnames(regressionTable) <- FxCurrencies
  # Assign equity names in first column (1 for every 5 rows)
  regressionTable[,1] <- EquitySymbols
  # Loop through second column to fill with 5 regression varaibles for every equity
  for(i in 1:nrow(regressionTable)) {
    if(i%%5 == 1) {
      regressionTable[i:(i+4), 2] <- RegressionVariables
    }
  }
  
  return(regressionTable)
}

# X axis -> FX Volatility
# Y axis -> EQUITY Returns
FXV_EQR_DF <- prepareRegressionTable("Volatility", "Returns")

# X axis -> FX Volatility
# Y axis -> EQUITY Volatility
FXV_EQV_DF <- prepareRegressionTable("Volatility", "Volatility")

# X axis -> FX Returns
# Y axis -> EQUITY Returns
FXR_EQR_DF <- prepareRegressionTable("Returns", "Returns")

# X axis -> FX Returns
# Y axis -> EQUITY Volatility
FXR_EQV_DF <- prepareRegressionTable("Returns", "Volatility")

for(i in 1:length(equityRVs)) {
  for(j in 1:length(fxRVs)) {
    getCoEffs(equityRVs[[i]], fxRVs[[j]], (1 + ((i - 1) * 5)), fxNameList[j])    
  }
}

print("FXV_EQR_DF Regression table")
FXV_EQR_DF
print("FXV_EQV_DF Regression table")
FXV_EQV_DF
print("FXR_EQR_DF Regression table")
FXR_EQR_DF
print("FXR_EQV_DF Regression table")
FXR_EQV_DF

# ===========================
# ===========================
# ===========================
# =======VIX & MOMENTUM======
# ===========================
# ===========================

# Fetch Equity data by Symbols
vixSource <- c("yahoo")
vixIndex <- c("^VIX")
vixColumnNames <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
suppressWarnings(getDataForSymbols(vixIndex, vixSource))

# Convert to data frame and assign column names
VIX <<- data.frame(date=index(VIX), coredata(VIX))
colnames(VIX) <- vixColumnNames
VIX <- VIX %>%
  select(Date, Close)

# Add Year, Month & Date columns 
vixTemp <- VIX %>%
    mutate(Year = as.numeric(substr(as.character(Date), 1, 4))) %>%
    mutate(Date = as.character(Date))

VIX <- vixTemp
rownames(VIX) <- NULL

# Remove "-" in Date string
vixTemp <- VIX %>%
    filter(Year>=startYear, Year<=endYear) %>%
    select(Date, Close) %>%
    mutate(Date = as.numeric(paste(substr(as.character(Date), 1, 4),
                                   substr(as.character(Date), 6, 7),
                                   substr(as.character(Date), 9, 10)
                                   ,sep="")))
VIX <- vixTemp

print("VIX data snapshot")
head(VIX)

# switch directory to read momentum CSV
getwd()
# MAC
# setwd("../Momentum")
# PC
setwd("..\\Momentum")


momentum <- data.frame()
momentum <- read.table("F-F_Momentum_Factor_daily.csv", sep=",")
colnames(momentum) <- c("Date", "Momentum")
#Delete last two rows in Kenneth table : Has author name in CSV
momentum <- momentum[1:(nrow(momentum)-2), ]
# Filter data from start year and end year
momentum <- momentum %>%
  mutate(Year = as.numeric(substr(as.character(Date), 1, 4))) %>%
  filter(Year >= startYear & Year <= endYear) %>%
  mutate(Date = as.numeric(as.character(Date))) %>%   # Convert factor to numeric datatype "sharu" style
  mutate(MOM_Factor = Momentum / 100) %>%
  select(Date, MOM_Factor)
  
print("Momentum data snapshot")
head(momentum)

# Merge VIX, Momentum
# For large tables in R dplyr's function inner_join() is much faster than merge()

VIX_MOM <- inner_join(VIX, momentum, by="Date") %>%
  mutate(VIX = Close) %>%
  select(Date, VIX, MOM_Factor)

head(VIX_MOM)

# ===========================================
# ===========================================
# ===========================================
# =======DIVIDEND YIELDS & TERM SPREAD======
# ===========================================
# ===========================================

# Fetch Equity data by Symbols
vymSource <- c("yahoo")
vymIndex <- c("VYM")
vymColumnNames <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
suppressWarnings(getDataForSymbols(vymIndex, vymSource))

# Convert to data frame and assign column names
VYM <<- data.frame(date=index(VYM), coredata(VYM))
colnames(VYM) <- vymColumnNames
VYM <- VYM %>%
  select(Date, Close)

# Add Year, Month & Date columns 
vymTemp <- VYM %>%
  mutate(Year = as.numeric(substr(as.character(Date), 1, 4))) %>%
  mutate(Date = as.character(Date))

VYM <- vymTemp
rownames(VYM) <- NULL

# Remove "-" in Date string
vymTemp <- VYM %>%
  filter(Year>=startYear, Year<=endYear) %>%
  select(Date, Close) %>%
  mutate(Date = as.numeric(paste(substr(as.character(Date), 1, 4),
                                 substr(as.character(Date), 6, 7),
                                 substr(as.character(Date), 9, 10)
                                 ,sep="")))
VYM <- vymTemp

print("VYM data snapshot")
head(VYM)

# switch directory to read momentum CSV
getwd()
# MAC
# setwd("../Momentum")
# PC
setwd("..\\TermSpreads")

TermSpread <- data.frame()
TermSpread <- read.table("T10Y3M.csv", sep=",")
#Delete 1st row : Has titles in CSV
TermSpread <- TermSpread[2:nrow(TermSpread), ]
colnames(TermSpread) <- c("Date", "TermSpread")



# Filter data from start year and end year
TermSpread <- TermSpread %>%
  mutate(Year = as.numeric(substr(as.character(Date), 1, 4))) %>%
  filter(Year >= startYear & Year <= endYear) %>%
  mutate(Date = as.numeric(paste(substr(as.character(Date), 1, 4),   # Remove "-" in Date string
                                 substr(as.character(Date), 6, 7),
                                 substr(as.character(Date), 9, 10)
                                 ,sep=""))) %>%
  select(Date, TermSpread)

VYM <- vymTemp

print("TermSpread data snapshot")
head(TermSpread)

# Defining function for adding VYM and Termspread
VYM_TS <- inner_join(VYM, TermSpread, by="Date") %>%
  mutate(VYM = Close) %>%
  select(Date, VYM, TermSpread)

# ===============================================
# ===============================================
# ========== REGRESSIONS OF =====================
# ===VIX MOM DIVIDEND YIELDS & TERM SPREAD======
# ===============================================
# ===============================================

prepareFactorRegressionTable <- function(colSuffix) {
  # Symbols & Currencies vectors for column and row names
  FactorSymbols <- c('VIX', '', '', '', '', 'MOM', '', '', '', '', 'Dividend Yield', '', '', '', '', 'Term Spread', '', '', '', '')
  FxCurrencies <- c('Factors', 'Variables', paste('EURUSD', colSuffix, sep="_"), paste('USDCHF', colSuffix, sep="_"), paste('USDJPY', colSuffix, sep="_"),
                    paste('USDCAD', colSuffix, sep="_"), paste('AUDUSD', colSuffix, sep="_"), paste('NZDUSD', colSuffix, sep="_"), paste('USDSEK', colSuffix, sep="_"),
                    paste('USDNOK', colSuffix, sep="_"), paste('GBPUSD', colSuffix, sep="_"), paste('USDMXN', colSuffix, sep="_"))
  
  
  # 5 Regression variables for each factor
  RegressionVariables <- c('Alpha', 't-stat', 'Beta', 't-stat', 'R^2')
  # 5 regression variables * 4 factors
  regressionTable <- data.frame(matrix(0, ncol = 12, nrow = 4*5))
  colnames(regressionTable) <- FxCurrencies
  # Assign factor names in first column (1 for every 5 rows)
  regressionTable[,1] <- FactorSymbols
  # Loop through second column to fill with 5 regression varaibles for every equity
  for(i in 1:nrow(regressionTable)) {
    if(i%%5 == 1) {
      regressionTable[i:(i+4),2] <- RegressionVariables
    }
  }
  
  return(regressionTable)
}


# X axis -> FX Returns
# Y axis -> VIX Factor, MOM, VYM, TS
VIX_MOM_VYM_TS_FXR_DF <- prepareFactorRegressionTable("Returns")

# X axis -> FX Volatility
# Y axis -> VIX Factor, MOM, VYM, TS
VIX_MOM_VYM_TS_FXV_DF <- prepareFactorRegressionTable("Volatility")

print("Prepared empty factor regression table layout snapshot")
VIX_MOM_VYM_TS_FXR_DF


# Regressions of VIX & FX Returns
fillVIX_MOM_FXR_Regression <- function(factorsMergedData, fxColName) {
  fxReturns <- c(do.call("cbind", as.list(factorsMergedData$FX_Returns)))
  vixFactor <- c(do.call("cbind", as.list(factorsMergedData$VIX)))
  momFactor <- c(do.call("cbind", as.list(factorsMergedData$MOM_Factor)))
  vym <- c(do.call("cbind", as.list(factorsMergedData$VYM)))
  termSpread <- c(do.call("cbind", as.list(factorsMergedData$TermSpread)))
  
  # VIX - FXR
  factorIndex <- 1
  reg_vix_fxr <- lm(fxReturns~vixFactor, data=factorsMergedData)
  reg_vix_fxr_summary <- summary(reg_vix_fxr)
  
  alpha <- reg_vix_fxr_summary$coefficients[1]
  alpha_t_stat <- reg_vix_fxr_summary$coefficients[1,3]
  beta <- reg_vix_fxr_summary$coefficients[2]
  beta_t_stat <- reg_vix_fxr_summary$coefficients[2,3]
  adjusted_r_squared <- reg_vix_fxr_summary$adj.r.squared
  
  VIX_MOM_VYM_TS_FXR_DF[factorIndex, fxColName] <- alpha
  VIX_MOM_VYM_TS_FXR_DF[factorIndex+1, fxColName] <- alpha_t_stat
  VIX_MOM_VYM_TS_FXR_DF[factorIndex+2, fxColName] <- beta
  VIX_MOM_VYM_TS_FXR_DF[factorIndex+3, fxColName] <- beta_t_stat
  VIX_MOM_VYM_TS_FXR_DF[factorIndex+4, fxColName] <- adjusted_r_squared
  
  # MOM - FXR
  factorIndex <- 6
  reg_mom_fxr <- lm(fxReturns~momFactor, data=factorsMergedData)
  reg_mom_fxr_summary <- summary(reg_mom_fxr)
  
  alpha <- reg_mom_fxr_summary$coefficients[1]
  alpha_t_stat <- reg_mom_fxr_summary$coefficients[1,3]
  beta <- reg_mom_fxr_summary$coefficients[2]
  beta_t_stat <- reg_mom_fxr_summary$coefficients[2,3]
  adjusted_r_squared <- reg_mom_fxr_summary$adj.r.squared
  
  VIX_MOM_VYM_TS_FXR_DF[factorIndex, fxColName] <- alpha
  VIX_MOM_VYM_TS_FXR_DF[factorIndex+1, fxColName] <- alpha_t_stat
  VIX_MOM_VYM_TS_FXR_DF[factorIndex+2, fxColName] <- beta
  VIX_MOM_VYM_TS_FXR_DF[factorIndex+3, fxColName] <- beta_t_stat
  VIX_MOM_VYM_TS_FXR_DF[factorIndex+4, fxColName] <- adjusted_r_squared
  
  # VYM - FXR
  factorIndex <- 11
  reg_vym_fxr <- lm(fxReturns~vym, data=factorsMergedData)
  reg_vym_fxr_summary <- summary(reg_vym_fxr)
  
  alpha <- reg_vym_fxr_summary$coefficients[1]
  alpha_t_stat <- reg_vym_fxr_summary$coefficients[1,3]
  beta <- reg_vym_fxr_summary$coefficients[2]
  beta_t_stat <- reg_vym_fxr_summary$coefficients[2,3]
  adjusted_r_squared <- reg_vym_fxr_summary$adj.r.squared
  
  VIX_MOM_VYM_TS_FXR_DF[factorIndex, fxColName] <- alpha
  VIX_MOM_VYM_TS_FXR_DF[factorIndex+1, fxColName] <- alpha_t_stat
  VIX_MOM_VYM_TS_FXR_DF[factorIndex+2, fxColName] <- beta
  VIX_MOM_VYM_TS_FXR_DF[factorIndex+3, fxColName] <- beta_t_stat
  VIX_MOM_VYM_TS_FXR_DF[factorIndex+4, fxColName] <- adjusted_r_squared

  # TS - FXR
  factorIndex <- 16
  # Removing first value in fxReturns and last value in Term Spreads to lag TS
  fxReturnsForLagTS <- fxReturns[-1]
  termSpreadForLagTS <- termSpread[-length(termSpread)]
  
  reg_ts_fxr <- lm(fxReturnsForLagTS~termSpreadForLagTS, data=factorsMergedData)
  reg_ts_fxr_summary <- summary(reg_ts_fxr)
  
  alpha <- reg_ts_fxr_summary$coefficients[1]
  alpha_t_stat <- reg_ts_fxr_summary$coefficients[1,3]
  beta <- reg_ts_fxr_summary$coefficients[2]
  beta_t_stat <- reg_ts_fxr_summary$coefficients[2,3]
  adjusted_r_squared <- reg_ts_fxr_summary$adj.r.squared
  
  VIX_MOM_VYM_TS_FXR_DF[factorIndex, fxColName] <- alpha
  VIX_MOM_VYM_TS_FXR_DF[factorIndex+1, fxColName] <- alpha_t_stat
  VIX_MOM_VYM_TS_FXR_DF[factorIndex+2, fxColName] <- beta
  VIX_MOM_VYM_TS_FXR_DF[factorIndex+3, fxColName] <- beta_t_stat
  VIX_MOM_VYM_TS_FXR_DF[factorIndex+4, fxColName] <- adjusted_r_squared

  return(VIX_MOM_VYM_TS_FXR_DF)
}


# Regressions of VIX & FX Volatility (SD)
fillVIX_MOM_FXV_Regression <- function(factorsMergedData, fxColName) {
  fxVolatility <- c(do.call("cbind", as.list(factorsMergedData$FX_Standard_Deviation)))
  vixFactor <- c(do.call("cbind", as.list(factorsMergedData$VIX)))
  momFactor <- c(do.call("cbind", as.list(factorsMergedData$MOM_Factor)))
  vym <- c(do.call("cbind", as.list(factorsMergedData$VYM)))
  termSpread <- c(do.call("cbind", as.list(factorsMergedData$TermSpread)))
  
  # VIX - FXV
  factorIndex <- 1
  reg_vix_fxv <- lm(fxVolatility~vixFactor, data=factorsMergedData)
  reg_vix_fxv_summary <- summary(reg_vix_fxv)
  
  alpha <- reg_vix_fxv_summary$coefficients[1]
  alpha_t_stat <- reg_vix_fxv_summary$coefficients[1,3]
  beta <- reg_vix_fxv_summary$coefficients[2]
  beta_t_stat <- reg_vix_fxv_summary$coefficients[2,3]
  adjusted_r_squared <- reg_vix_fxv_summary$adj.r.squared
  
  VIX_MOM_VYM_TS_FXV_DF[factorIndex, fxColName] <- alpha
  VIX_MOM_VYM_TS_FXV_DF[factorIndex+1, fxColName] <- alpha_t_stat
  VIX_MOM_VYM_TS_FXV_DF[factorIndex+2, fxColName] <- beta
  VIX_MOM_VYM_TS_FXV_DF[factorIndex+3, fxColName] <- beta_t_stat
  VIX_MOM_VYM_TS_FXV_DF[factorIndex+4, fxColName] <- adjusted_r_squared
  
  # MOM - FXV
  factorIndex <- 6
  reg_mom_fxv <- lm(fxVolatility~momFactor, data=factorsMergedData)
  reg_mom_fxv_summary <- summary(reg_mom_fxv)
  
  alpha <- reg_mom_fxv_summary$coefficients[1]
  alpha_t_stat <- reg_mom_fxv_summary$coefficients[1,3]
  beta <- reg_mom_fxv_summary$coefficients[2]
  beta_t_stat <- reg_mom_fxv_summary$coefficients[2,3]
  adjusted_r_squared <- reg_mom_fxv_summary$adj.r.squared
  
  VIX_MOM_VYM_TS_FXV_DF[factorIndex, fxColName] <- alpha
  VIX_MOM_VYM_TS_FXV_DF[factorIndex+1, fxColName] <- alpha_t_stat
  VIX_MOM_VYM_TS_FXV_DF[factorIndex+2, fxColName] <- beta
  VIX_MOM_VYM_TS_FXV_DF[factorIndex+3, fxColName] <- beta_t_stat
  VIX_MOM_VYM_TS_FXV_DF[factorIndex+4, fxColName] <- adjusted_r_squared

  # VYM - FXV
  factorIndex <- 11
  reg_vym_fxv <- lm(fxVolatility~vym, data=factorsMergedData)
  reg_vym_fxv_summary <- summary(reg_vym_fxv)
  
  alpha <- reg_vym_fxv_summary$coefficients[1]
  alpha_t_stat <- reg_vym_fxv_summary$coefficients[1,3]
  beta <- reg_vym_fxv_summary$coefficients[2]
  beta_t_stat <- reg_vym_fxv_summary$coefficients[2,3]
  adjusted_r_squared <- reg_vym_fxv_summary$adj.r.squared
  
  VIX_MOM_VYM_TS_FXV_DF[factorIndex, fxColName] <- alpha
  VIX_MOM_VYM_TS_FXV_DF[factorIndex+1, fxColName] <- alpha_t_stat
  VIX_MOM_VYM_TS_FXV_DF[factorIndex+2, fxColName] <- beta
  VIX_MOM_VYM_TS_FXV_DF[factorIndex+3, fxColName] <- beta_t_stat
  VIX_MOM_VYM_TS_FXV_DF[factorIndex+4, fxColName] <- adjusted_r_squared
  
  # TS - FXV
  factorIndex <- 16
  # Removing first value in fxReturns and last value in Term Spreads to lag TS
  fxVolatilityForLagTS <- fxVolatility[-1]
  termSpreadForLagTS <- termSpread[-length(termSpread)]
  
  reg_ts_fxv <- lm(fxVolatilityForLagTS~termSpreadForLagTS, data=factorsMergedData)
  reg_ts_fxv_summary <- summary(reg_ts_fxv)
  
  alpha <- reg_ts_fxv_summary$coefficients[1]
  alpha_t_stat <- reg_ts_fxv_summary$coefficients[1,3]
  beta <- reg_ts_fxv_summary$coefficients[2]
  beta_t_stat <- reg_ts_fxv_summary$coefficients[2,3]
  adjusted_r_squared <- reg_ts_fxv_summary$adj.r.squared
  
  VIX_MOM_VYM_TS_FXV_DF[factorIndex, fxColName] <- alpha
  VIX_MOM_VYM_TS_FXV_DF[factorIndex+1, fxColName] <- alpha_t_stat
  VIX_MOM_VYM_TS_FXV_DF[factorIndex+2, fxColName] <- beta
  VIX_MOM_VYM_TS_FXV_DF[factorIndex+3, fxColName] <- beta_t_stat
  VIX_MOM_VYM_TS_FXV_DF[factorIndex+4, fxColName] <- adjusted_r_squared
  
  return(VIX_MOM_VYM_TS_FXV_DF)
}

# wrapper function to compute all 4 regressions and fill tables accordingly
getFactorCoEffs <- function(vixMomentum, fx) {
  # Regressions of VIX+MOM over FX Returns
  VIX_MOM_VYM_TS_FXR_DF <<- fillVIX_MOM_FXR_Regression(vixMomentum, paste(fx, "Returns", sep = "_"))
  
  # Regressions of VIX+MOM over FX Volatility (SD) 
  VIX_MOM_VYM_TS_FXV_DF <<- fillVIX_MOM_FXV_Regression(vixMomentum, paste(fx, "Volatility", sep = "_"))
}

# Defining function for adding VIX_MOM & VYM_TS to FX RV's
merge_VIX_MOM_VYM_TS_FX <- function(fxRV) {
  # For large tables in R dplyr's function inner_join() is much faster than merge()
  mergedDataFrame <- inner_join(VIX_MOM, fxRV, by="Date") %>%
    inner_join(., VYM_TS, by="Date") %>%
    select(Date, VIX, MOM_Factor, VYM, TermSpread, FX_Returns, FX_Standard_Deviation)
  
  return(mergedDataFrame)
}

for(i in 1:length(fxRVs)) {
  assign(paste("VIX_MOM_VYM_TS", fxNameList[i], sep = "_"), merge_VIX_MOM_VYM_TS_FX(fxRVs[[i]]))
}

print('Compute & fill factor regression values')
getFactorCoEffs(VIX_MOM_VYM_TS_EURUSD, 'EURUSD')
getFactorCoEffs(VIX_MOM_VYM_TS_USDCHF, 'USDCHF')
getFactorCoEffs(VIX_MOM_VYM_TS_USDJPY, 'USDJPY')
getFactorCoEffs(VIX_MOM_VYM_TS_USDCAD, 'USDCAD')
getFactorCoEffs(VIX_MOM_VYM_TS_AUDUSD, 'AUDUSD')
getFactorCoEffs(VIX_MOM_VYM_TS_NZDUSD, 'NZDUSD')
getFactorCoEffs(VIX_MOM_VYM_TS_USDSEK, 'USDSEK')
getFactorCoEffs(VIX_MOM_VYM_TS_USDNOK, 'USDNOK')
getFactorCoEffs(VIX_MOM_VYM_TS_USDMXN, 'USDMXN')
getFactorCoEffs(VIX_MOM_VYM_TS_GBPUSD, 'GBPUSD')

print("Factor computations completed")

print("VIX, Momentum over FX Returns Regression table")
VIX_MOM_VYM_TS_FXR_DF
print("VIX, Momentum over FX Volatility Regression table")
VIX_MOM_VYM_TS_FXV_DF


# ===========================
# ===========================
# ===========================
# =======CORRELATIONS========
# ===========================
# ===========================

# Creating empty data frames(tables)
# Unlike pairs
prepareCorrelationTable <- function(rowSuffix, colSuffix) {
  # Symbols & Currencies vectors for column and row names
  EquitySymbols <- c(paste('GSPC', rowSuffix, sep="_"), paste('GDAXI', rowSuffix, sep="_"), paste('SSMI', rowSuffix, sep="_"),
                      paste('N225', rowSuffix, sep="_"), paste('GSPTSE', rowSuffix, sep="_"), paste('AORD', rowSuffix, sep="_"),
                      paste('NZ50', rowSuffix, sep="_"), paste('OMX', rowSuffix, sep="_"), paste('FTSE', rowSuffix, sep="_"),
                      paste('MXX', rowSuffix, sep="_"))
  FxCurrencies <- c("", paste('EURUSD', colSuffix, sep="_"), paste('USDCHF', colSuffix, sep="_"), paste('USDJPY', colSuffix, sep="_"),
                    paste('USDCAD', colSuffix, sep="_"), paste('AUDUSD', colSuffix, sep="_"), paste('NZDUSD', colSuffix, sep="_"), paste('USDSEK', colSuffix, sep="_"),
                    paste('USDNOK', colSuffix, sep="_"), paste('GBPUSD', colSuffix, sep="_"), paste('USDMXN', colSuffix, sep="_"))
  correlationTable <- data.frame(matrix(0, ncol = 11, nrow = 10))
  colnames(correlationTable) <- FxCurrencies
  correlationTable[,1] <- EquitySymbols
  return(correlationTable)
}

# Like pairs - EQUITY
prepareLikePairEquityCorrelationTable <- function(rowSuffix, colSuffix) {
  # Symbols & Currencies vectors for column and row names
  EquitySymbolsCol <- c('', paste('GSPC', colSuffix, sep="_"), paste('GDAXI', colSuffix, sep="_"), paste('SSMI', colSuffix, sep="_"),
                      paste('N225', colSuffix, sep="_"), paste('GSPTSE', colSuffix, sep="_"), paste('AORD', colSuffix, sep="_"),
                      paste('NZ50', colSuffix, sep="_"), paste('OMX', colSuffix, sep="_"), paste('FTSE', colSuffix, sep="_"),
                      paste('MXX', colSuffix, sep="_"))
  EquitySymbolsRow <- c(paste('GSPC', rowSuffix, sep="_"), paste('GDAXI', rowSuffix, sep="_"), paste('SSMI', rowSuffix, sep="_"),
                      paste('N225', rowSuffix, sep="_"), paste('GSPTSE', rowSuffix, sep="_"), paste('AORD', rowSuffix, sep="_"),
                      paste('NZ50', rowSuffix, sep="_"), paste('OMX', rowSuffix, sep="_"), paste('FTSE', rowSuffix, sep="_"),
                      paste('MXX', rowSuffix, sep="_"))
  
  correlationTable <- data.frame(matrix(0, ncol = 11, nrow = 10))
  colnames(correlationTable) <- EquitySymbolsCol
  correlationTable[,1] <- EquitySymbolsRow
  return(correlationTable)
}

# Like pairs - fX
prepareLikePairFXCorrelationTable <- function(rowSuffix, colSuffix) {
  # Symbols & Currencies vectors for column and row names
  FxCurrenciesCol <- c('', paste('EURUSD', colSuffix, sep="_"), paste('USDCHF', colSuffix, sep="_"), paste('USDJPY', colSuffix, sep="_"),
                    paste('USDCAD', colSuffix, sep="_"), paste('AUDUSD', colSuffix, sep="_"), paste('NZDUSD', colSuffix, sep="_"), paste('USDSEK', colSuffix, sep="_"),
                    paste('USDNOK', colSuffix, sep="_"), paste('GBPUSD', colSuffix, sep="_"), paste('USDMXN', colSuffix, sep="_"))
  FxCurrenciesRow <- c(paste('EURUSD', rowSuffix, sep="_"), paste('USDCHF', rowSuffix, sep="_"), paste('USDJPY', rowSuffix, sep="_"),
                    paste('USDCAD', rowSuffix, sep="_"), paste('AUDUSD', rowSuffix, sep="_"), paste('NZDUSD', rowSuffix, sep="_"), paste('USDSEK', rowSuffix, sep="_"),
                    paste('USDNOK', rowSuffix, sep="_"), paste('GBPUSD', rowSuffix, sep="_"), paste('USDMXN', rowSuffix, sep="_"))
  
  correlationTable <- data.frame(matrix(0, ncol = 11, nrow = 10))
  colnames(correlationTable) <- FxCurrenciesCol
  correlationTable[,1] <- FxCurrenciesRow
  return(correlationTable)
}

# Defining the functions for filling those empty tables----------
# Unlike pairs Correlations
fillFXV_EQR_Correlation <- function(fxEquityReturnsVolatility, rowIndex, colIndex) {
  fxVolatility <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Standard_Deviation)))
  eqReturns <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Returns)))
  correl_fxv_eqr <- cor.test(fxVolatility, eqReturns)
  Cor_FXV_EQR_DF[rowIndex, colIndex]  <- correl_fxv_eqr$estimate[1]
  return(Cor_FXV_EQR_DF)
}

fillFXV_EQV_Correlation <- function(fxEquityReturnsVolatility, rowIndex, colIndex) {
  fxVolatility <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Standard_Deviation)))
  eqVolatility <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Standard_Deviation)))
  correl_fxv_eqv <- cor.test(fxVolatility, eqVolatility)
  Cor_FXV_EQV_DF[rowIndex, colIndex]  <- correl_fxv_eqv$estimate[1]
  return(Cor_FXV_EQV_DF)
}  

fillFXR_EQR_Correlation <- function(fxEquityReturnsVolatility, rowIndex, colIndex) {
  fxReturns <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Returns)))
  eqReturns <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Returns)))
  correl_fxr_eqr <- cor.test(fxReturns, eqReturns)
  Cor_FXR_EQR_DF[rowIndex, colIndex]  <- correl_fxr_eqr$estimate[1]
  return(Cor_FXR_EQR_DF)
}  

fillFXR_EQV_Correlation <- function(fxEquityReturnsVolatility, rowIndex, colIndex) {
  fxReturns <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Returns)))
  eqVolatility <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Standard_Deviation)))
  correl_fxr_eqv <- cor.test(fxReturns, eqVolatility)
  Cor_FXR_EQV_DF[rowIndex, colIndex]  <- correl_fxr_eqv$estimate[1]
  return(Cor_FXR_EQV_DF)
}

# Like Pair Correlations
fillEQR_EQR_Correlation <- function(fxEquityReturnsVolatility, equityIndex, equityName){
  # Same Equity Correlation : Eg : GSPC on GSPC
  eqReturnsX <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Returns.x)))
  eqReturnsY <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Returns.y)))
  correl_eqr_eqr <- cor.test(eqReturnsX, eqReturnsY)
  Cor_EQR_EQR_DF[equityIndex, equityName]  <- correl_eqr_eqr$estimate[1]

  return(Cor_EQR_EQR_DF)
}

# fxEquityReturnsVolatility : Fixed Equity RV + Variable Equity RV
fillEQV_EQV_Correlation <- function(fxEquityReturnsVolatility, equityIndex, equityName){
  eqVolatilityX <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Standard_Deviation.x)))
  eqVolatilityY <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Standard_Deviation.y)))
  correl_eqv_eqv <- cor.test(eqVolatilityX, eqVolatilityY)
  Cor_EQV_EQV_DF[equityIndex, equityName]  <- correl_eqv_eqv$estimate[1]
  return(Cor_EQV_EQV_DF)
}

# fxEquityReturnsVolatility : Fixed FX RV + Variable FX RV
fillFXR_FXR_Correlation <- function(fxEquityReturnsVolatility, fxIndex, fxName){
  fxReturnsX <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Returns.x)))
  fxReturnsY <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Returns.y)))
  correl_fxr_fxr <- cor.test(fxReturnsX, fxReturnsY)
  Cor_FXR_FXR_DF[fxIndex, fxName]  <- correl_fxr_fxr$estimate[1]
  return(Cor_FXR_FXR_DF)
}

# fxEquityReturnsVolatility : Fixed FX RV + Variable FX RV
fillFXV_FXV_Correlation <- function(fxEquityReturnsVolatility, fxIndex, fxName){
  fxVolatilityX <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Standard_Deviation.x)))
  fxVolatilityY <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Standard_Deviation.y)))
  correl_fxv_fxv <- cor.test(fxVolatilityX, fxVolatilityY)
  print(fxIndex)
  Cor_FXV_FXV_DF[fxIndex, fxName]  <- correl_fxv_fxv$estimate[1]
  return(Cor_FXV_FXV_DF)
}

# Putting it in the data frames prepared earlier-------
# X axis -> FX Volatility
# Y axis -> EQUITY Returns
Cor_FXV_EQR_DF <- prepareCorrelationTable("Volatility", "Returns")

# X axis -> FX Volatility
# Y axis -> EQUITY Volatility
Cor_FXV_EQV_DF <- prepareCorrelationTable("Volatility", "Volatility")

# X axis -> FX Returns
# Y axis -> EQUITY Returns
Cor_FXR_EQR_DF <- prepareCorrelationTable("Returns", "Returns")

# X axis -> FX Returns
# Y axis -> EQUITY Volatility
Cor_FXR_EQV_DF <- prepareCorrelationTable("Returns", "Volatility")

# Unlike pairs
# X axis -> Equity Returns
# Y axis -> EQUITY Returns
Cor_EQR_EQR_DF <- prepareLikePairEquityCorrelationTable("Returns", "Returns")

# X axis -> EQUITY Volatility
# Y axis -> EQUITY Volatility
Cor_EQV_EQV_DF <- prepareLikePairEquityCorrelationTable("Volatility", "Volatility")

# X axis -> FX Returns
# Y axis -> FX Returns
Cor_FXR_FXR_DF <- prepareLikePairFXCorrelationTable("Returns", "Returns")

# X axis -> FX Volatility
# Y axis -> FX Volatility
Cor_FXV_FXV_DF <- prepareLikePairFXCorrelationTable("Volatility", "Volatility")

# Loop Logic : Fix ONE Equity & ONE FX and rotate all the remaining Equities & Fx combinations.
# Parameters : Fixed Equity, Fixed FX, Variable Equity, Variable FX
# Compute & fill tables
# GSPC -> FXs
# 'GSPC' : Index 1


for(i in 1:length(equityRVs)) {
  for(j in 1:length(fxRVs)) {
    # for i'th Equity & j'th Fx
    print(paste("Filling Unlike Table Row : ", i, " Col : ", j, sep=""))
    
    # Merge RV data frames by Date
    fxEquityReturnsVolatility <<- mergeAndFormatRVs(equityRVs[[i]], fxRVs[[j]])
    
    # Correlations of FX Volatility & Equity Returns (SD)
    Cor_FXV_EQR_DF <<- fillFXV_EQR_Correlation(fxEquityReturnsVolatility, i, 1+j)
    
    # Correlations of FX Volatility & Equity Volatility (SD) 
    Cor_FXV_EQV_DF <<- fillFXV_EQV_Correlation(fxEquityReturnsVolatility, i, 1+j)
    
    # Correlations of FX Returns & Equity Returns
    Cor_FXR_EQR_DF <<- fillFXR_EQR_Correlation(fxEquityReturnsVolatility, i, 1+j)
    
    # Correlations of FX Returns & Equity Volatility (SD) 
    Cor_FXR_EQV_DF <<- fillFXR_EQV_Correlation(fxEquityReturnsVolatility, i, 1+j)
  }
}


for(i in 1:length(equityRVs)) {
  for(j in 1:length(equityRVs)) {
    # for i'th Equity & j'th Equity
    print(paste("Filling Like Equity Table Row : ", i, " Col : ", j, sep=""))
    
    # Merge RV data frames by Date
    equityReturnsVolatility <<- mergeAndFormatRVs(equityRVs[[i]], equityRVs[[j]])
    
    # Correlations of Equity Returns & Equity Returns
    Cor_EQR_EQR_DF <<- fillEQR_EQR_Correlation(equityReturnsVolatility, i, 1+j)
    
    # Correlations of Equity Volatility & Equity Volatility 
    Cor_EQV_EQV_DF <<- fillEQV_EQV_Correlation(equityReturnsVolatility, i, 1+j)
  }
}


for(i in 1:length(fxRVs)) {
  for(j in 1:length(fxRVs)) {
    # for i'th FX & j'th FX
    print(paste("Filling Like FX Table Row : ", i, " Col : ", j, sep=""))
    
    # Merge RV data frames by Date
    fxReturnsVolatility <<- mergeAndFormatRVs(fxRVs[[i]], fxRVs[[j]])
    
    # Correlations of FX Returns & FX Returns
    Cor_FXR_FXR_DF <<- fillFXR_FXR_Correlation(fxReturnsVolatility, i, 1+j)
    
    # Correlations of FX Volatility & FX Volatility
    Cor_FXV_FXV_DF <<- fillFXV_FXV_Correlation(fxReturnsVolatility, i, 1+j)
  }
}


print("Correlations computations completed")

# Unlike pairs
print("Filled Correlations FXV_EQR Table")
Cor_FXV_EQR_DF

print("Filled Correlations FXV_EQV Table")
Cor_FXV_EQV_DF

print("Filled Correlations FXR_EQR Table")
Cor_FXR_EQR_DF

print("Filled Correlations FXR_EQV Table")
Cor_FXR_EQV_DF

# Like pairs
print("Filled Correlations EQR_EQR Table")
Cor_EQR_EQR_DF

print("Filled Correlations EQV_EQV Table")
Cor_EQV_EQV_DF

print("Filled Correlations FXR_FXR Table")
Cor_FXR_FXR_DF

print("Filled Correlations FXV_FXV Table")
Cor_FXV_FXV_DF


# Save to CSV's
getwd()
# Mac
# setwd('../../WM_Fix_Results/')
# PC
setwd('..\\..\\WM_Fix_Results\\')

# Save Regression results into CSVs
write.table(FXV_EQR_DF, file = "Reg_FXV_EQR_DF.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(FXV_EQV_DF, file = "Reg_FXV_EQV_DF.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(FXR_EQR_DF, file = "Reg_FXR_EQR_DF.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(FXR_EQV_DF, file = "Reg_FXR_EQV_DF.csv", sep = ",", col.names = NA, qmethod = "double")

# Save VIX MOM results into CSVs
write.table(VIX_MOM_VYM_TS_FXR_DF, file = "VIX_MOM_VYM_TS_FXR_DF.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(VIX_MOM_VYM_TS_FXV_DF, file = "VIX_MOM_VYM_TS_FXV_DF.csv", sep = ",", col.names = NA, qmethod = "double")

# Save Unlike Correlation results into CSVs
write.table(Cor_FXV_EQR_DF, file = "Cor_FXV_EQR_DF.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(Cor_FXV_EQV_DF, file = "Cor_FXV_EQV_DF.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(Cor_FXR_EQR_DF, file = "Cor_FXR_EQR_DF.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(Cor_FXR_EQV_DF, file = "Cor_FXR_EQV_DF.csv", sep = ",", col.names = NA, qmethod = "double")

# Save Unlike Correlation results into CSVs
write.table(Cor_EQR_EQR_DF, file = "Cor_EQR_EQR_DF.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(Cor_EQV_EQV_DF, file = "Cor_EQV_EQV_DF.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(Cor_FXR_FXR_DF, file = "Cor_FXR_FXR_DF.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(Cor_FXV_FXV_DF, file = "Cor_FXV_FXV_DF.csv", sep = ",", col.names = NA, qmethod = "double")

