################################################################################
# Gather data on crisis costs, banking system size, and discount rate
# Christopher Gandrud
# 20 January 2015
# MIT License
################################################################################

# Load packages
library(dplyr)
library(WDI)
library(repmis)
library(DataCombine)
library(xlsx)
library(tidyr)
library(lubridate)
library(countrycode)

# Set working directory. Change as needed.
setwd('/git_repositories/ela_fiscal_costs/source/')

#### Gather Data ###############################################################
indicators <- c('GFDD.DI.06', 'GFDD.DI.02')

wdi <- WDI(indicator = indicators, start = 1990, end = 2013) %>%
        rename(CentralBankAssetsGDP = GFDD.DI.06,
                BankAssetsGDP = GFDD.DI.02)

# EU Membership
eu <- "https://raw.githubusercontent.com/christophergandrud/BankingUnionMaps/master/csv/BankingUnionMembers.csv" %>%
        source_data() %>%
        select(iso2c, membership)

# Download LV crisis costs
costs <-
    'https://raw.githubusercontent.com/christophergandrud/Keefer2007Replication/master/data/KefferFiscal.csv' %>%
    source_data() %>%
    select(iso2c, year, LV2012.Fiscal)

# Discount rate
discount_rate <- read.xlsx('data/raw/IMF_discount_rate.xlsx',
                    sheetIndex = 1)
dr_gathered <- discount_rate %>%
                gather(year, discount_rate, 2:ncol(discount_rate)) %>%
                rename(country = c..Interest.Rates..Discount..Percent.per.annum..Zimbabwe....Interest.Rates..Discount..Percent.per.annum..Zambia...)
dr_gathered$country <- gsub('Interest Rates, Discount, Percent per annum, ',
                                       '', dr_gathered$country)
dr_gathered$year <- gsub('X', '', dr_gathered$year) %>%
                                as.numeric()
dr_gathered <- subset(dr_gathered, !is.na(discount_rate))

# combined discount rate for Euro area countries
dr_euro <- subset(dr_gathered, country == 'Euro Area') %>% select(-country)
dr_euro$membership <- 'SSM'

# Insert discount rate for the United Kingdom
dr_uk <- read.csv('data/raw/BoE_annual_discount_rate.csv',
                    stringsAsFactors = FALSE) %>%
                rename(year = DATE, discount_rate = IUAABEDR)
dr_uk$year <- dr_uk$year %>% dmy() %>% year()
dr_uk$country <- 'United Kingdom'

dr_gathered <- bind_rows(dr_gathered, dr_uk)

dr_gathered$iso2c <- countrycode(dr_gathered$country,
                                            origin = 'country.name',
                                            destination = 'iso2c')
dr_gathered <- select(dr_gathered, -country) %>%
                            filter(year >= 1990)

#### Merge #####################################################################
comb <- merge(wdi, costs, all.x = TRUE)
comb <- merge(comb, dr_gathered, all.x = TRUE)
comb <- merge(eu, comb, all = TRUE, by = 'iso2c')

comb <- FillIn(comb, dr_euro, Var1 = 'discount_rate',
               KeyVar = c('membership', 'year')) %>%
            arrange(iso2c, year)


#### Clean Up, lag, log ########################################################
# Creat Europe/JP/US group
comb$grouping <- 'Not Europe/JP/US'
comb$grouping[!is.na(comb$membership)] <- 'Europe/JP/US'

comb$grouping <- factor(comb$grouping)

# Create 1 year lags
for (i in c("CentralBankAssetsGDP", "BankAssetsGDP")){
    comb <- slide(comb, Var = i, GroupVar = 'iso2c', slideBy = -1,
                  NewVar = paste0(i, '_lag1'))
}

# Create 3 year lead average for discount rate 
## i.e. average discount rate over the course of the 'crisis'
comb <- slideMA(comb, Var = 'discount_rate', GroupVar = 'iso2c',
                             periodBound = 2, offset = 0, 
                             NewVar = 'discount_rate_ma3')

# Keep only crisis years
comb_sub <- subset(comb, !is.na(LV2012.Fiscal))

# Standardise costs by banking system size
comb_sub$costs_deviation <- comb_sub$LV2012.Fiscal / comb_sub$BankAssetsGDP_lag1

# Create log variables
vars <- c('CentralBankAssetsGDP_lag1', 'BankAssetsGDP_lag1',
          'discount_rate', 'discount_rate_ma3', 'LV2012.Fiscal', 
          'costs_deviation')
for (i in vars) {
    comb_sub[, paste0(i, '_log')] <- log(comb_sub[, i])
}

# Drop countries with no crisis costs.
## These create -Inf log values and, as in the case of Portugal 2008, are
## implossible.
comb_sub <- subset(comb_sub, costs_deviation_log != -Inf)

# Save data as CSV
write.csv(comb_sub, file = 'data/main_data.csv', row.names = FALSE)
