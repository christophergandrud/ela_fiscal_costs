#################################################
# Explore link between crisis costs, banking system size and discount rate
# Christopher Gandrud
# 20 January 2015
# MIT License
#################################################

# Load packages
library(dplyr)
library(WDI)
library(repmis)
library(DataCombine)
library(ggplot2)
library(xlsx)
library(tidyr)
library(countrycode)


#### Gather Data ####
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
discount_rate <- read.xlsx('~/Desktop/christopher.gandrud@gmail.com_Workbook.xlsx', sheetIndex = 1)
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

dr_gathered$iso2c <- countrycode(dr_gathered$country,
                                            origin = 'country.name',
                                            destination = 'iso2c')
dr_gathered <- select(dr_gathered, -country) %>%
                            filter(year >= 1990)

# merge
comb <- merge(wdi, costs, all.x = TRUE)
comb <- merge(comb, dr_gathered, all.x = TRUE)
comb <- merge(eu, comb, all = TRUE, by = 'iso2c')

comb <- FillIn(comb, dr_euro, Var1 = 'discount_rate',
               KeyVar = c('membership', 'year')) %>%
            arrange(iso2c, year)

comb$grouping <- 'Not Europe/JP/US'
comb$grouping[!is.na(comb$membership)] <- 'Europe/JP/US'

comb$grouping <- factor(comb$grouping)

for (i in c("CentralBankAssetsGDP", "BankAssetsGDP")){
    comb <- slide(comb, Var = i, GroupVar = 'iso2c', slideBy = -1,
                  NewVar = paste0(i, '_lag1'))
}

comb_sub <- subset(comb, !is.na(LV2012.Fiscal))

comb_sub$costs_deviation <- comb_sub$LV2012.Fiscal / comb_sub$BankAssetsGDP_lag1

vars <- c('CentralBankAssetsGDP_lag1', 'BankAssetsGDP_lag1',
          'discount_rate', 'LV2012.Fiscal', 'costs_deviation')
for (i in vars) {
    comb_sub[, paste0(i, '_log')] <- log(comb_sub[, i])
}

comb_sub <- subset(comb_sub, costs_deviation_log != -Inf)

#### Exploratory plots ####
ggplot(comb_sub, aes(BankAssetsGDP_lag1_log, LV2012.Fiscal,
                     label = iso2c, color = grouping)) +
    geom_text() +
    scale_color_brewer(palette = 'Set1') +
    stat_smooth(method = 'lm', se = F) +
    xlab('\nDeposit Bank Assets/GDP (log, 1 year lag)') +
    ylab('Fiscal Costs/GDP (Laven & Valencia)\n') +
    theme_bw()

ggplot(comb_sub, aes(discount_rate_log, costs_deviation_log,
                     label = iso2c)) +
    geom_text() +
    stat_smooth(method = 'lm', se = F) +
    xlab('\nDiscount rate (log)') +
    ylab('Fiscal costs / Bank Assets (log, 1 year lag)\n') +
    theme_bw()

m1 <- lm(LV2012.Fiscal ~ discount_rate_log + BankAssetsGDP_lag1_log,
         data = comb_sub)

m2 <- lm(LV2012.Fiscal ~ CentralBankAssetsGDP_lag1_log,
         data = comb_sub)

m3 <- lm(costs_deviation_log ~ discount_rate_log,
         data = comb_sub)
