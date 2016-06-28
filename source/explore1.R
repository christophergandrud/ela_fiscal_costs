################################################################################
# Explore relationship between crisis costs, banking system size, and
# discount rate
# Christopher Gandrud
# 23 January 2015
# MIT License
################################################################################

# Load required packages
library(ggplot2)
library(DataCombine)

# Set working directory. Change as needed.
setwd('/git_repositories/ela_fiscal_costs/')

# Load data created by ELA_data_gather.R
main <- read.csv('source/data/main_data.csv')
main$eurozone <- factor(main$eurozone, labels = c('Not Member', 'Member'))

# Drop countries with no crisis costs.
## These create -Inf log values and, as in the case of Portugal 2008, are
## implossible.
sub <- subset(main, costs_deviation_log != -Inf) 
sub <- subset(sub, iso2c != 'NG')

#### Exploratory plots ####
# Change in domestic credit prevision and Costs 
ggplot(sub, aes(DomesticCredit_change_ma3, LV2012.Fiscal, 
                     label = iso2c)) +
    geom_text() +
    stat_smooth(method = 'lm', se = F) +
    xlab('\nChange in Domestic Credit (3 year moving average)') +
    ylab('Fiscal Costs/GDP (Laven & Valencia)\n') +
    theme_bw()

# Change in domestic credit provision and Costs, grouped
ggplot(sub, aes(DomesticCredit_change_ma3, LV2012.Fiscal, 
                 label = iso2c, color = major_currency)) +
    geom_text(size = 3) +
    scale_color_brewer(palette = 'Set1') +
    stat_smooth(method = 'lm', se = F) +
    theme_bw()

png('figures/Size_Costs.png')
ggplot(sub, aes(BankAssetsGDP_lag1_log, LV2012.Fiscal,
                label = iso2c, color = major_currency)) +
    geom_text(size = 3) +
    scale_color_brewer(palette = 'Set1') +
    stat_smooth(method = 'lm', se = F) +
    xlab('\nBank Assets/GDP (log, 1 year lag)') +
    ylab('Fiscal Costs/GDP (Laven & Valencia)\n') +
    theme_bw()
dev.off()

png('figures/DiscountRate_CostsStnd.png')
ggplot(sub, aes(discount_rate_ma3_log, costs_deviation_log,
                     label = iso2c, color = eurozone)) +
    geom_text(size = 4, position = position_dodge()) +
    stat_smooth(method = 'lm', se = F) +
    scale_color_brewer(palette = 'Set1') +
    xlab('\nDiscount rate (log, 3 year average lead)') +
    ylab('Fiscal costs / Bank Assets (log, 1 year lag)\n') +
    theme_bw()
dev.off()

m1 <- lm(costs_deviation_log ~ discount_rate_ma3_log, data = sub)

#### Exploritory regressions
# RandomForests
library(randomForest)

# Remove NAs
rmNA <- function(data, vars) {
    temp <- DropNA(data, vars)
    temp <- temp[, vars]   
    temp
}

vars_dr <- c('iso2c', 'costs_deviation_log', 'discount_rate_ma3_log', 'eurozone')
sub_dr <- rmNA(sub, vars_dr)

rf1 <- randomForest(costs_deviation_log ~ discount_rate_ma3_log + eurozone, 
                    data = sub_dr, 
                    importance = T, do.trace=100)

# Predict costs from model
rf1_est <- predict(rf1, sub_dr)

sub_dr_predict <- cbind(sub_dr, rf1_est)

ggplot(sub_dr_predict, aes(costs_deviation_log, rf1_est, color = eurozone)) +
        geom_point() +
        stat_smooth(method = 'lm', se = F) +
        scale_color_brewer(palette = 'Set1') +
        xlab('\nActual costs deviation (log)') + 
        ylab('Predicted costs deviation\n') +
        theme_bw()

disk <- sub %>% select(country, year, discount_rate)

m1 <- lm(LV2012.Fiscal ~ DomesticCredit_change_ma3,
         data = sub)

m1 <- lm(LV2012.Fiscal ~ discount_rate_log + BankAssetsGDP_lag1_log + DomesticCredit_change_ma3,
         data = sub)

m2 <- lm(LV2012.Fiscal ~ CentralBankAssetsGDP_lag1_log,
         data = sub)

m3 <- lm(costs_deviation_log ~ discount_rate_ma3_log + DomesticCredit_change_ma3,
         data = sub)

m4 <- lm(costs_deviation_log ~ DomesticCredit_change_ma3,
         data = sub)
