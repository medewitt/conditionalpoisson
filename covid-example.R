library(data.table)
library(ggplot2)
library(dplyr)
library(haven)
library(gnm)

# read in the data --------------------------------------------------------

hh <- here::here

cases <- nccovid::pull_covid_summary()
hospitalizations <- nccovid::get_hospitalizations()

admits <- hospitalizations[,.(admits = sum(at_admission_confirmed_covid19_patients_in_24_hrs)), by = "date"]



combined_at <- merge(cases[,.(date,deaths_by_date, cases = cases_reported, n_tests)], admits, by = "date", all.y = TRUE)

combined_dat <- combined_at[!is.na(admits)][!is.na(deaths_by_date)]

# Add Variant Swags
combined_dat[,Variant:=fcase(date < "2021-03-15","Original",
             date < "2021-07-25","Alpha",
             date < "2021-12-15","Delta",
             default = "Omicron")]

combined_dat[,Variant:=factor(Variant, c("Original", "Alpha", "Delta", "Omicron"))]

p1 <- combined_dat %>%
  ggplot(aes(date,admits, color = Variant))+
  geom_line()+
  labs(
    title = "Trend of Admissions",
    y = "Number of Admissions",
    x = NULL
  )+
  theme_classic()
p2 <- combined_dat %>%
  ggplot(aes(date,cases, color = Variant))+
  geom_line()+
  labs(
    title = "Trend of Cases",
    y = "Cases",
    x = NULL
  )+
  theme_classic()

cowplot::plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12)


# do the analysis like they do --------------------------------------------
summary(combined_dat)

# SET THE DEFAULT ACTION FOR MISSING DATA TO na.exclude
# (MISSING EXCLUDED IN ESTIMATION BUT RE-INSERTED IN PREDICTION/RESIDUALS)
options(na.action="na.exclude")

# SCALE EXPOSURE
combined_dat$cases100 <- combined_dat$cases/100

# GENERATE MONTH AND YEAR
combined_dat$month  <- as.factor(months(combined_dat$date))
combined_dat$year   <- as.factor(format(combined_dat$date, format="%Y") )
combined_dat$dow    <- as.factor(weekdays(combined_dat$date))
combined_dat$stratum <- as.factor(combined_dat$year:combined_dat$month:combined_dat$dow)

combined_dat <- combined_dat[order(combined_dat$date),]
combined_dat$tst <- rnorm(nrow(combined_dat))
# FIT A CONDITIONAL POISSON MODEL WITH A YEAR X MONTH X DOW STRATA
variant_cpr <- gnm(admits ~ cases100 + Variant+tst, data=subset(combined_dat,date > Sys.Date()-180), family=quasipoisson(),
                 eliminate=factor(stratum))

summary(variant_cpr)

confint(variant_cpr)*100

plot(predict(variant_cpr, type = "response"))

plot(predict(variant_cpr))
