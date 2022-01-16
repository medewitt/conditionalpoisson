library(data.table)
library(ggplot2)
library(dplyr)
library(haven)
library(gnm)

# read in the data --------------------------------------------------------

hh <- here::here

cases <- nccovid::pull_covid_summary()
hospitalizations <- nccovid::get_hospitalizations()

admits <- hospitalizations[,.(admits = sum(at_admission_confirmed_covid19_patients_in_24_hrs),
                              ed_visits = sum(ed_covid_visits_previous_day),
                              icu = sum(full_adult_icu_covid19_positive_patients)), by = "date"]

variant_information <- fread(hh("data-raw", "variant.csv"))[,list(DateDT,Variant,Perc)]

variant_information[,DateDT:=DateDT-30]

admits <- admits[order(date)][!is.na(ed_visits)]

mobility <- rbindlist(lapply(list.files(pattern = ".csv"), fread))

nc_mobility <-mobility[sub_region_1=="North Carolina"][
  sub_region_2==""][
    ,list(date,retail = retail_and_recreation_percent_change_from_baseline)]

combined_at <- merge(cases[,.(date,deaths_by_date, cases = cases_reported, n_tests)], admits, by = "date", all.y = TRUE)

combined_dat <- combined_at[!is.na(admits)][!is.na(deaths_by_date)]

combined_dat <- merge(combined_dat,nc_mobility, by = "date", all.x = TRUE)

library(tidyr)
combined_dat_full <- combined_dat %>%
  gather(metric, value, -date, -retail) %>%
  left_join(variant_information, by = c("date" = "DateDT")) %>%
  filter(!is.na(Variant)) %>%
  mutate(value =  round(value * Perc)) %>%
  select(date,retail, metric, value, Variant) %>%
  spread(metric, value, fill = 0)

# # Add Variant Swags
# combined_dat[,Variant:=fcase(date < "2021-03-15","Original",
#              date < "2021-07-25","Alpha",
#              date < "2021-12-15","Delta",
#              default = "Omicron")]
#
# combined_dat[,Variant:=factor(Variant, c("Original", "Alpha", "Delta", "Omicron"))]

p1 <- combined_dat_full %>%
  ggplot(aes(date,admits, color = Variant))+
  geom_line()+
  labs(
    title = "Trend of Admissions",
    y = "Number of Admissions",
    x = NULL
  )+
  theme_classic()
p2 <- combined_dat_full %>%
  ggplot(aes(date,cases, color = Variant))+
  geom_line()+
  labs(
    title = "Trend of Cases",
    y = "Cases",
    x = NULL
  )+
  theme_classic()

p3 <- combined_dat_full %>%
  ggplot(aes(date,deaths_by_date, color = Variant))+
  geom_line()+
  labs(
    title = "Trend of Deaths",
    y = "Cases",
    x = NULL
  )+
  theme_classic()

p4 <- combined_dat_full %>%
  ggplot(aes(date,n_tests, color = Variant))+
  geom_line()+
  labs(
    title = "Trend of Tests",
    y = "Tests",
    x = NULL
  )+
  theme_classic()

p5 <- combined_dat_full %>%
  ggplot(aes(date,retail, color = Variant))+
  geom_line()+
  labs(
    title = "Trend of Moblity",
    y = "Cases",
    x = NULL
  )+
  theme_classic()

cowplot::plot_grid(p1, p2, p3, p4,
                   labels = c('A', 'B', 'C', 'D'), label_size = 12)
unique(combined_dat_full$Variant)
combined_dat_full$Variant <- factor(combined_dat_full$Variant,
                                                            c("Alpha", "Beta",
                                                              "Delta", "Delta-Plus",
                                                              "Epsilon", "Gamma",
                                                              "Mu", "Omicron"))

combined_dat_full_use <- setDT(combined_dat_full)[!is.na(Variant)][Variant%in% c("Alpha","Delta", "Omicron")]
# do the analysis like they do --------------------------------------------
summary(combined_dat_full_use)

# SET THE DEFAULT ACTION FOR MISSING DATA TO na.exclude
# (MISSING EXCLUDED IN ESTIMATION BUT RE-INSERTED IN PREDICTION/RESIDUALS)
options(na.action="na.exclude")

# SCALE EXPOSURE
combined_dat_full_use$cases100 <- combined_dat_full_use$cases/100
combined_dat_full_use$logtests <- log(combined_dat_full_use$n_tests)

# GENERATE MONTH AND YEAR
combined_dat_full_use$month  <- as.factor(months(combined_dat_full_use$date))
combined_dat_full_use$year   <- as.factor(format(combined_dat_full_use$date, format="%Y") )
combined_dat_full_use$dow    <- as.factor(weekdays(combined_dat_full_use$date))
combined_dat_full_use$stratum <- as.factor(combined_dat_full_use$year:combined_dat_full_use$month:combined_dat_full_use$dow)

combined_dat_full_use <- combined_dat_full_use[order(combined_dat_full_use$date),]
combined_dat_full_use$tst <- rnorm(nrow(combined_dat_full_use))
# FIT A CONDITIONAL POISSON MODEL WITH A YEAR X MONTH X DOW STRATA
variant_cpr_death <- gnm(deaths_by_date ~ cases100 + Variant + retail,
                         data=subset(combined_dat_full_use,date > Sys.Date()-180), family=quasipoisson(),
                 eliminate=factor(stratum))
variant_cpr_icu <- gnm(icu ~ cases100 + Variant+ retail,
                       data=subset(combined_dat_full_use,date > Sys.Date()-180), family=quasipoisson(),
                   eliminate=factor(stratum))
variant_cpr_admit <- gnm(admits ~ cases100 + Variant+ retail ,
                         data=subset(combined_dat_full_use,date > Sys.Date()-180), family=quasipoisson(),
                       eliminate=factor(stratum))
variant_cpr_ed <- gnm(ed_visits ~ cases100 + Variant+ retail,
                      data=subset(combined_dat_full_use,date > Sys.Date()-180), family=quasipoisson(),
                         eliminate=factor(stratum))

fits <- list(variant_cpr_death,variant_cpr_admit,variant_cpr_ed,variant_cpr_icu)

results_large <- lapply(fits, function(x){

results <- cbind(as.data.table(se(variant_cpr_admit))#,
                 #as.data.table(confint(x))
                 )

names(results) <- c("estimate", "std_error")

results$parameter <- names(coef(x))

results

})

names(results_large) <- c("Death", "Admit", "ED", "ICU")

results_large <- rbindlist(results_large, idcol = "outcome")

results_large[,q025:=estimate-2*std_error]
results_large[,q975:=estimate+2*std_error]

results_large %>%
ggplot(aes(parameter))+
  geom_pointrange(aes(y = estimate, ymin = q025, ymax = q975))+
  coord_flip()+
  geom_hline(yintercept = 0, lty = "dashed")+
  theme_classic()+
  facet_wrap(~outcome)

# predict counterfactuals -------------------------------------------------


plot(predict(variant_cpr_ed))
start_dat <- subset(combined_dat,date > Sys.Date()-180)
pred_all_delta <- copy(start_dat)[,Variant:="Delta"]
pred_all_omicron <- copy(start_dat)[,Variant:="Omicron"]

pred_all_delta$pred <- predict(variant_cpr_ed, newdata=pred_all_delta,
                               type = "response")
pred_all_omicron$pred <- predict(variant_cpr_ed, newdata=pred_all_omicron,
                               type = "response")

new_out <- data.table(date = pred_all_delta$date,
                 actual = pred_all_delta$ed_visits,
                 delta = pred_all_delta$pred,
                 omicron = pred_all_omicron$pred)

new_out %>%
  melt(id.vars = "date") %>%
  ggplot(aes(date, value, color = variable))+
  geom_line()
