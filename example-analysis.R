library(data.table)
library(ggplot2)
library(dplyr)
library(haven)
library(gnm)

# read in the data --------------------------------------------------------

hh <- here::here

data <- read_dta(hh("data-raw", "londondataset2002_2006.dta"))


p1 <- data %>%
  ggplot(aes(date,numdeaths))+
  geom_line()+
  labs(
    title = "Trend of Deaths",
    y = "Number of Deaths",
    x = NULL
  )+
  theme_classic()
p2 <- data %>%
  ggplot(aes(date,ozone))+
  geom_line()+
  labs(
    title = "Trend of Ozone Level",
    y = "Ozone Level",
    x = NULL
  )+
  theme_classic()

cowplot::plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12)


# do the analysis like they do --------------------------------------------
summary(data)

# SET THE DEFAULT ACTION FOR MISSING DATA TO na.exclude
# (MISSING EXCLUDED IN ESTIMATION BUT RE-INSERTED IN PREDICTION/RESIDUALS)
options(na.action="na.exclude")

# SCALE EXPOSURE
data$ozone10 <- data$ozone/10

# GENERATE MONTH AND YEAR
data$month  <- as.factor(months(data$date))
data$year   <- as.factor(format(data$date, format="%Y") )
data$dow    <- as.factor(weekdays(data$date))
data$stratum <- as.factor(data$year:data$month:data$dow)

data <- data[order(data$date),]

# FIT A CONDITIONAL POISSON MODEL WITH A YEAR X MONTH X DOW STRATA
modelcpr1 <- gnm(numdeaths ~ ozone10 + temperature, data=data, family=poisson,
                 eliminate=factor(stratum))
summary(modelcpr1)

# ALLOW FOR OVERDISPERSION
modelcpr2 <- gnm(numdeaths ~ ozone10 + temperature , data=data, family=quasipoisson,
                 eliminate=factor(stratum)  )
summary(modelcpr2) # ANTONIO - summary NOT WORKING ON MY PC

# ADD BRUMBACK AUTOCORRELATION ADJUSTMENT
library(tsModel)   # FACILITATES GETTING LAGGED VALUES'
reslag1 <- Lag(residuals(modelcpr1,type="deviance"),1)
modelcpr3 <- gnm(numdeaths ~ ozone10 +  temperature + reslag1, data=data,
                 family=quasipoisson, eliminate=factor(stratum))
summary(modelcpr3)

# ALLOW FOR AUTOCORRELATION AND OVERDISPERSION
library(tsModel)   # FACILITATES GETTING LAGGED VALUES'
reslag1 <- Lag(residuals(modelcpr1,type="deviance"),1)
modelcpr4 <- gnm(numdeaths ~ ozone10 +  temperature + reslag1, data=data,
                 family=quasipoisson, eliminate=factor(stratum))
summary(modelcpr4)

# ILLUSTRATION OF ALLOWING FOR VARYING RATE DENOMINATORS
#  FOR THIS WE HAVE IMAGINED AVAILABILITY OF A RELEVANT POPULATION MEASURE CHANGING
#  AT SHORT TIME SCALES
data$population <- 3000000
logpop <- log(data$population)
modelcpr5 <- gnm(numdeaths ~ ozone10 + temperature, data=data, family=poisson , offset=logpop,
                 eliminate=factor(stratum))
summary(modelcpr5)

# FURTHER CODE FOR THE UNCONDITIONAL POISSON AND CONDITIONAL LOGISTIC (CASE CROSSOVER)
#  ANALYSES REPORTED IN THE TEXT

#  FIT UNCONDITIONAL POISSON MODEL
model_upr <- glm(numdeaths ~ ozone10  + temperature + factor(stratum),data=data,family=poisson)
summary(model_upr)

# FIT CONDITIONAL LOGISTIC MODEL

# EXPAND THE DATA IN A CASE-CROSSOVER FORMAT (AND EXCLUDE STRATA WITH 0)
#  REQUIRED FUNCTION
funccmake <-  function(date,cases,vars=NULL,dow) {
  #
  # DERIVE STRATUM VARIABLES
  if(missing(dow)) dow <- ifelse(class(date)=="Date",TRUE,FALSE)
  if(class(date)=="Date") {
    day <- if(dow) weekdays(date) else rep(1,length(date))
    month <- months(date)
    year <- format(date, format="%Y")
  } else {
    day <- rep(1,length(date))
    month <- date
    year <- rep(1,length(date))
    if(dow) stop("'dow' only available when 'date' is a date")
  }
  #
  # DERIVE INDEXING VARIABLES
  gfactor <- factor(day):factor(month):factor(year)
  gnumber <- match(gfactor,unique(gfactor))
  gindex <- lapply(1:length(date),
                   function(x) (1:length(date))[gnumber%in%gnumber[x]])
  gstatus <- lapply(1:length(date), function(x) gindex[[x]]==x)
  #
  # EXPAND PREDICTORS
  if(!is.null(vars)) {
    varnames <- if(is.vector(vars)) deparse(substitute(vars)) else names(vars)
    vars <- as.matrix(vars)
    dimnames(vars) <- list(NULL,varnames)
  }
  #
  # RESULTS
  res <- data.frame(
    index=unlist(gindex),
    status=unlist(gstatus)+0,
    stratum=rep(1:length(date),sapply(gindex,length)),
    weights=rep(cases,sapply(gindex,length))
  )
  if(!is.null(vars)) res <- cbind(res,vars[res$index,])
  #
  return(res)
}

dataexp <- funccmake(data$stratum,data$numdeaths,vars=cbind(data$ozone10,data$temperature ))
dataexp <- dataexp[dataexp$weights>0,]
Xexp <- as.matrix(dataexp)[,-seq(4)]

# RUN CLR
library(survival)
timeout <- as.numeric(factor(dataexp$stratum))
timein <- timeout-0.1
model_clr <- coxph(Surv(timein,timeout,status) ~ Xexp, weights=weights, dataexp)
summary(model_clr)

f <- gnm(case ~ spontaneous + induced, data=infert,
   family=quasipoisson, eliminate=factor(stratum), subset =  parity <= 2)
library(rstanarm)

dat <- infert[order(infert$stratum), ] # order by strata
post <- stan_clogit(case ~ spontaneous + induced + (1 | education),
                    strata = stratum,
                    data = dat,
                    subset = parity <= 2,
                    QR = TRUE,
                    chains = 2, iter = 500)
summary(post)
summary(f)
