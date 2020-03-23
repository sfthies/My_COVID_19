###### Empirical Analysis of actual Covid-19 cases
# Make sure to download and update https://github.com/CSSEGISandData/COVID-19 before running.

#Set working directory:
setwd("C:/Users/Stephan/Documents/GitHub/My_COVID_19/")

#Set path for John Hopkins Github COVID 19 data:
jhu_data_path <- "C:/Users/Stephan/Documents/GitHub/COVID-19"

# Libaries ----------------------------------------------------------------
require(plotly)
require(tidyr)
require(dplyr)

# Functions: --------------------------------------------------------------

add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}



#### Read Data ####

## Read Data from JHU:
#Confirmed:
dat_con <- read.csv(paste0(jhu_data_path,"/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))
#Deaths:
dat_dea <- read.csv(paste0(jhu_data_path,"/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"))
#Recovered
dat_rec <- read.csv(paste0(jhu_data_path,"/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"))

##Bring into correct format:
#Confirmed:
dat_con_2 <- t(aggregate(dat_con[,-(1:4)], list(dat_con$Country.Region), FUN = sum))
colnames(dat_con_2) <- dat_con_2[1,]
dat_con_2 <- dat_con_2[-1,]
class(dat_con_2) <- 'numeric'
dat_con_2 <- as.data.frame(dat_con_2)
dat_con_2$Date <- as.Date(gsub("X", "",rownames(dat_con_2)), format = '%m.%d.%y')
rownames(dat_con_2) <- 1:nrow(dat_con_2)

#Deaths:
dat_dea_2 <- t(aggregate(dat_dea[,-(1:4)], list(dat_dea$Country.Region), FUN = sum))
colnames(dat_dea_2) <- dat_dea_2[1,]
dat_dea_2 <- dat_dea_2[-1,]
class(dat_dea_2) <- 'numeric'
dat_dea_2 <- as.data.frame(dat_dea_2)
dat_dea_2$Date <- as.Date(gsub("X", "",rownames(dat_dea_2)), format = '%m.%d.%y')
rownames(dat_dea_2) <- 1:nrow(dat_dea_2)

#Recovered:
dat_rec_2 <- t(aggregate(dat_rec[,-(1:4)], list(dat_rec$Country.Region), FUN = sum))
colnames(dat_rec_2) <- dat_rec_2[1,]
dat_rec_2 <- dat_rec_2[-1,]
class(dat_rec_2) <- 'numeric'
dat_rec_2 <- as.data.frame(dat_rec_2)
dat_rec_2$Date <- as.Date(gsub("X", "",rownames(dat_rec_2)), format = '%m.%d.%y')
rownames(dat_rec_2) <- 1:nrow(dat_rec_2)


# Visualize ---------------------------------------------------------------

## Select all countries with more than 100 reported cases
countries <- colnames(dat_con_2)[which(dat_con_2[nrow(dat_con_2),-ncol(dat_con_2)]>1000)]

#Confirmed:
plt <- plot_ly(x = ~Date, data = dat_con_2)
for(c in countries){
  plt <- plt %>% add_lines(y = dat_con_2[,c], name = c) 
}
plt %>% layout(yaxis = list(title = 'Confirmed cases'))
plt <- plt %>% layout(yaxis = list(title = 'Confirmed cases (log scale)', type = 'log'))
plt



# Breakpoint regressions --------------------------------------------------

# Run bi-sectional regressions with endogenous breakpoint

bi_sec_break<-function(ts_dat){
  log_ts_dat <- log(ts_dat)
  log_ts_dat[is.finite(log_ts_dat)==F]<-NA
  
  t<-1:length(ts_dat)
  
  l2<- function(t0){
    t_int <- t>=t0
    lm_temp <- lm(log_ts_dat~t+t*t_int)
    return(summary(lm_temp)$r.squared)
  }
  
  t0<-which.max(sapply(t, l2))
  return(t0)
}

#Determine breaks:
breaks<-apply(dat_con_2[,countries],2, function(x) bi_sec_break(x))

#Plot breakpoints:
par(mfrow= c(3,4), mar = c(4,4,.1,.1), mgp = c(2,1,0))
for(c in countries){
  plot(1:nrow(dat_con_2), dat_con_2[,c], ylab = c, xlab = 'Day', log = "y")
  abline(v = breaks[c], col = 2, lty = 2)
}

#Run regression to estimate growth rate in every country:

est_growth <- sapply(countries, function(c){
  temp_dat<-log(dat_con_2[breaks[c]:nrow(dat_con_2),c])
  temp_t<-1:length(temp_dat)
  summary(lm(temp_dat~temp_t))$coef[2,1:2]
  })


est_growth <- as.data.frame(t(est_growth)) 

est_growth[,'Country']<-row.names(est_growth)

plot_ly(y = ~(exp(Estimate)-1) , x = ~Country, data = est_growth, type = 'bar', error_y = ~list(array = (est_growth$`Std. Error`*exp(est_growth$Estimate)^2),
                                                                                       color = 'black')) %>% 
  layout(yaxis = list(title = 'Estimated growth rate'))


est_growth[,'Double'] <- log(2)/log(1+est_growth$Estimate)

est_growth[,'Ten'] <- log(10)/log(1+est_growth$Estimate)

plot_ly(y = ~Double , x = ~Country, data = est_growth, type = 'bar') %>% 
  layout(yaxis = list(title = 'Estimated days to double'))

plot_ly(y = ~Ten , x = ~Country, data = est_growth, type = 'bar') %>% 
  layout(yaxis = list(title = 'Estimated days to increase by ten times'))


# Estimate infection parameters ------------------------------------------
par(mfrow=c(1,1))

## Estimate average time until death or recovery

# Theoretically the mean time from reporting to death or recovery (tau) can be obtained by
# minimizing the squared error in the lagged functions:

# min_tau sum_(t=1)^(T-tau) (I_t - R_(t+tau) - D_(t+tau))^2

#Function takes t
error_fun<-function(tau, c){
  e<-dat_con_2[1:(nrow(dat_con_2)-tau),c]  -(dat_dea_2[(tau+1):(nrow(dat_dea_2)),c]+dat_rec_2[(tau+1):(nrow(dat_rec_2)),c] )
  sum(e^2)/(nrow(dat_con_2))
}

#Obtain least square estimate for single countries:
c <- 'Germany'
sapply(countries, function(c) which.min(sapply(1:30, function(x) error_fun(x,c))))

#Obtain least square estimate across countries:
which.min(sapply(1:30, function(x) sum(sapply(countries, function(c) error_fun(x, c)))))


## Estimate recovery time or time to death seperately
error_fun_sep<-function(tau1, tau2, c){
  tau<-max(tau1, tau2)
  e<-dat_con_2[1:(nrow(dat_con_2)-tau),c]  -(dat_dea_2[1:(nrow(dat_con_2)-tau)+tau2,c]+dat_rec_2[1:(nrow(dat_con_2)-tau)+tau1,c] )
  sum(e^2)/(nrow(dat_con_2))
}

#Doesnt work too well:
sapply(countries, function(c){
       temp<-sapply(5:25, function(y) sapply(5:25, function(x) error_fun_sep(x,y,c)))
       taus<-which(temp == min(temp), arr.ind = T)
       colnames(taus)<-c('tau1','tau2')
       return(taus)}
       )


## Estimate R0:

c<-'Germany'

#newly infected per day
(Tstart<-breaks[c])
(Tmax<-nrow(dat_con_2))
is<-diff(dat_con_2[Tstart:Tmax,c])

#Different R0 depending on tau:
res<-rep(NA, length(is))

for(tau in 1:length(is)){
  alpha_ps <- sapply(1:(length(is)-tau), function(t) is[tau+t]/sum(is[1:tau+t-1]))
  alpha_p <-mean(alpha_ps)
  res[tau]<-mean(alpha_p*tau)
}

#Different resulting R0:
res

#select one tau:
tau<-12

#And calculate respective alpha_p
alpha_p<-res[12]/12

#Evaluate fit
fitted<-is
fitted[(tau+1): length(is)]<-NA

for(t in 1:(length(is)-tau)){
fitted[tau+t]<-sum(fitted[1:tau+t-1])*alpha_p
}

plot(is)
lines(fitted, lty = 2)


# Estimate fatality -------------------------------------------------------
c <- 'Germany'
tau<-15
Tmax<-nrow(dat_con_2)

fatals<-dat_dea_2[tau:Tmax,c]/dat_con_2[1:(Tmax-tau+1),c]

#Resulting fatality rate:
mean(fatals[fatals>0], na.rm = T)


