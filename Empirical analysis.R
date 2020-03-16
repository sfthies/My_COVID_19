###### Empirical Analysis of actual Covid-19 cases

#Set working directory:
setwd("C:/Users/Stephan/Documents/GitHub/My_COVID_19/")

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
dat_con <- read.csv("../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
#Deaths:
dat_dea <- read.csv("../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
#Recovered
dat_rec <- read.csv("../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

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

plot_ly(y = ~Estimate , x = ~Country, data = est_growth, type = 'bar', error_y = ~list(array = est_growth$`Std. Error`,
                                                                                       color = 'black')) %>% 
  layout(yaxis = list(title = 'Estimated growth rate'))


est_growth[,'Double'] <- log(2)/log(1+est_growth$Estimate)

plot_ly(y = ~Double , x = ~Country, data = est_growth, type = 'bar') %>% 
  layout(yaxis = list(title = 'Estimated days to double'))




