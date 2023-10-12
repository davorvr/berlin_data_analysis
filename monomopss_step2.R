library(tidyverse)
library(readxl)
library(lme4)
#library(padr)

data <- arrow::read_parquet("data_clean.parquet")
data

# this part helps me identify the large gaps in the data
# diff gets the gaps between two timepoints
#data$difftime <- c(NA, diff(data$timestamp))
# sorting this gives the rows where a gap ENDS
#data %>% arrange(desc(difftime))
# still not doing anything with this information

# column renaming
data <- data %>% rename(timestamp = V2,
                        animal_id = V3,
                        timestamp_millis = V4,
                        event_type = V5)
# convert timestamp column from integers to datetime
data$timestamp <- as_datetime(data$timestamp, tz="Europe/Berlin")
# this cuts off the data at the first start of a light period. night in this case
starttime <- as_datetime("2020-11-30 20:00:00", tz="Europe/Berlin")
data <- data[data$timestamp > starttime, ]

#### convert event to period data ####
# this code changes the data format so the timestamp column represents
# equidistant points. it turns a list of events into a list of event counts
# for every hourly period in the data range.

# get event counts for every minute (or other arbitrary time bin) for every animal.
# hours with zero events# aren't present in this dataframe: so-called "sparse data
# structure"
time_bin_size = "15 min"
binned_counts <- data %>%
  mutate(time_bin = floor_date(timestamp, unit = time_bin_size)) %>%
  group_by(animal_id, time_bin) %>%
  count(name="event_count")
binned_counts$time_bin <- as_datetime(binned_counts$time_bin, tz="Europe/Berlin")

data_counts <- binned_counts %>% group_by(animal_id) %>% 
  complete(time_bin = seq(starttime, max(binned_counts$time_bin), time_bin_size), fill = list(event_count = 0))
data_counts <- data_counts %>% arrange(time_bin)

data_counts <- data_counts %>% rename(timestamp = time_bin)
data_counts <- ungroup(data_counts)

##### load metadata and add cols ####

# add light/dark column
data_counts <- data_counts %>% mutate(phase = case_when(((hour(data_counts$timestamp) >= 20) | (hour(data_counts$timestamp) < 8)) ~ "dark",
                                                        TRUE ~ "light"))
# read metadata
animal_ids <- read_excel("animal_ids.xlsx")
result_df <- merge(data_counts, animal_ids, by = "animal_id", all.x = TRUE)
df <- result_df %>% subset(select=-color_code)
df <- df %>% arrange(timestamp)
# add variables with week/day/whatever counts. useful for facet_wrap() later on,
# and performing operations on specific days, weeks, or other chunks of data.
df$n_week <- (df$timestamp-starttime)%/%ddays(7)+1
df$n_12hr <- (df$timestamp-starttime)%/%dhours(12)+1
df$n_24hr <- (df$timestamp-starttime)%/%dhours(24)+1
df$n_15min <- (df$timestamp-starttime)%/%dminutes(15)+1
df$n_1min <- (df$timestamp-starttime)%/%dminutes(1)

##### sine per day per group ####
# let's give it a shot - one sine curve per group for each day
sine_df <- data.frame(Day=integer(),
                      Group=character(),
                      Amplitude=double(),
                      Acrophase=double(),
                      MESOR=double())
oneday <- df %>% filter(n_24hr == min(df$n_24hr))
period <- length(unique(oneday$timestamp))

for (day in unique(df$n_24hr)) {
  m <- lm(event_count ~ sin(2*pi*n_1min)+cos(2*pi*n_1min), data = df %>% filter(n_24hr == day))
  
}

oneday <- df %>% filter(n_24hr == 133) 
#oneday$event_ratio <- oneday$event_count/sum(oneday$event_count)
oneday <- oneday %>% filter(treatment == "C")

#oneday$n_1min_in_day <- (oneday$timestamp-min(oneday$timestamp))%/%dminutes(1)+1
oneday$sin <- sin(2*pi*(oneday$n_1min%%period/period))
oneday$cos <- cos(2*pi*(oneday$n_1min%%period/period))
oneday$animal_id <- as.factor(oneday$animal_id)
#oneday$log_event_count <- log(oneday$event_count+1)
m <- glmmTMB(event_count ~ sin+cos+animal_id+sin*animal_id+cos*animal_id, data = oneday)
#m <- lmer(event_count ~ sin+cos+(1|animal_id) + (0+sin|animal_id) + (0+cos|animal_id), data = oneday)
#par(mfrow=c(2,2))
plot(m)

# distribution fitting - https://stats.stackexchange.com/questions/132652/how-to-determine-which-distribution-fits-my-data-best
library(fitdistrplus)
library(logspline)
descdist(oneday$event_count, discrete = TRUE)
m.fit <- fitdist(oneday$event_count, "nbinom")
n.sims <- 1e3

stats <- replicate(n.sims, {      
  r <- rnbinom(n = length(oneday$event_count)
               , size= m.fit$estimate["size"]
               , mu = m.fit$estimate["mu"]
  )
  estfit.nbinom <- fitdist(r, "nbinom") # added to account for the estimated parameters
  as.numeric(ks.test(r
                     , "pnbinom"
                     , size = estfit.nbinom$estimate["size"]
                     , mu = estfit.nbinom$estimate["mu"])$statistic
  )      
})
plot(ecdf(stats), las = 1, main = "KS-test statistic simulation (CDF)", col = "darkorange", lwd = 1.7)
grid()
fit <- logspline(stats)

1 - plogspline(ks.test(oneday$event_count
                       , "pnbinom"
                       , size = m.fit$estimate["size"]
                       , mu = m.fit$estimate["mu"])$statistic
               , fit
)

library(gamlss)
library(gamlss.dist)
library(gamlss.add)

fit <- fitDist(df$event_count, k = 2, type = "counts", trace = FALSE, try.gamlss = TRUE, parallel = "multicore", ncpus=8)

#####

#library(openxlsx)
#for (week in unique(df$n_week)) {
#  oneweek <- df %>% filter(n_week == week)
#  fname <- paste("D:/Davor/Documents/posao/working2/monomopss/monomopss_binned_1min_week", toString(week), ".xlsx", sep="")
#  write.xlsx(oneweek, fname)
#}

# this introduces a light/dark column
#data <- data %>% mutate(phase = case_when(((hour(data$timestamp) >= 20) | (hour(data$timestamp) < 8)) ~ "dark",
#                                          TRUE ~ "light"))

# remove weeks 7-14 because they have virtually no data
df <- df %>% filter(!n_week %in% 7:14)

##### cosinoRmixedeffects ####

library(cosinoRmixedeffects)
require(emmeans)
require(lme4)
require(ggplot2)
require(reshape2)
require(dplyr)
require(limma)

data(db.cosinor)
head(db.cosinor)
db.model<-create.cosinor.param(time="Hour_of_Day", period=24, data=db.cosinor)
df.model<-create.cosinor.param(time="n_15min", period=96, data=df)
## the number of participants with COVID+ and COVID-
table(db.model[!duplicated(db.model$participant_id), c("T0toT14")])
## the number of male and female participants
table(db.model[!duplicated(db.model$participant_id), c("gender")])
## the number of normal weight, overweight and obese participants
table(db.model[!duplicated(db.model$participant_id), c("bmi_baseline_cat")])

## Fit Model 1
f1<-fit.cosinor.mixed(y="hrv",x="gender",random="1|participant_id", data=db.model)
m1<-fit.cosinor.mixed(y="event_count",x="treatment",random="1|animal_id", data=df.model)

db.means<-get.means.ci.cosinor(f1, contrast.frm="~gender",nsim=500)
db.means
## the MEAN is the EMM, with 95% confidence interval lower boundary as "2.5%" and upper boundary as "97.5%" columns. 

df.means<-get.means.ci.cosinor(m1, contrast.frm="~treatment",nsim=500)
df.means

##### plots ####

df_1week_c <- df_1week %>% filter(animal_id == "900_200000631325")
ssp <- spectrum(df_1week_c$event_count)
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]

df_1week <- df %>% filter(n_week == 5)
df_1week$n_hour <- (df_1week$timestamp-min(df_1week$timestamp))%/%dhours(1)+1
df_1week$cos <- cos((2*pi*df_1week$n_hour)/24)
df_1week$sin <- sin((2*pi*df_1week$n_hour)/24)

reslm <- lmer(event_count ~ sin(2*pi/24*n_hour)+cos(2*pi/24*n_hour)+(1|animal_id)+treatment, data=df_1week)
reslm2 <- lmer(event_count~ 1 + cos + sin + treatment + sin*treatment + cos*treatment + (1|animal_id) + (0+sin|animal_id) + (0+cos|animal_id), data=df_1week)
reslm2 <- lmer(event_count~ 1 + cos + sin + treatment+ sin*treatment + cos*treatment + (1|animal_id) + (0+sin+cos|animal_id), data=df_1week)
ggplot(df_1week, aes(x=timestamp, y=event_count, group=treatment, color=treatment)) +
  geom_smooth(span=0.2) +
  geom_point()+
  theme_bw()
  #facet_wrap(~n_24hr, scales="free_x")





# Group by 'timestamp', 'treatment', and 'n_week' to calculate means and standard errors
df_summary <- df %>%
  group_by(timestamp, treatment, n_week) %>%
  summarize(
    mean_event_count = mean(event_count),
    se_event_count = sd(event_count) / sqrt(n())
  ) %>%
  ungroup()  # Remove grouping

df_summary_1week <- df_summary %>% filter(n_week == 5)
ggplot(df_summary, aes(timestamp, mean_event_count, colour=treatment)) + 
  #geom_line() + 
  geom_point() + 
  geom_errorbar(aes(ymin=mean_event_count-se_event_count, ymax=mean_event_count+se_event_count))+
  facet_wrap(~n_week, scales="free")

#################### playing around with various plots below ####

# Create the plot
ggplot(df_summary, aes(x = timestamp, y = mean_event_count, color = treatment)) +
  geom_point() +  # Points for the means
  geom_errorbar(aes(ymin = mean_event_count - se_event_count, ymax = mean_event_count + se_event_count), width = 0.2) +  # Error bars
  theme_bw() +
  facet_wrap(~n_week, scales = "free") +
  labs(
    x = "Timestamp",
    y = "Mean Event Count",
    color = "Treatment"
  ) +
  ggtitle("Time Series Plot with Treatment Group Means and Error Bars")

df_animalmean <- result_df %>%
  group_by(timestamp) %>%
  summarize(mean_animal = mean(event_count))

df_animalmean$n_week <- (df_animalmean$timestamp-starttime)%/%ddays(7)

#ggplot(df_animalmean[df_animalmean$timestamp < as_datetime("2020-12-30 20:00:00", tz="Europe/Berlin"),], aes(x=timestamp, y=mean_animal)) +
ggplot(df_animalmean, aes(x=timestamp, y=mean_animal)) +
  #geom_line( color="steelblue") + 
  geom_point() +
  xlab("") +
  theme_bw()+
  facet_wrap(~n_week, scales="free_x")