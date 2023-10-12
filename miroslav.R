#library(kronos)
library(tidyverse)
library(openxlsx)
library(zoo)
library(dplyr)
library(lubridate)

full_data <- arrow::read_parquet("D:/Davor/Documents/posao/working2/mph2_analyses/mph2_logs/miroslav/miro_mph2_tall_1m_noNaN.parquet")
full_data <- na.locf(full_data, na.rm = FALSE, fromLast = TRUE)

#### get NaN sequences ####
rle_animals <- data.frame()
for (animal in unique(full_data$animal_id)) {
  data <- full_data %>% filter(animal_id == animal) %>% arrange(ts_sent)
  #rle_data<-rle(is.na(data$miro_value))
  rle_data <- rle(data$miro_value == 1)
  end <- cumsum(rle_data$lengths)
  start <- c(1, lag(end)[-1] + 1)
  rle_data <- data.frame(lengths = rle_data$lengths,
                         values = rle_data$values,
                         end = cumsum(rle_data$lengths),
                         start = c(1, lag(end)[-1] + 1)) 
  # for (row in 1:nrow(rle_data)) {
  #   data[rle_data[row,]$start:rle_data[row,]$end, "rle_length"] <- rle_data[row,]$length
  #   data[rle_data[row,]$start:rle_data[row,]$end, "rle_value"] <- rle_data[row,]$values
  #   data[rle_data[row,]$start:rle_data[row,]$end, "rle_start"] <- rle_data[row,]$start
  #   data[rle_data[row,]$start:rle_data[row,]$end, "rle_end"] <- rle_data[row,]$end
  # }
  rle_data$group <- data$group[rle_data$start]
  rle_data$animal_id <- data$animal_id[rle_data$start]
  rle_data$ts_start <- data$ts_sent[rle_data$start]
  rle_data$ts_end <- data$ts_sent[rle_data$end]
  rle_data$sensor_type <- data$sensor_type[rle_data$start]
  rle_animals <- rbind(rle_animals, rle_data)
  #rle_data_ones <- rle_data %>% filter(values == TRUE) %>% arrange(desc(lengths))
}

rle_animals <- rle_animals %>% filter(values == T) %>% arrange(desc(lengths))
rle_animals$date <- as.Date(rle_animals$ts_start)
# TO DO: histogram of rle dates
ggplot(rle_animals %>% filter(lengths>5) %>% filter(lengths<60), aes(x=date))+
  geom_histogram(bins=55)+
  scale_x_continuous(breaks = seq(min(rle_animals$date), max(rle_animals$date), by=1))+
  theme(axis.text.x=element_text(angle=90))

lijevi_mirek <- c()
desni_mirek <- c()
for (color in c("zeleni", "plavi", "crni", "crveni")) {
  for (number in c(1:5)) {
    animal <- paste0(color, number)
    lijevi_mirek <- append(lijevi_mirek, c(animal))
  }
  for (number in c(6:10)) {
    animal <- paste0(color, number)
    desni_mirek <- append(desni_mirek, c(animal))
  }
}



# Display results
rle_data <- data.frame(start, end)
rle_data$length <- rle_data$end-rle_data$start
rle_data <- rle_data %>% arrange(desc(length))
rle_data$animal_id <- rle_data$start

rle_data$start <- data$ts_sent[rle_data$start]
rle_data$end <- data$ts_sent[rle_data$end]
rle_data$animal_id <- data$animal_id[rle_data$animal_id]
trimmed_rle_data <- rle_data %>% filter(length >= 5) %>% arrange(start)
write.xlsx(trimmed_rle_data, "D:/Davor/Documents/posao/working2/mph2_analyses/mph2_logs/miroslav/nanovi_4h.xlsx")

#####

#data <- data %>% filter(after_stz == 1)
data <- data %>% filter(sensor_type == "H")
data$after_stz <- as.factor(data$after_stz)



## this
#data$n_week <- (data$ts_sent-min(data$ts_sent))%/%ddays(7)+1
## or the following 3 lines
starttime <- as_datetime("2023-02-06T06:00:00")
full_data$n_week <- (full_data$ts_sent-starttime)%/%ddays(7)+1
full_data <- full_data %>% filter(ts_sent > starttime) %>% arrange(ts_sent)
full_data <- full_data %>% filter(sensor_type == "H")
full_data_lijevi <- full_data %>% filter(animal_id %in% lijevi_mirek)
full_data_desni <- full_data %>% filter(animal_id %in% desni_mirek)
ggplot(full_data_desni, aes(x=miro_value, color=group))+
  geom_histogram(bins=100)+
  #scale_x_continuous(breaks = seq(min(rle_animals$date), max(rle_animals$date), by=1))+
  theme(axis.text.x=element_text(angle=90))

# ggplot(full_data_lijevi, aes(x=ts_sent, y=miro_value, color=group)) +
#   geom_point() +
#   theme_bw()+
#   facet_wrap(~n_week, scales="free_x")

df_summary <- full_data_desni %>% 
  thicken('4 hour') %>%   ##pad to 4 hr
  #mutate(ts_sent = cut(ts_sent, "4 hour")) %>%   ##create new 'five_min' column
  group_by(ts_sent_4_hour, group, sensor_type, n_week) %>%     ## group by the new col
  summarise(miro_value = mean(miro_value, na.rm=FALSE))  ##aggregate the new sum
df_summary$hour <- hour(df_summary$ts_sent_4_hour)
df_summary$hour_shade <- ifelse(df_summary$hour >= 18 | df_summary$hour <= 6, "gray60", "gray90")
ggplot(df_summary, aes(ts_sent_4_hour, miro_value, colour=group)) + 
  geom_smooth(span=0.1) + 
  geom_point() + 
  #geom_errorbar(aes(ymin=mean_event_count-se_event_count, ymax=mean_event_count+se_event_count))+
  scale_x_datetime(
    date_minor_breaks = "6 hours", date_breaks = "12 hours",
    date_labels = "%H",
    guide = guide_axis(n.dodge = 3))+
  facet_wrap(~n_week, scales="free")+
  geom_rect(aes(xmin = ts_sent_4_hour, xmax = lead(ts_sent_4_hour), ymin = -Inf, ymax = Inf, fill = hour_shade, color = hour_shade),
            linetype=0, alpha=0.3)

# Group by 'timestamp', 'treatment', and 'n_week' to calculate means and standard errors
df_summary <- df_summary %>%
  filter(!is.na(miro_value)) %>% 
  #filter(group %in% c("ctr", "stz")) %>% 
  group_by(ts_sent, group, n_week) %>%
  summarize(
    mean_event_count = mean(miro_value, na.rm = FALSE),
    se_event_count = sd(miro_value, na.rm = FALSE) / sqrt(n())
  ) %>%
  ungroup()  # Remove grouping
df_summary$hour_of_day <- hour(df_summary$ts_sent)
df_summary$ts_sent %>% as.POSIXlt()

ggplot(df_summary, aes(ts_sent, mean_event_count, colour=group)) + 
  geom_line() + 
  geom_point() + 
  #geom_errorbar(aes(ymin=mean_event_count-se_event_count, ymax=mean_event_count+se_event_count))+
  scale_x_datetime(
    date_minor_breaks = "6 hours", date_breaks = "12 hours",
    date_labels = "%H",
    guide = guide_axis(n.dodge = 3))+
  facet_wrap(~n_week, scales="free")

# get hour in day
data$hour <- hour(data$ts_sent)
# get day as ordinal
data$day <- date(data$ts_sent)-(min(date(data$ts_sent))-1)

#for (i_day in sort(unique(data$day))) {
data_oneday <- data %>% filter(day %in% c(10:15))
ggplot(data = data_oneday, aes(x = ts_sent,  y = miro_value, color = group)) +
#  geom_point() +
  geom_smooth(aes(group=group), method="loess", span=0.05, na.rm=TRUE)
#  geom_smooth(method = "lm", formula = y ~ x + sin(x) + cos(x))
  #geom_smooth(method = "loess", span = 0.5)+
  #facet_wrap(~day)
  #ggtitle(paste("day", i_day))

output <- kronos(formula = miro_value ~ group+time(hour), 
                 data = data, 
                 period = 6, 
                 verbose = T, 
                 pairwise = T)
