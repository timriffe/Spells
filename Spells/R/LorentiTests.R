library(here)
library(tidyverse)
library(markovchain)
library(reshape2)
library(Spells)
# 0) load functions

# need to source LorentiFunctions.R and LorentiPrep.R to do this
Dat <- readRDS(here::here("Spells","Data","Lorenti","SILCsim.rds"))
# now this is tidy and can be analyzed
# head(Dat)

DisStats <- 
  Dat %>% 
  # remove age 80, since we closed out
  filter(age < 80) %>% 
  group_by(InQ, id) %>% 
  # say we want average duration of disability
  # * spells starting in age x
  # * spells ending in age x
  # avg duration of nth spells by age.
  mutate(dis_dur = clock(state, 
                         state = "Disabled",
                         clock_type = "duration"),
         dis_order = clock(state, 
                           state = "Disabled",
                           clock_type = "order",
                           increasing = TRUE)) %>% 
  group_by(InQ, id, dis_order) %>% 
  mutate(first = row_number() == 1L,
         last = row_number() == n(),
         first = ifelse(state == "Disabled" & 
                          max(age) == 79, FALSE, first),
         last = ifelse(state == "Disabled" & 
                          max(age) == 79, FALSE, last)) 
 
    
# mean spell duration for spells
# starting in age x
DisStats %>% 
  filter(first,
         !is.na(dis_dur)) %>% 
  group_by(sex, InQ, age) %>% 
  summarize(dur_first_mean = mean(dis_dur, na.rm = TRUE)) %>% 
  mutate(dur_first_mean = na_if(dur_first_mean, NaN)) %>% 
  ggplot(mapping = aes(x = age, 
                       y = dur_first_mean, 
                       color = InQ)) + 
  geom_line(size=2) + 
  xlim(16,70) +
  labs(x = "Age", y = "mean spell duration",
       main = "Mean disability spell duration of spells starting in age x")

# mean spell duration for spells
# ending in age x
DisStats %>% 
  filter(last,
         !is.na(dis_dur)) %>% 
  group_by(sex, InQ, age) %>% 
  summarize(dur_last_mean = mean(dis_dur, na.rm = TRUE)) %>% 
  mutate(dur_last_mean = na_if(dur_last_mean, NaN)) %>% 
  ggplot(mapping = aes(x = age, 
                       y = dur_last_mean, 
                       color = InQ)) + 
  geom_line(size=2) + 
  xlim(17,80)
  

library(ggridges)
Dat %>% 
  # remove age 80, since we closed out
  filter(age < 80,
         state != "Dead") %>% 
  group_by(InQ, id) %>% 
  mutate(ttd = max(age) - age,
         ad5 = max(age) - max(age) %% 5) %>% 
  ungroup() %>% 
  filter(ad5 > 30) %>% 
  group_by(sex, InQ, ttd, ad5) %>% 
  summarize(ttdprev = mean(state == "Disabled")) %>% 
  ggplot(mapping = aes(x = ttd, 
                       y = as.factor(ad5), 
                       color = InQ,
                       height = ttdprev)) + 
  geom_density_ridges(stat = "identity",fill = "transparent") 


# mean spell order of spells
# starting in age x
DisStats %>% 
  filter(first,
         !is.na(dis_dur)) %>% 
  group_by(sex, InQ, age) %>% 
  summarize(order_first_mean = mean(dis_order, na.rm = TRUE)) %>% 
  mutate(order_first_mean = na_if(order_first_mean, NaN)) %>% 
  ggplot(mapping = aes(x = age, 
                       y = order_first_mean, 
                       color = InQ)) + 
  geom_line() + 
  xlim(16,70) + 
  geom_segment(aes(x=54,y=2,xend=65,yend=2),color = "black") +
  annotate("text", x = 58, y = 2.05, label = "11 years") +
  labs(x = "Age", 
       y = "mean episode order",
       main = "New disability episodes are on average 2nd episodes by age 54 if you're poor, 65 if you're rich")
  



