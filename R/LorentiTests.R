library(here)
library(tidyverse)
library(markovchain)
library(reshape2)
library(Spells)
# 0) load functions

# need to source LorentiFunctions.R and LorentiPrep.R to do this


# now this is tidy and can be analyzed
# head(Dat)
Dat <- readRDS(here::here("Spells","Data","Lorenti","SILCsim.rds"))

# TR: note also for all measures used here, we want to make sure right censoring doesn't mess up
# stats. Those that die in the interval are not right censored, so they're safe. If results too
# noisy due to not enough deaths, then increase N.
DisStats <- 
  Dat %>% 
  filter(age < 80) %>% 
  group_by(InQ, id) %>% 
  mutate(dead = ifelse(any(state == "Dead"),TRUE,FALSE)) %>% 
  ungroup() %>% 
  filter(#dead,
         state != "Dead") %>% 

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

# Macro 1
# mean spell duration for spells
# starting in age x
App1_macro1 <- 
  DisStats %>% 
  ungroup() %>% 
  mutate(InQ = case_when(InQ == "I" ~ "lowest 20%",
                         InQ == "V" ~ "highest 20%")) %>% 
  filter(state != "Dead", 
         first,
         !is.na(dis_dur)) %>% 
  group_by(sex, InQ, age) %>% 
  summarize(dur_first_mean = mean(dis_dur, na.rm = TRUE)) %>% 
  mutate(dur_first_mean = na_if(dur_first_mean, NaN)) %>% 
  ggplot(mapping = aes(x = age, 
                       y = dur_first_mean, 
                       color = InQ)) + 
  geom_line(size=1.5) + 
  geom_segment(aes(x=53.5,y=2,xend=66,yend=2),color = "black") +
  annotate("text", x = 58, y = 2.05, label = "13 years") + 
  annotate("text", x = 40, y = 1.68, label = "lowest 20%") +
  annotate("text", x = 40, y = 1.17, label = "highest 20%") +
  xlim(16,70) +
  labs(x = "Age", y = "conditional mean spell duration (years)",
       main = "Mean disability spell duration of spells starting in age x")+
  guides(color=FALSE)+
  theme(
    axis.title.x = element_text(size = 13), # use 16 for presentations
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 13))
ggsave("/home/tim/workspace/Spells/Spells/Figures/App1_macro1.pdf",
       App1_macro1, width = 7, height = 6)

# library(reshape2)
# X <- DisStats %>% 
#   filter(state != "Dead") %>% 
#   group_by(InQ, id) %>% 
#   filter(first,
#          !is.na(dis_dur),
#          InQ %in% c("I","V")) %>% 
#   group_by(sex, InQ, age) %>% 
#   summarize(dur_first_mean = mean(dis_dur, na.rm = TRUE)) %>% 
#   mutate(dur_first_mean = na_if(dur_first_mean, NaN)) %>% 
#   acast(age~InQ, value.var = "dur_first_mean")
# X[,1] / X[,2]  
# mean spell duration for spells
# ending in age x
# DisStats %>% 
#   filter(first,
#          !is.na(dis_dur)) %>% 
#   group_by(sex, InQ, age) %>% 
#   summarize(dur_last_mean = mean(dis_dur, na.rm = TRUE)) %>% 
#   mutate(dur_last_mean = na_if(dur_last_mean, NaN)) %>% 
#   ggplot(mapping = aes(x = age, 
#                        y = dur_last_mean, 
#                        color = InQ)) + 
#   geom_line(size=2) + 
#   xlim(17,80)
#   


# Macro 2
# mean spell order of spells
# starting in age x
App1_macro2 <- DisStats %>% 
  filter(first,
         !is.na(dis_dur)) %>% 
  group_by(sex, InQ, age) %>% 
  summarize(order_first_mean = mean(dis_order, na.rm = TRUE)) %>% 
  mutate(order_first_mean = na_if(order_first_mean, NaN)) %>% 
  ggplot(mapping = aes(x = age, 
                       y = order_first_mean, 
                       color = InQ)) + 
  geom_line(size=1.5) + 
  xlim(16,70) + 
  ylim(1,3.5) + 
  geom_segment(aes(x=54,y=2,xend=65,yend=2),color = "black") +
  annotate("text", x = 58, y = 2.05, label = "11 years") +
  annotate("text", x = 40, y = 1.6, label = "lowest 20%") +
  annotate("text", x = 40, y = 1.14, label = "highest 20%") +
  labs(x = "Age", 
       y = "mean episode order",
       main = "New disability episodes are on average 2nd episodes by age 54 if you're poor, 65 if you're rich")+
  guides(color=FALSE)+
  theme(
    axis.title.x = element_text(size = 13), # use 16 for presentations
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 13))
ggsave("/home/tim/workspace/Spells/Spells/Figures/App1_macro2.pdf",
       App1_macro2, width = 7, height = 6)  


# Macro 3
library(ggridges)
App1_macro3 <- Dat %>%
  filter(age < 80) %>% 
  group_by(InQ, id) %>% 
  mutate(dead = ifelse(any(state == "Dead"),TRUE,FALSE)) %>% 
  ungroup() %>% 
  filter(dead,
         state != "Dead") %>% 
  group_by(InQ, id) %>% 
  mutate(ttd = max(age) - age,
         ad5 = max(age) - max(age) %% 5) %>% 
  ungroup() %>% 
  filter(ad5 > 30) %>% 
  group_by(sex, InQ, ttd, ad5) %>% 
  summarize(ttdprev = mean(state == "Disabled")) %>% 
  ggplot(mapping = aes(x = ad5-ttd, 
                       y = as.factor(ad5), 
                       color = InQ,
                       fill = InQ,
                       height = ttdprev)) + 
  geom_density_ridges(stat = "identity",alpha = .4) +
  annotate("text",x=68,y=5.5,label="highest 20%") + 
  geom_segment(aes(x=63,y=5.5,xend=59,yend=6.1),color = "black") +
  annotate("text",x=49,y=6.7,label="lowest 20%") + 
  geom_segment(aes(x=54,y=6.7,xend=58,yend=6.5),color = "black") +
  labs(x = "Age", y = "Prevalence by age at death",size=2) +
  guides(fill=FALSE,color=FALSE) +
  theme(
    axis.title.x = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    legend.position = c(0.9, 0.1))
ggsave("/home/tim/workspace/Spells/Spells/Figures/App1_macro3.pdf",
       App1_macro3, width = 7, height = 6)

# Years spent healthy and disabled.
# near-complete survival for both income groups,
# but poorer spend more than twice as much time disabled.
# summed expentancies show poorer survive a bit better,
# presumably due to data quality? Need to find out 
# These disability results based on SRH, will be swapped
# out with functional disabiltiy anyway
Dat %>% 
  filter(age < 80) %>% 
  group_by(InQ, id) %>%
  summarize(H = sum(state == "Healthy"),
            D = sum(state == "Disabled")) %>% 
  ungroup() %>% 
  group_by(InQ) %>% 
  summarize(H = mean(H),
            D = mean(D))

