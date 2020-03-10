

library(here)
library(tidyverse)
library(ggplot2)

A1.1 <- readRDS(here("Spells","Data","Lorenti","A1.1.rds"))

App1_macro1 <- 
  A1.1 %>% 
  group_by(InQ, sex, age) %>% 
  summarize(lower = quantile(dur_first_mean,0.025),
            upper = quantile(dur_first_mean,0.975),
            median = median(dur_first_mean)) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x = age, 
                       y = median, 
                       color = InQ,
                       fill = InQ,
                       ymin = lower,
                       ymax = upper)) + 
  geom_ribbon(alpha=.2, color = NA)+
  geom_line(size=1.5) + 
  geom_segment(aes(x=40,y=2,xend=49,yend=2),color = "black") +
  annotate("text", x = 38, y = 1.4, label = "lowest 20%") +
  annotate("text", x = 40, y = 2.2, label = "highest 20%") +
  annotate("text", x = 50, y = 1.6, label = "9-year lag in reaching\nmean duration of 2 years")+
  geom_curve(mapping=aes(x=43.5,y=1.65,xend = 45,yend=2),
             color="black",size=.4,curvature=-.6,arrow = arrow(length = unit(0.02, "npc")))+
  xlim(16,65) +
  labs(x = "Age", y = "conditional mean spell duration (years)",
       main = "Mean disability spell duration of spells starting in age x")+
  guides(color = FALSE, fill = FALSE)+
  theme(
    axis.title.x = element_text(size = 13), # use 16 for presentations
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 13))+
  scale_fill_brewer(palette = "Paired") + 
  scale_color_brewer(palette = "Paired")
ggsave(here("Spells","Figures","App1_macro1.pdf"),
       App1_macro1, width = 7, height = 6)


##############################
# Application 1.2

A1.2 <- readRDS(here("Spells","Data","Lorenti","A1.2.rds"))
head(A1.2)
A1.2 %>% 
  group_by(InQ, sex, age) %>% 
  summarize(lower = quantile(order_first_mean,0.025),
            upper = quantile(order_first_mean,0.975),
            median = median(order_first_mean)) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x = age, 
                       y = order_first_mean, 
                       color = InQ,
                       fill = InQ,
                       ymin = lower,
                       ymax = upper)) + 
  geom_ribbon(alpha=.2, color = NA) +
  geom_line(size=1.5) + 
  scale_fill_brewer(palette = "Paired") + 
  scale_color_brewer(palette = "Paired")+
  xlim(16,70) + 
  ylim(1,3.5) + 
  
  # to be fixed:
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




