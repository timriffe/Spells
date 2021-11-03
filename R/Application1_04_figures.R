source(here::here("R","00_install_packages.R"))


A1.1 <- readRDS(here("Data","Application1","A1.1.rds"))

App1_macro1 <- 
  A1.1 %>% 
  group_by(InQ, sex, age) %>% 
  summarize(lower = quantile(dur_first_mean,0.025),
            upper = quantile(dur_first_mean,0.975),
            median = median(dur_first_mean),
            .groups = "drop") %>% 
  ggplot(mapping = aes(x = age, 
                       y = median, 
                       color = InQ,
                       fill = InQ,
                       ymin = lower,
                       ymax = upper)) + 
  geom_ribbon(alpha=.2, color = NA) +
  geom_line(size=1.5) + 
  geom_segment(aes(x=40,y=2,xend=49,yend=2),color = "black") +
  annotate("text", x = 38, y = 1.4, label = "highest 20%") +
  annotate("text", x = 40, y = 2.2, label = "lowest 20%") +
  annotate("text", x = 45, y = 1.4, label = "Women from the highest\nincome quintile reach a mean\ndisability bout duration\nof 2 years 9 years later.", hjust=0)+
  geom_curve(mapping=aes(x=43.5,y=1.65,xend = 45,yend=2),
             color="black",size=.1,curvature=-.6,arrow = arrow(length = unit(0.02, "npc")))+
  xlim(16,65) +
  labs(x = "Age", y = "conditional mean spell duration (years)",
       main = "Mean disability spell duration of spells starting in age x")+
  guides(color = "none", fill = "none")+
  theme(
    axis.title.x = element_text(size = 13), # use 16 for presentations
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 13))+
  scale_fill_brewer(palette = "Paired") + 
  scale_color_brewer(palette = "Paired")

ggsave(here("Figures","App1_macro1.pdf"),
       App1_macro1, width = 7, height = 6)


##############################
# Application Figure 1.2

A1.2 <- readRDS(here("Data","Application1","A1.2.rds"))

A1.2 %>% 
  group_by(InQ, sex, age) %>% 
  summarize(lower = quantile(order_first_mean,0.025),
            upper = quantile(order_first_mean,0.975),
            median = median(order_first_mean),.groups = "drop") %>% 
  filter(age == 60)

App1_macro2 <-
  A1.2 %>% 
  group_by(InQ, sex, age) %>% 
  summarize(lower = quantile(order_first_mean,0.025),
            upper = quantile(order_first_mean,0.975),
            median = median(order_first_mean),
            .groups = "drop") %>% 
  ggplot(mapping = aes(x = age, 
                       y = median, 
                       color = InQ,
                       fill = InQ,
                       ymin = lower,
                       ymax = upper)) + 
  geom_ribbon(alpha=.2, color = NA) +
  geom_line(size=1.5) + 
  scale_fill_brewer(palette = "Paired") + 
  scale_color_brewer(palette = "Paired") +
  xlim(16,78) + 
  ylim(1,6) + 
  # to be fixed:
  geom_segment(aes(x=60,y=3.61,xend=60,yend=4.37),color = "black") +
  annotate("text", x = 35, y = 4.5, label = "Women in the lowest\nincome quintile had\non average .75 more\ndisability bouts by age 60",hjust=0) +
  geom_curve(mapping=aes(x=43.5,y=4,xend = 59,yend=4),
             color="black",size=.1,curvature=.3,arrow = arrow(length = unit(0.02, "npc")))+
  annotate("text", x = 36, y = 2.6, label = "lowest 20%") +
  annotate("text", x = 40, y = 1.5, label = "highest 20%") +
  labs(x = "Age", 
       y = "mean episode order",
       main = "New disability episodes are on average 2nd episodes by age 54 if you're poor, 65 if you're rich")+
  guides(color = "none", fill = "none")+
  theme(
    axis.title.x = element_text(size = 13), # use 16 for presentations
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 13))
ggsave(here("Figures","App1_macro2.pdf"),
       App1_macro2, width = 7, height = 6)  

##############################
# Application 1.3
A1.3 <- readRDS(here("Data","Application1","A1.3.rds"))

A1.3 %>% 
  group_by(InQ, sex, ad5, ttd) %>% 
  summarize(median = quantile(ttdprev,.5)) %>% 
  ggplot(mapping = aes(x = ad5-ttd, 
                       y = as.factor(ad5), 
                       color = InQ,
                       fill = InQ,
                       height = median)) + 
  geom_density_ridges(stat = "identity",alpha = .4) +
  annotate("text",x=68,y=5.5,label="highest 20%") + 
  geom_segment(aes(x=63,y=5.5,xend=59,yend=6.1),color = "black") +
  annotate("text",x=49,y=6.7,label="lowest 20%") + 
  geom_segment(aes(x=54,y=6.7,xend=58,yend=6.5),color = "black") +
  labs(x = "Age", y = "Prevalence by age at death",size=2) +
  guides(fill="none",color="none") +
  theme(
    axis.title.x = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    legend.position = c(0.9, 0.1))

ggsave(here("Figures","App1_macro3.pdf"),
       App1_macro3, width = 7, height = 6)

