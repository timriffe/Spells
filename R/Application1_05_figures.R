source(here::here("R","00_load_functions.R"))


A1.1 <- readRDS(here("Data","Application1","A1.1.rds"))

App1_macro1 <- 
  A1.1 %>% 
  group_by(INC_Q, age) %>% 
  summarize(lower = quantile(dur_first_mean,0.025),
            upper = quantile(dur_first_mean,0.975),
            median = median(dur_first_mean),
            .groups = "drop") %>% 
  ggplot(mapping = aes(x = age, 
                       y = median, 
                       color = INC_Q,
                       fill = INC_Q,
                       ymin = lower,
                       ymax = upper)) + 
  geom_ribbon(alpha=.2, color = NA) +
  geom_line(size=1.5) + 
  geom_segment(aes(x=38,y=2,xend=49,yend=2),color = "black") +
  annotate("text", x = 38, y = 1.4, label = "highest 20%") +
  annotate("text", x = 35, y = 2.2, label = "lowest 20%") +
  annotate("text", x = 45, y = 1.4, label = "Women from the highest\nincome quintile reach a mean\ndisability bout duration\nof 2 years 11 years later.", hjust=0)+
  geom_curve(mapping=aes(x=43.5,y=1.65,xend = 45,yend=2),
             color="black",size=.1,curvature=-.6,arrow = arrow(length = unit(0.02, "npc")))+
  xlim(26,65) +
  labs(x = "Age", y = "conditional mean spell duration (years)",
       main = "Mean disability spell duration of spells starting in age x")+
  guides(color = "none", fill = "none")+
  theme(
    axis.title.x = element_text(size = 14), # use 16 for presentations
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") + 
  scale_color_brewer(palette = "Paired")
App1_macro1
ggsave(here("Figures","App1_macro1.pdf"),
       App1_macro1, width = 7, height = 6)


##############################
# Application Figure 1.2

A1.2 <- readRDS(here("Data","Application1","A1.2.rds"))

A1.2 %>% 
  group_by(INC_Q, age) %>% 
  summarize(lower = quantile(order_first_mean,0.025),
            upper = quantile(order_first_mean,0.975),
            median = median(order_first_mean),.groups = "drop") %>% 
  filter(age == 70)

App1_macro2 <-
  A1.2 %>% 
  group_by(INC_Q, age) %>% 
  summarize(lower = quantile(order_first_mean,0.025),
            upper = quantile(order_first_mean,0.975),
            median = median(order_first_mean),
            .groups = "drop") %>% 
  ggplot(mapping = aes(x = age, 
                       y = median, 
                       color = INC_Q,
                       fill = INC_Q,
                       ymin = lower,
                       ymax = upper)) + 
  geom_ribbon(alpha=.2, color = NA) +
  geom_line(size=1.5) + 
  scale_fill_brewer(palette = "Paired") + 
  scale_color_brewer(palette = "Paired") +
  xlim(30,78) + 
  ylim(1,6.5) + 
  # to be fixed:
  geom_segment(aes(x=65,y=3.54,xend=65,yend=4.3),color = "black") +
  annotate("text", x = 35, y = 4.5, label = "Women in the lowest\nincome quintile had\non average .8 more\ndisability bouts by age 65",hjust=0) +
  geom_curve(mapping=aes(x=43.5,y=4,xend = 59,yend=4),
             color="black",size=.1,curvature=.3,arrow = arrow(length = unit(0.02, "npc")))+
  annotate("text", x = 46, y = 2.7, label = "lowest 20%", size = 5) +
  annotate("text", x = 50, y = 1.8, label = "highest 20%", size = 5) +
  labs(x = "Age", 
       y = "mean episode order",
       main = "New disability episodes are on average 2nd episodes by age 54 if you're poor, 65 if you're rich")+
  guides(color = "none", fill = "none")+
  theme(
    axis.title.x = element_text(size = 13), # use 16 for presentations
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 13))
App1_macro2
ggsave(here("Figures","App1_macro2.pdf"),
       App1_macro2, width = 7, height = 6)  

##############################
# Application 1.3
A1.3 <- readRDS(here("Data","Application1","A1.3.rds"))
App1_macro3 <-
A1.3 %>% 
  group_by(INC_Q, ad5, ttd) %>% 
  summarize(median = quantile(ttdprev,.5)) %>% 
  ggplot(mapping = aes(x = ad5-ttd, 
                       y = as.factor(ad5), 
                       color = INC_Q,
                       fill = INC_Q,
                       height = median)) + 
  geom_density_ridges(stat = "identity",alpha = .4) +
  annotate("text",x=68,y=5.5,label="highest 20%") + 
  geom_segment(aes(x=63,y=5.5,xend=59,yend=6.1),color = "black") +
  annotate("text",x=39,y=6.8,label="lowest 20%") + 
  geom_segment(aes(x=43.5,y=6.7,xend=48,yend=6.6),color = "black") +
  labs(x = "Age", y = "Prevalence by age at death",size=2) +
  guides(fill="none",color="none") +
  theme(
    axis.title.x = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    legend.position = c(0.9, 0.1))
App1_macro3
ggsave(here("Figures","App1_macro3.pdf"),
       App1_macro3, width = 7, height = 6)


tp       <- 
  readRDS("Data/Application1/tp_limitations.rds") %>% 
  separate(from, into = c("age", "state_from"), sep = "::",convert=TRUE) %>% 
  separate(to, into = c(NA, "state_to"), sep = "::") %>% 
  filter(sex == "F") %>% 
  mutate(lt_adj = TRUE) %>% 
  select(-sex)
tp_unadj <- readRDS("Data/Application1/old/females_tp_limitations.rda") %>% 
  separate(from, into = c(NA, "state_from"), sep = "::") %>% 
  rename(INC_Q = InQ) %>% 
  select(-to) %>% 
  mutate(lt_adj = FALSE) 
readRDS("Data/Application1/tp_limitations.rds") %>% head()
str(tp)
tps <-
  tp %>% 
  bind_rows(tp_unadj) %>% 
  pivot_wider(names_from = state_to, values_from = probs) %>% 
  mutate(Dead = 1 - Disabled - Healthy) %>% 
  pivot_longer(Disabled:Dead, names_to = "state_to", values_to = "probs")

tps %>% 
  filter(state_from == "Disabled",
         INC_Q %in% c("I","V")) %>% 
  ggplot(aes(x = age, y = probs, 
             color = state_to, 
             lty = lt_adj, 
             group = interaction(state_to, lt_adj))) +
  geom_line() + 
  facet_wrap(~INC_Q)


LTf <- read_delim("Data/Application1/Italia_2014_Femmine.csv",skip=1,deli=";",locale = locale(decimal_mark = ",")) %>% 
  mutate(qx = `Probabilità di morte (per mille)`/1000) %>% 
  rename(age = Età) %>% 
  select(age, qx)

tps %>% 
  filter(state_to == "Dead") %>% 
  ggplot(aes(x=age, y = probs, color = state_from, lty = lt_adj,group = interaction(state_from, INC_Q, lt_adj))) +
  geom_line() +
  scale_y_log10() +
  geom_line(data = LTf %>% filter(between(age, 16,80)), mapping = aes(x = age, y = qx), color = "black", inherit.aes = FALSE)
  


