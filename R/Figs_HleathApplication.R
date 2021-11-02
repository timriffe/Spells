
# first diagrams:
library(here)
library(tidyverse)
library(reshape2)
# devtools::install_github("timriffe/Spells/Spells/R/Spells")
#devtools::install_local(here::here("Spells","R","Spells"), force = TRUE)
library(Spells)


Dat <- readRDS(here::here("Spells","Data","Lorenti","SILCsim.rds"))
Dat$state <- as.character(Dat$state)

XX <- Dat %>% 
  filter(age < 80) %>% 
  group_by(InQ, id) %>% 
  mutate(dead = ifelse(any(state == "Dead"),TRUE,FALSE)) %>% 
  ungroup() %>% 
  filter(dead) %>% 
  filter(InQ == "I",
         age >= 50 & age <= 80) %>% 
  mutate(state = case_when(state == "Healthy" ~ "H",
                           state == "Disabled" ~ "D",
                           TRUE ~ "Dead")) %>%
  acast(id~age, value.var = "state") 
XX <- XX[1:10,]


colsHD <- c(H = "#add996",D="#bf9319",Dead="#FFFFFF00")
pdf(here::here("Spells","Figures","App1_grammar1.pdf"),width=9,height=4)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,81), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
  draw_sequence(
    state_seq = XX[i,],
    x = 50:79,
    labels = clock(XX[i,], 
                   state = "D", 
                   clock_type = "duration", 
                   condition = "entry"),
    cols = colsHD,
    y = i-1,
    box = TRUE,
    border = NA
  )
}
axis(1)
text(50,9:0+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
legend(-10,-2,fill = colsHD[c("H","D")], legend = c("Healthy","Disabled"),horiz = TRUE,xpd=TRUE,bty="n")
dev.off()

# grammar 2
pdf(here::here("Spells","Figures","App1_grammar2.pdf"),width=9,height=4)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,81), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
  draw_sequence(
    state_seq = XX[i,],
    x = 50:79,
    labels = clock(XX[i,], 
                   state = "D", 
                   clock_type = "order", 
                   increasing = TRUE,
                   condition = "entry"),
    cols = colsHD,
    y = i-1,
    box = TRUE,
    border = NA
  )
}
axis(1)
text(50,9:0+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
legend(-10,-2,fill = colsHD[c("H","D")], legend = c("Healthy","Disabled"),horiz = TRUE,xpd=TRUE,bty="n")
dev.off()


# grammar 3
# align on death
pdf(here::here("Spells","Figures","App1_grammar3.pdf"),width=9,height=4)
par(mai=c(.8,0,0,1))
plot(NULL, type = "n", xlim = c(-30,0), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
  clk <- clock(XX[i,], 
               state = "D", 
               clock_type = "identity")
  clk[clk == 0] <- NA
  draw_sequence(
    state_seq = XX[i,],
    x = align(XX[i,], state = "Dead",type="left"),
    labels = clk,
    cols = colsHD,
    y = i-1,
    box = TRUE,
    border = NA
  )
}
axis(1)
text(1,9:0+.5,1:10,pos=4,xpd=TRUE)
text(3,6,"Random individual i",xpd=TRUE,srt=90)
legend(-10,-2,fill = colsHD[c("H","D")], legend = c("Healthy","Disabled"),horiz = TRUE,xpd=TRUE,bty="n")
dev.off()
