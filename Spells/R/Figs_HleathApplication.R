
# first diagrams:


library(here)
library(tidyverse)
library(markovchain)
library(reshape2)
# devtools::install_github("timriffe/Spells/Spells/R/Spells")
#devtools::install_local(here::here("Spells","R","Spells"), force = TRUE)
library(Spells)


Dat <- readRDS(here::here("Spells","Data","Lorenti","SILCsim.rds"))
Dat$state <- as.character(Dat$state)

X <- Dat %>% 
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
X <- X[1:10,]
DurEntry      <- apply(X, 1, clock, state = "D", clock_type = "duration", entry = TRUE)

DurEntry <- t(DurEntry)

# this appears in draft manuscript
pdf(here("Spells","Figures","DisTrajExample.pdf"),width=7,height=4.5)
colsHD <- c("#add996","#bf9319",Dead="#FFFFFF00")
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,81), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
  draw_sequence2(X[i,],states = c("H","D","Dead"),colsHD,y=yvals[i], border = NA)
  draw_sequence3(DurEntry[i,],y=yvals[i])
  rect(50,yvals[i],(which(X[,i] == "Dead")[1]+49),yvals[i]+1,border = gray(.4),lwd=.5)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
legend(-10,-2,fill = colsHD[1:2], legend = c("Healthy","Disabled"),horiz = TRUE,xpd=TRUE,bty="n")
dev.off()



