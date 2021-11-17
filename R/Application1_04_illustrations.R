source(here::here("R","00_load_functions.R"))

Dat <- readRDS(here::here("Data","Application1","old","SILCsim.rds"))

XX <- Dat %>% 
  filter(age < 80) %>% 
  group_by(InQ, id) %>% 
  mutate(
    state = as.character(state),
    dead = ifelse(any(state == "Dead"),TRUE,FALSE)) %>% 
  ungroup() %>% 
  filter(dead) %>% 
  filter(InQ == "I",
         age >= 50 & age <= 80) %>% 
  mutate(state = case_when(state == "Healthy" ~ "H",
                           state == "Disabled" ~ "D",
                           TRUE ~ "Dead")) %>%
  acast(id~age, value.var = "state") 
XX <- XX[1:10,]

colsIN <- c("#855C75","#D9AF6B","#AF6458","#736F4C","#526A83","#625377","#68855C","#9C9C5E","#A06177","#8C785D","#467378","#7C7C7C")

# colsIN <-c("#88CCEE","#CC6677","#DDCC77","#117733","#332288","#AA4499","#44AA99","#999933","#882255","#661100","#6699CC","#888888")
# length(colsIN)
# plot(NULL,xlim=c(0,12),ylim=c(0,1),asp=1)
# for (i in 1:12){
#   rect(i-1,0,i,1,col=colsIN[i])
# }
# colsHD <- c(H = "#add996",D="#bf9319",Dead="#FFFFFF00")
colsHD <- c(H =  gray(.9), D = lighten(colsIN[3],.25),Dead="#FFFFFF00")
pdf(here::here("Figures","App1_grammar1.pdf"),width=9,height=4)
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
pdf(here::here("Figures","App1_grammar2.pdf"),width=9,height=4)
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
pdf(here::here("Figures","App1_grammar3.pdf"),width=9,height=4)
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



