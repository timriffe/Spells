# Author: tim
###############################################################################
setwd("/home/tim/git/Spells/Spells")
set.seed(1)
source("R/GenerateStationary.R")
source("R/Counting.R")
source("R/Distributions.R")
source("R/Align.R")

EmplSpells  <- apply(RTraj_clean, 2, spell_durAge, state = "Employed")
EmplSpellsH <- apply(RTraj_clean, 2, spell_durAge, state = "Employed",TRUE)


avg_dur_given_empl              <- rowMeans(EmplSpells,na.rm=TRUE)
employed_given_empl50           <- rowMeans(!is.na(EmplSpells))
avg_dur_given_emplH             <- rowMeans(EmplSpellsH,na.rm=TRUE)
employedH                       <- rowMeans(!is.na(EmplSpellsH))



plot(50:100, avg_dur_given_empl, type = 'l')
plot(50:100, employed_given_empl50, type = 'l')

plot(50:100, avg_dur_given_emplH, type = 'l')
plot(50:100, employedH, type = 'l')

plot(50:100, evg_dur_given_empl * employed_given_empl50)
plot(50:100, employedH * avg_dur_given_emplH)



InactSpells  <- apply(RTraj_clean, 2, spell_durAge, state = "Inactive")
InactSpellsH <- apply(RTraj_clean, 2, spell_durAge, state = "Inactive",TRUE)

avg_dur_InactSpells              <- rowMeans(InactSpells,na.rm=TRUE)
Inact_given_empl50               <- rowMeans(!is.na(InactSpells))
avg_dur_given_InactH             <- rowMeans(InactSpellsH,na.rm=TRUE)
InactH                           <- rowMeans(!is.na(InactSpellsH))


plot(50:100, avg_dur_InactSpells, type = 'l')
plot(50:100, Inact_given_empl50, type = 'l')

plot(50:100, avg_dur_given_InactH, type = 'l')
plot(50:100, InactH, type = 'l')


sdInact <- sqrt(rowMeans((InactSpells - avg_dur_InactSpells) ^ 2, na.rm=TRUE))
png("/home/tim/git/ArrowDecomp/ArrowDecomp/Figures/spelldurationagepattern.png")
plot(50:100, avg_dur_InactSpells, type = 'l',ylim=c(0,18),
		main = "Conditional average 'inactive' spell duration\ngiven spell membership (not spell entry or exit)",
		sub = "quantile contours in 2.5% intervals",
		xlab = "Age", ylab = "spell duration",col="red",
		xlim = c(50,80))
intervalfan(InactSpells,50:100,border=NA,col = "#00000010",probs=seq(.05,.95,by=.05))
lines(50:100,qdens(InactSpells,.5),col = "black",lwd=2)
lines(50:100,avg_dur_InactSpells,col = "red",lwd=2)
text(55,5,"median",cex=1.5,font=)
text(55,8,"mean",cex=1.5,col = "red",font=2)
dev.off()

x <- RTraj_clean[,3]


InactLeft  <- apply(RTraj_clean, 2, spell_dur_after, state = "Inactive")
InactSpent <- apply(RTraj_clean, 2, spell_dur_before, state = "Inactive")

InactLeftMean             <- rowMeans(InactLeft,na.rm=TRUE)
InactSpentMean             <- rowMeans(InactSpent,na.rm=TRUE)

plot(50:100, InactLeftMean, type = 'l',ylim=c(0,18),
		main = "Conditional average 'inactive' time left in spell\ngiven spell membership (not spell entry or exit)",
		sub = "quantile contours in 2.5% intervals",
		xlab = "Age", ylab = "spell duration",col="red",
		xlim = c(50,80))
intervalfan(InactLeft,50:100,border=NA,col = "#00000010",probs=seq(.05,.95,by=.05))
lines(50:100,qdens(InactLeft,.5),col = "black",lwd=2)
lines(50:100,InactLeftMean,col = "red",lwd=2)


plot(50:100, InactSpentMean, type = 'l',ylim=c(0,18),
		main = "Conditional average 'inactive' time spent in spell\ngiven spell membership (not spell entry or exit)",
		sub = "quantile contours in 2.5% intervals",
		xlab = "Age", ylab = "spell duration",col="red",
		xlim = c(50,80))
intervalfan(InactSpent,50:100,border=NA,col = "#00000010",probs=seq(.05,.95,by=.05))
lines(50:100,qdens(InactSpent,.5),col = "black",lwd=2)
lines(50:100,InactSpentMean,col = "red",lwd=2)


