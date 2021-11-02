## PROGRAM:		temp_sequences_28mar15.R
## DESCRIPTION:	Sequence analysis for PAA
## PROJECT:		Diss/Sequence analysis
## AUTHOR/DATE:	ERV/4April2015
## Edited: TR/12Dec2019
# original script located in ~/Spells/Data/MAFE/EV/temp_sequences_27apr15.r
library(foreign)
library(tidyverse)
library(Spells)
library(reshape2)
library(here)
## 2 - READ STATA 17nov DATASET: MIGRATION YEAR
legstat.17nov <- read.dta(
  here("Spells", "Data", "MAFE", "EV",
       "sequence_legstat_17nov_wide_migyr_05apr2015.dta"))

firstmigdec85 <- read.dta(
  here("Spells", "Data", "MAFE", "EV",
       "firstmigdec85.dta"))

novisaenter  <- read.dta(
  here("Spells", "Data", "MAFE", "EV",
       "novisa_14jun.dta"))

legstat.17nov.calyr <- read.dta(
  here("Spells", "Data", "MAFE", "EV",
       "sequence_legstat_17nov_wide_calyr_05apr2015.dta"))

# TR: funny there's no age variable.

Dat <- legstat.17nov %>% 
  left_join(novisaenter, by = "ident_nmspell") %>% 
  left_join(firstmigdec85, by = "ident_nmspell") %>% 
  pivot_longer(cols = legstat_17nov1:legstat_17nov53,
               names_to = "time",
               names_prefix = "legstat_17nov",
               values_to = "legstat",
               values_drop_na = TRUE) %>% 
  rename(id = ident_nmspell) %>% 
  mutate(time_left_mig = as.integer(time) - 1,
         year = firstmigyr_nmspell + time_left_mig,
         legstat = as.character(legstat)) %>% 
  filter( retmig == 0)

glimpse(Dat)

Dat %>% 
  group_by(id) %>% 
  mutate(time_exit_irreg = align(legstat, 
                                 state = c("Fully irreg. (NRP, NWP)"),
                                 type = "right",
                                 spell = "first"),
         time_spent_reg = clock(
                                 legstat, 
                                 state = c("Fully reg. (RP, WP)"),
                                 clock_type = "step"
                           ),
         irreg_dur = clock(
                                 legstat, 
                                 state = c("Fully irreg. (NRP, NWP)"),
                                 clock_type = "duration")) %>% 
  ungroup() %>% 
  group_by(time_left_mig) %>% 
  summarize(irreg_dur = mean(irreg_dur, na.rm = TRUE)) %>% 
  ggplot(mapping = aes(x = time_left_mig,
                       y = irreg_dur)) + 
  geom_line()
         
  


c("Fully irreg. (NRP, NWP)",
 "Fully reg. (RP, WP)",
 "Mixed (RP, NWP)",
 "Mixed (NRP, WP)")   


# legstat.17nov.calyr %>% 
# pivot_longer(cols = legstat_17nov1950:legstat_17nov2008,
#              names_to = "time",
#              names_prefix = "legstat_17nov",
#              values_to = "legstat",
#              values_drop_na = TRUE) %>% 
# pull(time) %>% 
#   cbind(a)






# 
# 
# # merge datasets
# legstat.17nov <- merge(legstat.17nov, firstmigdec85, by = "ident_nmspell")
# # names(legstat.17nov)
# 
# legstat.17nov <- merge(legstat.17nov, novisaenter, by = "ident_nmspell")
# 
# table(legstat.17nov$id_country)
# 
# # legstat.17nov$ctry_yr <- factor(legstat.17nov$ctry_yr, levels=c("FRANCE","ITALIE","ESPAGNE"))
# # levels(legstat.17nov$ctry_yr)[1] <- "France"
# # levels(legstat.17nov$ctry_yr)[2] <- "Italy"
# # levels(legstat.17nov$ctry_yr)[3] <- "Spain"
# 
# table(legstat.17nov$ctry_yr,legstat.17nov$id_country)
# table(legstat.17nov$id_country,legstat.17nov$ctry_yr)
# addmargins(table(legstat.17nov$id_country,legstat.17nov$ctry_yr))
# levels(legstat.17nov$legstat_17nov1)
# 
# ## READ STATA 17nov DATASET: CALENDAR YEAR
# 
# # TR: this was already read in, it seems
# legstat.17nov.calyr <- read.dta(
#   here("Spells", "Data", "MAFE", "EV",
#           "sequence_legstat_17nov_wide_calyr_05apr2015.dta"))
# # dir(here("Spells", "Data", "MAFE", "EV"))
# 
# legstat.17nov.calyr <- merge(legstat.17nov.calyr, firstmigdec85, 
#                              by="ident_nmspell")
# legstat.17nov.calyr <- merge(legstat.17nov.calyr, novisaenter, 
#                              by = "ident_nmspell")
# 
# # define sequence objects -------------------------------------------------
# 
# ## color palette for sequence objects
# legstat.17nov.pal <- brewer.pal(4, "Spectral")
# 
# ## with censored
# legstat.17nov.seq <- seqdef(legstat.17nov,2:54,right=NA,xtstep=5, cpal=legstat.17nov.pal)
# 
# ## without censored, for use in distance calculations
# legstat.17nov.nomiss.seq <- seqdef(legstat.17nov,2:54,right="DEL",xtstep=5, cpal=legstat.17nov.pal)
# 
# ## calendar year, with missing
# legstat.17nov.calyr.seq <- seqdef(legstat.17nov.calyr,2:60,right=NA,xtstep=5,cpal=legstat.17nov.pal)
# 
# # check sequence object with some plots -----------------------------------
# 
# seqlegend(legstat.17nov.seq,fontsize=.9)
# seqlegend(legstat.17nov.seq,ncol=2,fontsize=.9)
# seqstatl(legstat.17nov.seq)
# seqdplot(legstat.17nov.seq,with.legend=TRUE,with.missing=TRUE,cex.legend=.8,xtlab=c(1:54))
# 
# par(new=TRUE)
# plot(seqstatd(legstat.17nov.seq)$ValidStates, axes=FALSE,
# 	col="blue",ylim=c(0,768),type="p",pch=21,
# 	ylab="",xlab="")
# par(new=TRUE)
# lines(seqstatd(legstat.17nov.seq)$ValidStates, axes=FALSE,
# 	col="blue",ylim=c(0,768),lwd=2)
# axis(4)
# mtext("Number of Valid States",4)
# 
# 
# ###### Sequence Index plots -------------------------------------------
# 
# ## (overall) 
# windows()
# seqIplot(legstat.17nov.seq,xtlab=c(1:54),withlegend=TRUE, cex.legend = .85)#sortv=legstat.17nov.comp,
# 
# ## by destination
# seqIplot(legstat.17nov.seq,xtlab=c(1:54),withlegend=TRUE, cex.legend = .85, group = legstat.17nov$ctry_yr)
# 
# 
# ## by entry status
# seqIplot(legstat.17nov.seq,xtlab=c(1:54),withlegend=TRUE, cex.legend = .85, group = legstat.17nov$novisaenter)
# 
# 
# # Sequence frequency plots ------------------------------------------------
# 
# seqfplot(legstat.17nov.nomiss.seq, group = legstat.17nov$ctry_yr, pbarw = T,xtlab=c(1:54), cex.plot = .7)
# 
# seqfplot(legstat.17nov.nomiss.seq, group = legstat.17nov$novisaenter, pbarw = T,xtlab=c(1:54), cex.plot = .7)
# 
# seqfplot(legstat.17nov.nomiss.seq, pbarw = T,xtlab=c(1:54), cex.plot = .7)
# 
# 
# 
# # Figure 1: Mean time in each state ---------------------------------------
# ## MEAN TIME PLOTS
# 
# ## Mean time spent in each state, all destinations  
# 
# seqmtplot(legstat.17nov.seq)
# seqmtplot(legstat.17nov.seq, withlegend=TRUE, with.missing=FALSE,axes=FALSE, ylim=c(0,10),cex.legend=.77)
# 
# #title="Mean number of years in each state, all destinations"
# 
# #legend(-.75,-.25,legend=c(seqstatl(legstat.17nov.seq)[2:13]),col=legstat.17nov.pal,fill=legstat.17nov.pal,horiz=FALSE,cex=.7,ncol=6)
# 
# # Fig 2. Mean time spent in each state, by country ---------------------
# ## Mean time in each state, by destination  
# 
# mult.fig(mfrow=c(2,2),main = "Mean number of years in each state, by destination")
# seqmtplot(legstat.17nov.seq[legstat.17nov$ctry_yr == "France",], withlegend=FALSE, with.missing=FALSE, cex.plot=.75,ylim=c(0,10),axes=FALSE)
# title(main=list("France",cex=.9))	
# seqmtplot(legstat.17nov.seq[legstat.17nov$ctry_yr == "Italy",], withlegend=FALSE, with.missing=FALSE, cex.plot=.75,ylim=c(0,10),axes=FALSE)
# title(main=list("Italy",cex=.9))	
# seqmtplot(legstat.17nov.seq[legstat.17nov$ctry_yr == "Spain",], withlegend=FALSE, with.missing=FALSE, cex.plot=.75,ylim=c(0,10),axes=FALSE)
# title(main=list("Spain",cex=.9))
# seqlegend(legstat.17nov.seq,fontsize=1.15,position="center",with.missing=FALSE)
# 
# 
# # Fig 3 Mean time spent in each state, by initial dich lstat  --------
# ## Mean time spent in each state, by initial visa status  
# 
# mult.fig(mfrow=c(1,2),main = "Mean number of years in each state, by initial entry status")
# ##par(mfrow=c(1,2), mar=c(4,4,0.9,0.5), oma=c(1,2,2,4),cex.main=1.1) 
# ##mtext("Title")
# seqmtplot(legstat.17nov.seq[legstat.17nov$novisa == "No Visa",]
# 	, withlegend=FALSE, with.missing=FALSE, cex.plot=.75,ylim=c(0,10),axes=FALSE)
# title(main=list("No visa",cex=.9))	
# seqmtplot(legstat.17nov.seq[legstat.17nov$novisa == "Visa",]
# 	, withlegend=FALSE, with.missing=FALSE, cex.plot=.75,ylim=c(0,10),axes=FALSE)
# title(main=list("Visa",cex=.9))	
# par(new=TRUE)	
# seqlegend(legstat.17nov.seq, with.missing=FALSE,ncol = 2, position = "bottom", fontsize = .9)
# 
# 
# # Mean time plots by arrival period and sex -------------------------------
# 
# 
# ## Mean time spent in each state, by period
# mult.fig(mfrow=c(1,2),main = "Mean time in each state, by period of arrival")
# par(xpd=TRUE)
# seqmtplot(legstat.17nov.seq[(legstat.17nov$firstmigdec_di2_nms=="1950s-1980s"),]
#   , withlegend=FALSE, with.missing=FALSE, cex.plot=.75,ylim=c(0,7),axes=FALSE)
# title(main=list("1950s-1980s",cex=.9))	
# seqmtplot(legstat.17nov.seq[(legstat.17nov$firstmigdec_di2_nms=="1990s-2000s"),]
# 	, withlegend=FALSE, with.missing=FALSE, cex.plot=.75,ylim=c(0,7),axes=FALSE)
# title(main=list("1990s-2000s",cex=.9))
# par(new=TRUE)	
# ##legend(-10,-.25,legend=c(seqstatl(legstat.17nov.seq)[2:13]),col=legstat.17nov.pal,fill=legstat.17nov.pal,horiz=FALSE,cex=.7,ncol=6)	
# seqlegend(legstat.17nov.seq, with.missing=FALSE)
# 
# ## Mean time spent in each state, by sex
# mult.fig(mfrow=c(1,2),main = "Mean number of years in each state, by sex")
# ##par(mfrow=c(1,2), mar=c(4,4,0.9,0.5), oma=c(1,2,2,4),cex.main=1.1) 
# ##mtext("Title")
# par(new=TRUE)  
# seqmtplot(legstat.17nov.seq[legstat.17nov$q1 == "Man",], withlegend=FALSE, with.missing=FALSE, cex.plot=.75,ylim=c(0,10),axes=FALSE)
# 
# title(main=list("Men",cex=.9))
# 
# seqmtplot(legstat.17nov.seq[legstat.17nov$q1 == "Woman",], withlegend=FALSE, with.missing=FALSE, cex.plot=.75,ylim=c(0,10),axes=FALSE)
# 
# title(main=list("Women",cex=.9))  
# par(new=TRUE)	
# seqlegend(legstat.17nov.seq, with.missing=FALSE)
# mult.fig(mfrow=c(1,1)) #reset graphics output
# 
# 
# # Fig 4. Dist of states in first year of migration, by country --------
# 
# 
# legstat.17nov.statd <- seqstatd(legstat.17nov.seq)
# legstat.17nov.fr.statd <- seqstatd(legstat.17nov.seq[legstat.17nov$ctry_yr=="France",])
# legstat.17nov.it.statd <- seqstatd(legstat.17nov.seq[legstat.17nov$ctry_yr=="Italy",])
# legstat.17nov.sp.statd <- seqstatd(legstat.17nov.seq[legstat.17nov$ctry_yr=="Spain",])
# 
# # As stacked bar charts
# par(xpd=TRUE) ## allows legend outside of plot region
# barplot(as.matrix(legstat.17nov.statd$Frequencies[,1]),col=legstat.17nov.pal,ylab="Proportion", mgp = c(3, 2, 0),#middle parameter of mgp sets distance between label and axis
#   xlim=c(0,7),width=1,names.arg=list(c(paste("All destinations\nEntropy = ",round(legstat.17nov.statd$Entropy[1],2),
# 	"\nn = ",legstat.17nov.statd$ValidStates[1]))),cex.names=.75)
# 
# barplot(as.matrix(legstat.17nov.fr.statd$Frequencies[,1]),col=legstat.17nov.pal,ylab="Proportion", 
# 	names.arg=list(c(paste("France\nEntropy = ",round(legstat.17nov.fr.statd$Entropy[1],2),
# 	"\nn = ",legstat.17nov.fr.statd$ValidStates[1]))),cex.names=.75,
# 	,xlim=c(0,7),width=1,add=TRUE,space=2.2,mgp = c(3, 2, 0))
# 
# barplot(as.matrix(legstat.17nov.it.statd$Frequencies[,1]),col=legstat.17nov.pal,ylab="Proportion", 
# 	names.arg=list(c(paste("Italy\nEntropy = ",round(legstat.17nov.it.statd$Entropy[1],2),
# 	"\nn = ",legstat.17nov.it.statd$ValidStates[1]))),cex.names=.75,
# 	,xlim=c(0,7),width=1,add=TRUE,space=4.2,mgp = c(3, 2, 0))	
# 
# barplot(as.matrix(legstat.17nov.sp.statd$Frequencies[,1]),col=legstat.17nov.pal,ylab="Proportion", names.arg=list(c(paste("Spain\nEntropy = ",round(legstat.17nov.sp.statd$Entropy[1],2),"\nn = ",legstat.17nov.sp.statd$ValidStates[1]))),cex.names=.75,,xlim=c(0,7),width=1,add=TRUE,space=6.2,mgp = c(3, 2, 0))	
# 
# #title(main=list("Distribution of states in year of survey, by country",cex=1))
# 
# legend("topright", legend=c(seqstatl(legstat.17nov.seq)[2:5]),col=legstat.17nov.pal,fill=legstat.17nov.pal,horiz=FALSE,cex=.7,ncol=4)
# 
# #
# 
# mtext(expression("Pearson "*chi^2*" =  223.21 (22 df), p < .001"),cex=.8,padj=-.5,side=1)
# 
# 
# 
# # Fig 5 Dist of states in 2008, by country --------------------------------
# 
# ## Figure 5: DISTRIBUTION OF STATES IN 2008
# legstat.17nov.calyr.statd <- seqstatd(legstat.17nov.calyr.seq)
# legstat.17nov.calyr.fr.statd <- seqstatd(legstat.17nov.calyr.seq[legstat.17nov.calyr$ctry_yr=="France",])
# legstat.17nov.calyr.it.statd <- seqstatd(legstat.17nov.calyr.seq[legstat.17nov.calyr$ctry_yr=="Italy",])
# legstat.17nov.calyr.sp.statd <- seqstatd(legstat.17nov.calyr.seq[legstat.17nov.calyr$ctry_yr=="Spain",])
# 
# ## As stacked bar charts
# par(xpd=TRUE) ## allows legend outside of plot region
# barplot(as.matrix(legstat.17nov.calyr.statd$Frequencies[,59]),col=legstat.17nov.pal,ylab="Proportion", mgp = c(3, 2, 0),#middle parameter of mgp sets distance between label and axis
#   xlim=c(0,7),width=1,names.arg=list(c(paste("All destinations\nEntropy = ",round(legstat.17nov.calyr.statd$Entropy[59],2),
#   "\nn = ",legstat.17nov.calyr.statd$ValidStates[59]))),cex.names=.75)
# barplot(as.matrix(legstat.17nov.calyr.fr.statd$Frequencies[,59]),col=legstat.17nov.pal,ylab="Proportion", 
# 	names.arg=list(c(paste("France\nEntropy = ",round(legstat.17nov.calyr.fr.statd$Entropy[59],2),
# 	"\nn = ",legstat.17nov.calyr.fr.statd$ValidStates[59]))),cex.names=.75,
# 	,xlim=c(0,7),width=1,add=TRUE,space=2.2,mgp = c(3, 2, 0))
# barplot(as.matrix(legstat.17nov.calyr.it.statd$Frequencies[,59]),col=legstat.17nov.pal,ylab="Proportion", 
# 	names.arg=list(c(paste("Italy\nEntropy = ",round(legstat.17nov.calyr.it.statd$Entropy[59],2),
# 	"\nn = ",legstat.17nov.calyr.it.statd$ValidStates[59]))),cex.names=.75,
# 	,xlim=c(0,7),width=1,add=TRUE,space=4.2,mgp = c(3, 2, 0))	
# barplot(as.matrix(legstat.17nov.calyr.sp.statd$Frequencies[,59]),col=legstat.17nov.pal,ylab="Proportion", names.arg=list(c(paste("Spain\nEntropy = ",round(legstat.17nov.calyr.sp.statd$Entropy[59],2),"\nn = ",legstat.17nov.calyr.sp.statd$ValidStates[59]))),cex.names=.75,,xlim=c(0,7),width=1,add=TRUE,space=6.2,mgp = c(3, 2, 0))	
# title(main=list("Distribution of states in year of survey, by country",cex=1))
# 
# legend(-.25,-.11,legend=c(seqstatl(legstat.17nov.calyr.seq)[2:5]),col=legstat.17nov.pal,fill=legstat.17nov.pal,horiz=FALSE,cex=.7,ncol=2)
# 
# mtext(expression("Pearson "*chi^2*" =  223.21 (22 df), p < .001"),cex=.8,padj=-.5)
# 
# ## Chi-squared test of independence
# addmargins(table(legstat.17nov.calyr$legstat_17nov2008,legstat.17nov.calyr$ctry_yr))
# Xsq.calyr.ctry <- chisq.test(table(legstat.17nov.calyr$legstat_17nov2008,legstat.17nov.calyr$ctry_yr))
# Xsq.calyr.ctry$sigcolor[abs(Xsq.calyr.ctry$residuals) >= 2] <- "red"
# Xsq.calyr.ctry$sigcolor[abs(Xsq.calyr.ctry$residuals) <= 2] <- "black"
# 
# ## Dot chart of Pearson residuals, should give us an idea of which cells deviate significantly from independence
# dotchart(Xsq.calyr.ctry$residuals, color=Xsq.calyr.ctry$sigcolor,cex=.7,xlab="Standardized Pearson residual",
# 	main=expression("Contribution of variables to "*chi^2*" test of independence, year of survey"))
# segments(2,0,2,42)
# segments(-2,0,-2,42)
# 
# 
# # Mosaic plots for arrival year and 2008 ----------------------------------
# 
# # distribution of states in year of arrival, by country
# #str(prop.table(table(legstat.17nov$legstat_17nov1,legstat.17nov$ctry_yr),2))
# ctrylegstat1 <- xtabs(~legstat_17nov1 + ctry_yr, data=legstat.17nov)
# #summary(~legstat_17nov1 + ctry_yr, data=legstat.17nov, fun=table)
# 
# mosaic(ctrylegstat1, shade = TRUE, direction = "h", labeling_args = list(set_varnames = c(ctry_yr = "Country", legstat_17nov1 = "Legal status")), rot_labels = c(0,0,0,0), offset_labels = c(.5,.5,.5,1.4), offset_varnames = c(3,3,3,4),set_labels = list(legstat_17nov1 = c("Fully irreg.\nNRP_NWP", "Mixed no RP", "Mixed no WP", "Fully reg.\nRP_WP")))
# 
# # distribution of states in survey year, by country
# ctrylegstat08 <- xtabs(~legstat_17nov2008 + ctry_yr, data=legstat.17nov.calyr)
# #summary(~legstat_17nov1 + ctry_yr, data=legstat.17nov, fun=table)
# 
# mosaic(ctrylegstat08, shade = TRUE, direction = "h", labeling_args = list(set_varnames = c(ctry_yr = "Country", legstat_17nov2008 = "Legal status")), rot_labels = c(0,0,0,0), offset_labels = c(.5,.5,.5,1.5), offset_varnames = c(3,3,3,3),set_labels = list(legstat_17nov2008 = c("Fully irreg.\nNRP_NWP", "Mixed no RP", "Mixed no WP", "Fully reg.\nRP_WP")))
# 
# # 
# # ## Mosaic plots, first year
# # mosaic(prop.table(table(legstat.17nov$legstat_17nov1,legstat.17nov$ctry_yr)), direction = "V",split_vertical=TRUE)
# # 
# # mosaic.ctry.yr1 <- structable(legstat.17nov$ctry_yr ~ legstat_17nov1, data=legstat.17nov, labeling_args = list(set_varnames = c(ctry_yr = "Country", legstat_17nov1 = "Legal status")))
# # 
# # mosaic(mosaic.ctry.yr1, shade = TRUE, direction = "h",legend = TRUE, offset_labels = c(.5,.5,.5,.5), offset_varnames = c(3,3,3,3), rot_labels = c(0,0,0,45))#, labeling_args = list(set_varnames = legstat.17nov$ctry_yr = "A", legstat.17nov1 = "B", clip = FALSE) #varnames = c(FALSE, FALSE, FALSE, FALSE), 
# # 
# # mosaic(structable(legstat.17nov$ctry_yr ~ legstat.17nov$q1 + legstat_17nov1, data=legstat.17nov), shade = TRUE, direction = "h",legend = TRUE)
# # 
# # mosaic(ctry_yr ~ legstat_17nov1, data = legstat.17nov, shade = TRUE, labeling_args = list(set_varnames = c(ctry_yr = "Country", legstat_17nov1 = "Legal status")))
# # 
# # structable(legstat_17nov1 ~ legstat.17nov$ctry_yr, data=legstat.17nov)
# # 
# # mosaicplot(table(legstat.17nov$legstat_17nov1,legstat.17nov$ctry_yr), shade=TRUE,main="")
# # 
# # mosaicplot(table(legstat.17nov$legstat_17nov1,legstat.17nov$novisaenter), shade=TRUE,main="")
# # 
# # mosaicplot(table(legstat.17nov$legstat_17nov1,legstat.17nov$q1), shade=TRUE,main="")
# # 
# # mosaicplot(table(legstat.17nov$legstat_17nov1,legstat.17nov$firstmigdec_di2_nms), shade=TRUE,main="")
# # 
# # mosaicplot(table(legstat.17nov$legstat_17nov1,legstat.17nov$retmig), shade=TRUE,main="")
# # 
# # mosaicplot(table(legstat.17nov$novisaenter,legstat.17nov$retmig), shade=TRUE,main="")
# # 
# # mosaicplot(table(legstat.17nov$firstmigdec_di2_nms,legstat.17nov$retmig), shade=TRUE,main="")
# # 
# # ## Mosaic plots, 2008
# # mosaicplot(table(legstat.17nov.calyr$legstat_17nov2008,legstat.17nov.calyr$ctry_yr), shade=TRUE,main="")
# # 
# # mosaicplot(table(legstat.17nov.calyr$legstat_17nov2008,legstat.17nov.calyr$novisaenter), shade=TRUE,main="")
# # 
# # mosaicplot(table(legstat.17nov.calyr$legstat_17nov2008,legstat.17nov.calyr$q1), shade=TRUE,main="")
# # 
# # mosaic(structable(legstat.17nov.calyr$ctry_yr ~ legstat.17nov.calyr$q1 + legstat_17nov2008, data=legstat.17nov.calyr), shade = TRUE, direction = "h",legend = TRUE)
# # 
# # mosaic(structable(legstat.17nov$ctry_yr ~ legstat.17nov$q1 + cluster4 + novisaenter, data=legstat.17nov), shade = TRUE, direction = "h",legend = TRUE)
# 
# # Figure 6: Transversal state distribution frequencies  -------------------
# 
# 
# ## 6 - Figure 6: Transversal state distribution frequencies 
# seqdplot(legstat.17nov.seq,cex.legend=.85, with.missing=TRUE,xtlab=c(1:54), cex.plot=1,title="Transversal state distribution frequencies", 	xlab="Year in destination",cex.lab=.8)
# 
# seqlegend(legstat.17nov.seq)
# seqHtplot(legstat.17nov.seq,with.missing=TRUE)
# seqHtplot(legstat.17nov.seq,group=legstat.17nov$ctry_yr)
# seqdplot(legstat.17nov.seq,withlegend=FALSE,with.missing=TRUE)
# # legstat.apc.frsmyr.seq <- seqdef(legstat.17nov,2:2,right=NA,xtstep=5)
# # seqdplot(legstat.apc.frsmyr.seq)
# # seqstatd(legstat.apc.frsmyr.seq)
# # barplot(seqstatd(legstat.apc.frsmyr.seq)$Frequencies)
# seqIplot(legstat.17nov.seq,withlegend=TRUE, missing.color="white", 
#          group=legstat.17nov$ctry_yr,xtlab=c(1:54))
# seqdplot(legstat.17nov.seq,withlegend=TRUE,with.missing=TRUE,group=legstat.17nov$firstmigdec_di2_rev)
# 
# 
# # Fig 7 Transversal entropy index, all dest -------------------------------
# 
# seqHtplot(legstat.17nov.seq,title="Transversal entropy index, all destinations", xtlab=c(1:54))
# title(xlab="Year in destination")
# 
# seqHtplot(legstat.17nov.calyr.seq,title="Transversal entropy index, all destinations", xtlab=c(1950:2010))
# 	title(xlab="Year")
# 
# # Fig 8 Transversal state distribution frequencies, by dest ---------------
# mult.fig(mfrow=c(2,2))
# seqdplot(legstat.17nov.seq,cex.legend=.9,group=legstat.17nov$ctry_yr,border=NA,xtlab=c(1:54),with.missing=TRUE, xlab="Year in destination",cex.lab=.8)
# 
# ## same as above, but without censoring
# seqdplot(legstat.17nov.seq,cex.legend=.75,group=legstat.17nov$ctry_yr,border=NA,xtlab=c(1:54),with.missing=FALSE, xlab="Year in destination",cex.lab=.8)
# 
# 
# # Fig 9 Transversal entropy index by dest ---------------------------------
# 
# ## three separate plots
# seqHtplot(legstat.17nov.seq, xtlab=c(1:54),group=legstat.17nov$ctry_yr)
# 
# ## one plot overlaid like in paper?
# 
# par(mfrow=c(1,1))
# ##France
# plot(legstat.17nov.fr.statd$Entropy, axes=TRUE,col="blue",ylim=c(0,1), ylab="Entropy",xlab="Year in destination", pch = 2) 
# par(new=TRUE)
# lines(legstat.17nov.fr.statd$Entropy, axes=FALSE,col="blue", ylim=c(0,1), ylab="",xlab="",lwd = 2)
# 
# ##Italy
# par(new=TRUE)
# plot(legstat.17nov.it.statd$Entropy, axes=FALSE,col="red", ylim=c(0,1), ylab="",xlab="",pch = 0)
# par(new=TRUE)
# lines(legstat.17nov.it.statd$Entropy, axes=FALSE,col="red", ylim=c(0,1), ylab="",xlab="", lwd = 2)
# 
# ## Spain
# par(new=TRUE)
# plot(legstat.17nov.sp.statd$Entropy, axes=FALSE,col="green", ylim=c(0,1), ylab="",xlab="",pch = 9)
# par(new=TRUE)
# lines(legstat.17nov.sp.statd$Entropy, axes=FALSE,col="green", ylim=c(0,1), ylab="",xlab="", lwd = 2)
# 
# legend("topright",c("France","Italy","Spain"), col = c("blue","red","green"), pch = c(2,0,9), lty = c(1,1,1), lwd = c(2,2,2))
# 
# # Fig 10 Transversal state distributions, by initial dich lstat -----------
# 
# mult.fig(mfrow=c(1,2))
# seqdplot(legstat.17nov.seq,cex.legend=.65,group=legstat.17nov$novisaenter,border=NA,xtlab=c(1:54), with.missing=TRUE,xlab="Year in destination",cex.lab=.8,withlegend=FALSE,cex.plot=.75)
# 
# # Fig 11 Transversal entropy index by initial dich lstat --------
# 
# ## two separate plots
# seqHtplot(legstat.17nov.seq, xtlab=c(1:54),group=legstat.17nov$novisaenter)
# 
# ## two overlaid plots like in paper?
# legstat.17nov.novisa.statd <- seqstatd(legstat.17nov.seq[legstat.17nov$novisaenter == "No Visa",])
# legstat.17nov.visa.statd <- seqstatd(legstat.17nov.seq[legstat.17nov$novisaenter == "Visa",])
# 
# par(mfrow=c(1,1))
# ##No visa
# plot(legstat.17nov.novisa.statd$Entropy, axes=TRUE,col="blue",ylim=c(0,1), ylab="Entropy",xlab="Year in destination", pch = 2) 
# par(new=TRUE)
# lines(legstat.17nov.novisa.statd$Entropy, axes=FALSE,col="blue", ylim=c(0,1), ylab="",xlab="", lwd = 2)
# 
# ##Visa
# par(new=TRUE)
# plot(legstat.17nov.visa.statd$Entropy, axes=FALSE,col="red", ylim=c(0,1), ylab="",xlab="",pch = 0)
# par(new=TRUE)
# lines(legstat.17nov.visa.statd$Entropy, axes=FALSE,col="red", ylim=c(0,1), ylab="",xlab="", lwd = 2)
# 
# legend("topright",c("No Visa","Visa"), col = c("blue","red"), pch = c(2,0), lty = c(1,1), lwd = c(2,2))
# 
# ### Sequence distances ------------------------------------------------------
# 
# ## 17 DISTANCES
# ## DISTANCES
# 
# 
# legstat.17nov.dist.lcs <- seqdist(legstat.17nov.nomiss.seq, method = "LCS",norm=TRUE,with.missing=TRUE)
# ##legstat.apc.migyr.dist.lcs.wm <- seqdist(legstat.apc.migyr.seq, method = "LCS",norm=TRUE,with.missing=TRUE)
# legstat.17nov.trate.cost <- seqsubm(legstat.17nov.nomiss.seq, method = "TRATE",with.missing=TRUE)
# legstat.17nov.dist.om <- seqdist(legstat.17nov.nomiss.seq,method="OM",sm=legstat.17nov.trate.cost,norm=TRUE,with.missing=TRUE)
# ##legstat.apc.migyr.trate.cost.wm <- seqsubm(legstat.apc.migyr.seq, method = "TRATE",with.missing=TRUE)
# ##legstat.apc.migyr.dist.om <- seqdist(legstat.apc.migyr.seq,method="OM",sm=legstat.apc.migyr.trate.cost.wm,norm=TRUE,with.missing=TRUE)
# 
# 
# ### Distance clustering -----------------------------------------------------
# 
# 
# ## Clustering
# legstat.17nov.clust.lcs <- agnes(legstat.17nov.dist.lcs , diss = TRUE, method = "ward")
# ##legstat.apc.migyr.clust.lcs.wm <- agnes(legstat.apc.migyr.dist.lcs.wm , diss = TRUE, method = "ward")
# ##legstat.apc.migyr.pam.lcs <- pam(legstat.apc.migyr.dist.lcs, diss = TRUE, k=4)
# plot(legstat.17nov.clust.lcs, which.plots = 2,main="Clustering dendrogram for LCS sequence distances")
# bannerplot(legstat.17nov.clust.lcs)#, which.plots = 1, main="Banner plot for LCS sequence distances")
# dg.lcs <- as.dendrogram(legstat.17nov.clust.lcs)
# plot(dg,leaflab = c("none"))
# ##cluster4pam.apc.migyr.lcs <- legstat.apc.migyr.pam.lcs$clustering
# ##plot(legstat.apc.migyr.clust.lcs, which.plots = 2)
# legstat.17nov.clust.om <- agnes(legstat.17nov.dist.om, diss = TRUE, method = "ward")
# plot(legstat.17nov.clust.om, which.plots = 2)
# dg.om <- as.dendrogram(legstat.17nov.clust.om)
# plot(dg.om,leaflab = c("none"))
# ## ILLUSTRATION OF DISTANCES
# 
# seqiplot(legstat.17nov.nomiss.seq,tlim=501:505,withlegend=FALSE, xtlab=c(1:54),with.missing=FALSE)
# round(print(legstat.17nov.dist.lcs[501:505,501:505]),4)
# 
# 
# 
# 	
# 
# ## from here down need to add in the 4-cluster solution
# ## also need to save in the entropy stuff that's used below
# 
# 
# # Fig 12 Clustering dendrograms for sequence distances ---------------------
# ## DENDROGRAMS
# ## FIGURE 12
# mult.fig(mfrow=c(1,2),main="Clustering dendrograms for sequence distances")
# plot(dg.lcs,leaflab = c("none"), main="LCS distances")
# #plot(legstat.17nov.clust.lcs, which.plots = 2,main="LCS distances")
# ##plot(legstat.apc.migyr.clust.lcs.wm, which.plots = 2,main="LCS distances")
# plot(dg.om,leaflab = c("none"), main="OM distances")
# #plot(legstat.17nov.clust.om, which.plots = 2,main="OM distances")
# ## LCS and OM plots (without missing) seem similar
# par(mfrow=c(1,1))
# 
# # Create trajectory types -------------------------------------------------
# 
# 
# ## Deciding how many groups there are (based on dendrograms)
# 
# ## 4 groups
# cluster4.17nov.lcs <- cutree(legstat.17nov.clust.lcs, k = 4)
# ##cluster4.apc.migyr.lcs.wm <- cutree(legstat.apc.migyr.clust.lcs.wm, k = 4)
# cluster4.17nov.lcs <- factor(cluster4.17nov.lcs, labels = c("Type 1", "Type 2", "Type 3","Type 4"))
# table(cluster4.17nov.lcs)
# # cluster5.apc.migyr.lcs <- cutree(legstat.apc.migyr.clust.lcs, k = 5)
# # cluster5.apc.migyr.lcs <- factor(cluster4.apc.migyr.lcs, labels = c("Type 1", "Type 2", "Type 3","Type 4","Type 5"))
# # table(cluster5.apc.migyr.lcs, cluster4.apc.migyr.lcs)
# 
# ## 5 groups
# cluster5.17nov.lcs <- cutree(legstat.17nov.clust.lcs, k = 5)
# cluster5.17nov.lcs <- factor(cluster5.17nov.lcs, labels = c("Type 1", "Type 2", "Type 3", "Type 4", "Type 5"))
# table(cluster5.17nov.lcs)
# 
# ## 6 groups
# cluster6.17nov.lcs <- cutree(legstat.17nov.clust.lcs, k = 6)
# cluster6.17nov.lcs <- factor(cluster6.17nov.lcs, labels = c("Type 1", "Type 2", "Type 3","Type 4","Type 5","Type 6"))
# table(cluster6.17nov.lcs,cluster4.17nov.lcs)  
# # cluster7.apc.migyr.lcs <- cutree(legstat.apc.migyr.clust.lcs, k = 7)
# # cluster7.apc.migyr.lcs <- factor(cluster7.apc.migyr.lcs, labels = c("Type 1", "Type 2", "Type 3","Type 4","Type 5","Type 6","Type 7"))
# # cluster8.17nov.lcs <- cutree(legstat.17nov.clust.lcs, k = 8)
# # cluster8.17nov.lcs <- factor(cluster8.17nov.lcs, labels = c("Type 1", "Type 2", "Type 3","Type 4","Type 5","Type 6","Type 7","Type 8"))
# # table(cluster8.17nov.lcs,cluster6.17nov.lcs)  
# # seqmtplot(legstat.17nov.seq,group=cluster8.17nov.lcs,ylim=c(0,10))
# # cluster4.apc.migyr.om <- cutree(legstat.apc.migyr.clust.om, k = 4)
# # table(cluster4.apc.migyr.lcs,cluster4.apc.migyr.om)
# 
# ## add cluster vars to legstat.17nov data frame
# legstat.17nov$cluster4 <- cluster4.17nov.lcs
# legstat.17nov$cluster5 <- cluster5.17nov.lcs
# legstat.17nov$cluster6 <- cluster6.17nov.lcs
# 
# table(legstat.17nov$cluster4, legstat.17nov$cluster5)
# 
# ### Representative sequence based on distances ------------------------------
# 
# ## Representative sequences, based on distances
# seqrplot(legstat.17nov.nomiss.seq,dist.matrix=legstat.17nov.dist.lcs,withlegend=FALSE,cex.plot=1, xtlab = c(1:55))
# 
# seqrplot(legstat.17nov.nomiss.seq,dist.matrix=legstat.17nov.dist.lcs,withlegend=FALSE,cex.plot=.5, xtlab = c(1:55), group = legstat.17nov$ctry_yr)
# 
# seqrplot(legstat.17nov.nomiss.seq,dist.matrix=legstat.17nov.dist.lcs,withlegend=FALSE,cex.plot=.5, xtlab = c(1:55), group = legstat.17nov$novisaenter)
# 
# 
# seqrplot(legstat.17nov.nomiss.seq[cluster4.17nov.lcs=="Type 1",],dist.matrix=legstat.17nov.dist.lcs,withlegend=FALSE,cex.plot=1, xtlab = c(1:55))
# 
# seqrplot(legstat.17nov.nomiss.seq,dist.matrix=legstat.17nov.dist.lcs,withlegend=FALSE,cex.plot=.6, group=cluster4.17nov.lcs, xtlab = c(1:55))
# 
# seqrplot(legstat.17nov.nomiss.seq,dist.matrix=legstat.17nov.dist.lcs,withlegend=FALSE,cex.plot=.6, group=cluster5.17nov.lcs, xtlab = c(1:55))
# 
# seqrplot(legstat.17nov.nomiss.seq,dist.matrix=legstat.17nov.dist.lcs,withlegend=FALSE,cex.plot=.6, group=legstat.17nov$ctry_yr, xtlab = c(1:55))
# 
# seqrplot(legstat.17nov.nomiss.seq,dist.matrix=legstat.17nov.dist.lcs,withlegend=FALSE,cex.plot=.6, group=legstat.17nov$firstmigdec_di2_nms, xtlab = c(1:55))
# 
# seqrplot(legstat.17nov.nomiss.seq,dist.matrix=legstat.17nov.dist.lcs,withlegend=FALSE,cex.plot=.6, group=legstat.17nov$novisa, xtlab = c(1:55))
# 
# seqrplot(legstat.17nov.nomiss.seq,dist.matrix=legstat.17nov.dist.lcs,withlegend=FALSE,cex.plot=.6, xtlab = c(1:55))
# 
# # Fig 13 Sequence frequency plots, by type of legal-status traject --------
# 
# ## FIGURE 13: Sequence frequency plots, by type of legal-status trajectory
# seqfplot(legstat.17nov.nomiss.seq, group = cluster4.17nov.lcs, pbarw = T,xtlab=c(1:54))
# 
# seqfplot(legstat.17nov.nomiss.seq, group = cluster5.17nov.lcs, pbarw = T,xtlab=c(1:54), cex.lab = .75, with.missing = FALSE)
# 
# seqfplot(legstat.17nov.nomiss.seq, group = cluster6.17nov.lcs, pbarw = T,xtlab=c(1:54))
# 
# 
# # Fig 14 Mean time spent in each state, by type of legal status tr --------
# 
# ## FIGURE 14: Mean time spent in each state, by type of legal status trajectory
# seqmtplot(legstat.17nov.nomiss.seq, group = cluster4.17nov.lcs,ylim=c(0,11),axes=FALSE)
# 
# seqmtplot(legstat.17nov.nomiss.seq, group = cluster5.17nov.lcs,ylim=c(0,15),axes=FALSE)
# 
# seqmtplot(legstat.17nov.nomiss.seq, group = cluster6.17nov.lcs,ylim=c(0,10),axes=FALSE)
# 
# seqdplot(legstat.17nov.nomiss.seq, group = cluster4.17nov.lcs, xtlab=c(1:55))
# 
# seqdplot(legstat.17nov.nomiss.seq, group = cluster5.17nov.lcs, xtlab=c(1:55))
# 
# seqdplot(legstat.17nov.nomiss.seq, group = cluster6.17nov.lcs, xtlab=c(1:55))
# 
# # Fig 15 Sequence index plots, by type of legal status trajectory ---------
# ## FIGURE 15: Sequence index plots, by type of legal status trajectory
# tiff(file="myplot.tiff")
# seqIplot(legstat.17nov.nomiss.seq,group=cluster4.17nov.lcs,xtlab=c(1:54),withlegend=FALSE)#sortv=legstat.17nov.comp,
# 
# seqIplot(legstat.17nov.nomiss.seq,group=cluster5.17nov.lcs,xtlab=c(1:54),withlegend=TRUE, with.missing = FALSE)#sortv=legstat.17nov.comp,
# 
# seqIplot(legstat.17nov.nomiss.seq,group=cluster6.17nov.lcs,xtlab=c(1:54),withlegend=FALSE)#sortv=legstat.17nov.comp,
# 
# dev.off()
# ## this does a good job of showing the four groups:
# ## 1 = RP+WP
# ## 2 = RP only
# ## 3 = NV_NRP_NWP
# ## 4 = other
# 
# # Fig 16 Dist of types of leg stat trajectory, by dest --------------------
# ## FIGURE 16 Distribution of types of legal status trajectory, by destination
# 
# 
# legstat.17nov.pal5 <- brewer.pal(5, "Spectral")
# legstat.17nov.pal5 <- c("#D7191C", "#FDAE61", "#2B83BA","#FFFFBF", "#ABDDA4")
# 
# 
# addmargins(prop.table(table(cluster4.17nov.lcs,legstat.17nov$ctry_yr),2))
# 
# chisq.test(table(cluster4.17nov.lcs,legstat.17nov$ctry_yr))
# 
# barplot(prop.table(table(cluster4.17nov.lcs,legstat.17nov$ctry_yr),2), horiz=FALSE, beside=TRUE, #xlim=c(0,4.8),
#         #legend.text=c(levels(cluster4.17nov.lcs)),
#         #args.legend=list(cex=.8,horiz=FALSE,"center"), 
#         ylab="proportion", col=legstat.17nov.pal5)
#         #main="Distribution of types of legal status trajectory, by destination")## column percentages
# 
# barplot(prop.table(table(cluster5.17nov.lcs,legstat.17nov$ctry_yr),2), horiz=FALSE, beside=TRUE, #xlim=c(0,4.8),
#         legend.text=c(levels(cluster5.17nov.lcs)),
#         args.legend=list(cex=.8,horiz=FALSE,"center"), 
#         ylab="proportion", col=legstat.17nov.pal5)
#         #main="Distribution of types of legal status trajectory, by destination")## column percentages
# 
# ## Distribution of destinations, by type of legal status trajectory
# barplot(prop.table(table(legstat.17nov$ctry_yr,cluster4.17nov.lcs),2), horiz=FALSE, beside=TRUE, #xlim=c(0,9),
#         legend.text=c(levels(legstat.17nov$ctry_yr)),
#         args.legend=list(cex=.83,horiz=FALSE), cex.names=.7,names.arg=c(levels(cluster4n.17nov.lcs)),
#         ylab="proportion", xlab="Trajectory",
#         main=list("Distribution of destinations, by type of legal status trajectory", cex=1))## column percentages
# 
# ## Mosaic plots
# trajdest4 <- xtabs(~cluster4 + ctry_yr, data=legstat.17nov)
# 
# mosaic(trajdest4, shade = TRUE, direction = "h", labeling_args = list(set_varnames = c(ctry_yr = "Country", cluster4 = "Trajectory type")), rot_labels = c(0,0,0,0), offset_labels = c(.5,.5,.5,1.4), offset_varnames = c(3,3,3,4),set_labels = list(legstat_17nov1 = c("Fully irreg.\nNRP_NWP", "Mixed no RP", "Mixed no WP", "Fully reg.\nRP_WP")))
# 
# trajdest5 <- xtabs(~cluster5 + ctry_yr, data=legstat.17nov)
# 
# mosaic(trajdest5, shade = TRUE, direction = "h", labeling_args = list(set_varnames = c(ctry_yr = "Country", cluster5 = "Trajectory type")), rot_labels = c(0,0,0,0), offset_labels = c(.5,.5,.5,1.4), offset_varnames = c(3,3,3,4),set_labels = list(legstat_17nov1 = c("Fully irreg.\nNRP_NWP", "Mixed no RP", "Mixed no WP", "Fully reg.\nRP_WP")))
# 
# trajdest6 <- xtabs(~cluster6 + ctry_yr, data=legstat.17nov)
# 
# mosaic(trajdest6, shade = TRUE, direction = "h", labeling_args = list(set_varnames = c(ctry_yr = "Country", cluster6 = "Trajectory type")), rot_labels = c(0,0,0,0), offset_labels = c(.5,.5,.5,1.4), offset_varnames = c(3,3,3,4),set_labels = list(legstat_17nov1 = c("Fully irreg.\nNRP_NWP", "Mixed no RP", "Mixed no WP", "Fully reg.\nRP_WP")))
# 
# # Fig 17 Dist of types of leg stat trajectory, by initial stat ------------
# ## FIGURE 17 Distribution of types of legal status trajectory, by initial legal status
# addmargins(prop.table(table(cluster4.17nov.lcs,legstat.17nov$novisa),2))
# 
# chisq.test(table(cluster4.17nov.lcs,legstat.17nov$novisa))
# 
# barplot(prop.table(table(cluster4.17nov.lcs,legstat.17nov$novisa),2), horiz=FALSE, beside = TRUE, #xlim=c(0,3.3), 
#   legend.text=c(levels(cluster4n.17nov.lcs)),
# 	args.legend=list(cex=.9,horiz=FALSE,"center"), 
# 	ylab="proportion",col=legstat.17nov.pal,
# 	main="Distribution of types of legal status trajectory, by initial entry status")## column percentages
# 
# 
# barplot(prop.table(table(cluster5.17nov.lcs,legstat.17nov$novisa),2), horiz=FALSE, beside = TRUE, #xlim=c(0,3.3), 
#   legend.text=c(levels(cluster5.17nov.lcs)),
# 	args.legend=list(cex=.9,horiz=FALSE,"center"), 
# 	ylab="proportion", col=legstat.17nov.pal5)
# 	#main="Distribution of types of legal status trajectory, by initial entry status")## column percentages
# 
# 
# trajvisa4 <- xtabs(~cluster4 + novisaenter, data=legstat.17nov)
# 
# mosaic(trajvisa4, shade = TRUE, direction = "h", labeling_args = list(set_varnames = c(novisaenter = "Visa status", cluster4 = "Trajectory type")), rot_labels = c(0,0,0,0), offset_labels = c(.5,.5,.5,1.4), offset_varnames = c(3,3,3,4),set_labels = list(legstat_17nov1 = c("Fully irreg.\nNRP_NWP", "Mixed no RP", "Mixed no WP", "Fully reg.\nRP_WP")))
# 
# trajvisa5 <- xtabs(~cluster5 + novisaenter, data=legstat.17nov)
# 
# mosaic(trajvisa5, shade = TRUE, direction = "h", labeling_args = list(set_varnames = c(novisaenter = "Visa status", cluster5 = "Trajectory type")), rot_labels = c(0,0,0,0), offset_labels = c(.5,.5,.5,1.4), offset_varnames = c(3,3,3,4),set_labels = list(legstat_17nov1 = c("Fully irreg.\nNRP_NWP", "Mixed no RP", "Mixed no WP", "Fully reg.\nRP_WP")))
# 
# trajvisa6 <- xtabs(~cluster6 + novisaenter, data=legstat.17nov)
# 
# mosaic(trajvisa6, shade = TRUE, direction = "h", labeling_args = list(set_varnames = c(novisaenter = "Visa status", cluster6 = "Trajectory type")), rot_labels = c(0,0,0,0), offset_labels = c(.5,.5,.5,1.4), offset_varnames = c(3,3,3,4),set_labels = list(legstat_17nov1 = c("Fully irreg.\nNRP_NWP", "Mixed no RP", "Mixed no WP", "Fully reg.\nRP_WP")))
# 
# ## Distribution of visa status, by type of legal status trajectory
# barplot(prop.table(table(legstat.17nov$novisa,cluster4.17nov.lcs),2), horiz=FALSE, beside = TRUE, #xlim=c(0,9), 
# 	legend.text=c(levels(legstat.17nov$novisa)),
# 	args.legend=list(cex=.7,horiz=FALSE), 
# 	ylab="proportion", xlab="Trajectory",
# 	main=list("Distribution of inital legal status, by type of legal status trajectory", cex=1))## column percentages
# 	
# ## FIGURE 17 Distribution of types of legal status trajectory, by period of entry
# prop.table(table(cluster6.17nov.lcs,legstat.17nov$firstmigdec_di_85),2)
# chisq.test(table(cluster6.17nov.lcs,legstat.17nov$firstmigdec_di_85))
# barplot(prop.table(table(cluster6.17nov.lcs,legstat.17nov$firstmigdec_di_85),2), horiz=FALSE, xlim=c(0,3.3),
# 	legend.text=c(levels(cluster6n.17nov.lcs)),
# 	args.legend=list(cex=.9,horiz=FALSE), 
# 	ylab="proportion",col=cluster6.17nov.pal,
# 	main="Distribution of types of legal status trajectory, by period of entry")## column percentages
# barplot(prop.table(table(legstat.17nov$firstmigdec_di2_nms,cluster6.17nov.lcs),2), horiz=FALSE, xlim=c(0,9),
# 	legend.text=c(levels(legstat.17nov$firstmigdec_di2_nms)),
# 	args.legend=list(cex=.7,horiz=FALSE), cex.names=.7,names.arg=c(levels(cluster6n.17nov.lcs)),
# 	ylab="proportion",
# 	main="Distribution of types of legal status trajectory, by period of entry")## column percentages
# 
# trajper4 <- xtabs(~cluster4 + firstmigdec_di2_nms, data=legstat.17nov)
# 
# mosaic(trajper4, shade = TRUE, direction = "h", labeling_args = list(set_varnames = c(firstmigdec_di2_nms = "Period of arrival", cluster4 = "Trajectory type")), rot_labels = c(0,0,0,0), offset_labels = c(.5,.5,.5,1.4), offset_varnames = c(3,3,3,4),set_labels = list(legstat_17nov1 = c("Fully irreg.\nNRP_NWP", "Mixed no RP", "Mixed no WP", "Fully reg.\nRP_WP")))
# 
# trajper5 <- xtabs(~cluster5 + firstmigdec_di2_nms, data=legstat.17nov)
# 
# mosaic(trajper5, shade = TRUE, direction = "h", labeling_args = list(set_varnames = c(firstmigdec_di2_nms = "Period of arrival", cluster5 = "Trajectory type")), rot_labels = c(0,0,0,0), offset_labels = c(.5,.5,.5,1.4), offset_varnames = c(3,3,3,4),set_labels = list(legstat_17nov1 = c("Fully irreg.\nNRP_NWP", "Mixed no RP", "Mixed no WP", "Fully reg.\nRP_WP")))
# 
# trajper6 <- xtabs(~cluster6 + firstmigdec_di2_nms, data=legstat.17nov)
# 
# mosaic(trajper6, shade = TRUE, direction = "h", labeling_args = list(set_varnames = c(firstmigdec_di2_nms = "Period of arrival", cluster6 = "Trajectory type")), rot_labels = c(0,0,0,0), offset_labels = c(.5,.5,.5,1.4), offset_varnames = c(3,3,3,4),set_labels = list(legstat_17nov1 = c("Fully irreg.\nNRP_NWP", "Mixed no RP", "Mixed no WP", "Fully reg.\nRP_WP")))
# 
# # Table 4. Number of transitions, by destination and legal status ---------
# 
# legstat.17nov$ntrans <- seqtransn(legstat.17nov.seq)
# 
# transdesc1 <- describeBy(legstat.17nov$ntrans, group = legstat.17nov$ctry_yr, mat = TRUE)
# 
# transdesc2 <- describeBy(legstat.17nov$ntrans, group = legstat.17nov$novisaenter, mat = TRUE)
# 
# legstat.17nov$dummy <- 1
# transdesc3 <- describeBy(legstat.17nov$ntrans, group = legstat.17nov$dummy, mat = TRUE)
# 
# stargazer(transdesc1, transdesc2, type = "html", out = "ntrans_ctry.htm", summary = FALSE)
# 
# stargazer(transdesc3, type = "html", out = "ntrans_ctry_overall.htm", summary = FALSE)
# 
# # Table 5. Longitudinal transition matrix between legal statuses ----------
# legstat.17nov.transtab <- (seqtrate(legstat.17nov.seq))
# 
# #print(legstat.17nov.transtab, type="html", file="legst17nov_transtab.html")
# 
# stargazer(legstat.17nov.transtab, type="html", out="transtab.html")
# 
# # Table 6. Within-sequence complexity, by destination and init stat --------
# legstat.17nov$entr <- seqient(legstat.17nov.seq)
# legstat.17nov$turb <- seqST(legstat.17nov.seq) 
# legstat.17nov$compl <- seqici(legstat.17nov.seq)
# 
# ent1 <- describeBy(legstat.17nov$entr, group = legstat.17nov$ctry_yr, mat = TRUE)
# 
# turb1 <- describeBy(legstat.17nov$turb, group = legstat.17nov$ctry_yr, mat = TRUE)
# 
# comp1 <- describeBy(legstat.17nov$compl, group = legstat.17nov$ctry_yr, mat = TRUE)
# 
# ent2 <- describeBy(legstat.17nov$entr, group = legstat.17nov$novisaenter, mat = TRUE)
# 
# turb2 <- describeBy(legstat.17nov$turb, group = legstat.17nov$novisaenter, mat = TRUE)
# 
# comp2 <- describeBy(legstat.17nov$compl, group = legstat.17nov$novisaenter, mat = TRUE)
# 
# complexity <- list(mat1 = ent1, mat2 = turb1)
# complexity2 <- list(ent2, turb1, comp1)
# stargazer(complexity, type = "html", out = "ntrans_ctry.htm", summary = FALSE)
# 
# # Table 7  OLS regression of measures of within-sequence complexity -------
# 
# # Table 8: Multinomial logistic regression of types of trajectories --------
# 
# 
# 
# # mlogit(cluster6.apc.migyr.lcs ~ 1 | undocent + ctry_yr + male + wolof + mouride + villo_dk
#   # + married + kids + onepar_alive + siblings + eldest + fath_unemp
#   # + fath_lsecsch + migpaidfam + motive_work + def_stay
# 	# , data = legstat.apc.migyr, reflevel="1")
# 
# #reorder clusters for multinomial logit
# legstat.17nov$cluster4 <- relevel(legstat.17nov$cluster4, ref = "Type 2")
# 
# ## mlogit:
# mlogitdata <- mlogit.data(legstat.17nov, choice = "cluster4", shape = "wide", sep = "", alt.levels = c("Type 2", "Type 1", "Type 3", "Type 4")) #if above reflevel command isn't run, might need to reorder the alt.levels; best to check levels of cluster4
# 
# mlogit.model1 <- mlogit(cluster4 ~ 1 | q1 + firstmigdec_di2_nms + novisaenter, data = mlogitdata, reflevel = "Type 2")
# summary(mlogit.model1)
# 
# allEffects(mlogit.model1)
# stargazer(mlogit.model1,type = "html", out = "mlogit.htm")
# 
# ## with multinom from nnet package
# 
# mlogit.model<-multinom(legstat.17nov$cluster4 ~ q1 + firstmigdec_di2_nms + novisaenter, data=legstat.17nov)
# 
# summary(mlogit.model)
# 
# stargazer(mlogit.model,type = "html", out = "mlogit_nnet.htm") #this output is nicer
# 
# 
# plot(effect(c("q1"),mlogit.model),stacked="TRUE")
# 
# allEffects( mlogit.model)
# 
# plot(allEffects(mlogit.model)) #not useful
# 
# plot(effect("q1*firstmigdec_di2_nms", mlogit.model))#, style="stacked")
# 
# z <- summary(mlogit.model)$coefficients/summary(mlogit.model)$standard.errors
# # 2-tailed z test
# p <- (1 - pnorm(abs(z), 0, 1)) * 2
# 
# barplot(summary(mlogit.model)$coefficients[1:3,2:4],beside=TRUE,horiz=TRUE,names.arg=c("Sex (ref: male)","Arrival decade (ref: <1990)","No visa"),cex.names=.7,args.legend=list(cex=.7,horiz=FALSE),ylab="Predictor",xlab="Log odds",plot.ci=TRUE,ci.u=summary(mlogit.model)$coefficients+summary(mlogit.model)$standard.errors,ci.l=summary(mlogit.model)$coefficients-summary(mlogit.model)$standard.errors,legend=c("NRP_NWP","RP_NWP","NRP_WP"))
# #legend("bottom",mlogit.model$lab[2:4])
# abline(v=0)
# 
# ## error bars for multinomial coefficients
# error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
#   if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
#     stop("vectors must be same length")
#   arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
# }
# 
# mlogit.model3 <- vglm(cluster4 ~ as.factor(q1) + as.factor(firstmigdec_di2_nms) + as.factor(novisaenter), data=legstat.17nov, multinomial)
# summary(mlogit.model3)
# rowMeans(margeff(mlogit.model3))
# 
# # Mean sequence length, by sequence type ----------------------------------
# 
# 
# 
# tapply(seqlength(legstat.17nov.nomiss.seq), cluster4.17nov.lcs, summary)
# #summary(seqlength(legstat.17nov.nomiss.seq)by=cluster4.17nov.lcs)
# 
# mean(seqlength(legstat.17nov.nomiss.seq))
# tapply(seqlength(legstat.17nov.nomiss.seq), cluster4.17nov.lcs, mean)
# barplot(tapply(seqlength(legstat.17nov.nomiss.seq), cluster4.17nov.lcs, mean))
# 
# 
# # Complexity and entropy --------------------------------------------------
# 
# 
# 
# ## Complexity and entropy
# 
# cmp<-seqici(legstat.17nov.seq)
# hist(cmp)
# ent<-seqient(legstat.17nov.seq)
# hist(ent)
# 
# plot(cmp,ent)
# barplot(tapply(cmp, legstat.17nov$q1, mean))
# barplot(tapply(ent, legstat.17nov$q1, mean))
# barplot(tapply(cmp, legstat.17nov$ctry_yr, mean))
# barplot(tapply(ent, legstat.17nov$ctry_yr, mean))
# 
# seqpcplot(legstat.17nov.seq,order.align="time")
# seqpcplot(legstat.17nov.seq,order.align="first")
# seqpcplot(legstat.17nov.seq,order.align="last")
# seqt <- seqtree(legstat.17nov.nomiss.seq ~ ctry_yr + q1 + firstmigdec_di2_nms + novisaenter + retmig, data=legstat.17nov,R=10,diss=legstat.17nov.dist.lcs)
# seqtreedisplay(seqt,seqdata=legstat.17nov.nomiss.seq)
# 
# ## 13 - WITHIN-SEQUENCE ENTROPY, TURBULENCE, AND COMPLEXITY
# # legstat.apc.migyr.nomiss.seq <- seqdef(legstat.apc.migyr,2:54,right="DEL",xtstep=5,cpal=pal23.apc)
# # seqdplot(legstat.apc.migyr.nomiss.seq, with.missing=FALSE)
# 
# mult.fig(mfrow=c(3,2))
# hist(seqient(legstat.17nov.seq,with.missing=FALSE),main="Entropy, no missing",col="lavender",ylim=c(1,250))
# hist(seqient(legstat.17nov.seq,with.missing=TRUE),main="Entropy, with missing",col="salmon",ylim=c(1,250))
# hist(seqST(legstat.17nov.nomiss.seq),main="Turbulence, no missing",col="lavender",ylim=c(1,250))
# hist(seqST(legstat.17nov.seq),main="Turbulence, with missing",col="salmon",ylim=c(1,250))
# hist(seqici(legstat.17nov.seq,with.missing=FALSE),main="Complexity, no missing",col="lavender",ylim=c(1,250))
# hist(seqici(legstat.17nov.seq,with.missing=TRUE),main="Complexity, with missing",col="salmon",ylim=c(1,250))
# 
# 
# # start here --------------------------------------------------------------
# #most frequent sequences
# seqtab(legstat.17nov.seq)
# #transition rates
# seqtrate(legstat.17nov.seq)
# 
# legstat.17nov.ent<-seqient(legstat.17nov.seq)
# 
# # seqmtplot(legstat.apc.migyr.nomiss.seq, group = cluster6.apc.migyr.lcs,ylim=c(0,10),axes=FALSE,withlegend=FALSE)
# # seqmtplot(legstat.apc.migyr.nomiss.seq, group = cluster4pam.apc.migyr.lcs,ylim=c(0,10),axes=FALSE,cex.legend=.6)
# # seqmtplot(legstat.apc.migyr.seq, group = cluster4.apc.migyr.lcs.wm,ylim=c(0,20),axes=FALSE,with.missing=TRUE)
# # seqrplot(legstat.apc.migyr.nomiss.seq,dist.matrix=legstat.apc.migyr.dist.lcs,group=cluster6.apc.migyr.lcs,withlegend=FALSE,cex.font=.4)
# 
# # seqdplot(legstat.apc.migyr.seq[legstat.apc.migyr$undocent=="Undocumented" ,],withlegend=FALSE,border=NA,xtlab=c(1:54),
# 	# with.missing=TRUE, xlab="Year in destination",cex.lab=.8)
# 
# # not sure what this is #################
# cluster6n.17nov.lcs <- cluster6.17nov.lcs
# cluster6n.17nov.lcs <- factor(cluster6n.17nov.lcs, labels = c("NRP_NWP", "RP_WP1", "RP_NWP","Type 4","RP_WP2","NRP_WP"))
# cluster6.17nov.pal <- legstat.17nov.pal[c(10,6,9,1,2,8)]
# boxplot(legstat.17nov.ent~cluster6.17nov.lcs)
# boxplot(legstat.17nov.ent~cluster4.17nov.lcs)
# dotchart(mean(legstat.17nov.ent), group=cluster6.17nov.lcs)
# 
# 
# # check this code vs fig 16 above -----------------------------------------
# 
# 
# ## FIGURE 16 Distribution of types of legal status trajectory, by destination
# addmargins(prop.table(table(cluster6.17nov.lcs,legstat.17nov$ctry_yr),2))
# chisq.test(table(cluster6.17nov.lcs,legstat.17nov$ctry_yr))
# barplot(prop.table(table(cluster6.17nov.lcs,legstat.17nov$ctry_yr),2), horiz=FALSE, xlim=c(0,4.8),
# 	legend.text=c(levels(cluster6n.17nov.lcs)),
# 	args.legend=list(cex=.8,horiz=FALSE,"center"), 
# 	ylab="proportion", col=cluster6.17nov.pal,
# 	main="Distribution of types of legal status trajectory, by destination")## column percentages
# 
# 
# # Distribution of destinations, by type of traj ---------------------------
# 
# 
# ## Distribution of destinations, by type of legal status trajectory
# barplot(prop.table(table(legstat.17nov$ctry_yr,cluster6.17nov.lcs),2), horiz=FALSE, xlim=c(0,9),
# 	legend.text=c(levels(legstat.17nov$ctry_yr)),
# 	args.legend=list(cex=.83,horiz=FALSE), 
# 	ylab="proportion", xlab="Trajectory",
# 	main=list("Distribution of destinations, by type of legal status trajectory", cex=1))## column percentages
# 
# 	
# 
# # Entropy by trajectory ---------------------------------------------------
# 
# 
# cluster4n.17nov.lcs <- cluster4.17nov.lcs
# cluster4n.17nov.lcs <- factor(cluster4n.17nov.lcs, labels = c("NRP_NWP", "RP_WP", "RP_NWP","NRP_WP"))
# cluster6.17nov.pal <- legstat.17nov.pal[c(10,6,9,1,2,8)]
# boxplot(legstat.17nov.ent~cluster4.17nov.lcs,axes=FALSE)
# axis(1, at = 1:4, labels = c(levels(cluster4n.17nov.lcs)))
# axis(2)
# 
# dotchart(mean(legstat.17nov.ent), group=cluster4.17nov.lcs)
# 
# 
# # check this code vs. fig 17 code -----------------------------------------
# 
# 
# ## FIGURE 17 Distribution of types of legal status trajectory, by initial legal status
# addmargins(prop.table(table(cluster4.17nov.lcs,legstat.17nov$novisa),2))
# chisq.test(table(cluster4.17nov.lcs,legstat.17nov$novisa))
# barplot(prop.table(table(cluster4.17nov.lcs,legstat.17nov$novisa),2), horiz=FALSE, beside=TRUE,#xlim=c(0,3.3),
#         legend.text=c(levels(cluster4n.17nov.lcs)),
#         args.legend=list(cex=.9,horiz=FALSE,"center"), 
#         ylab="proportion",#col=cluster6.17nov.pal,
#         main="Distribution of types of legal status trajectory, by initial entry status")## column percentages
# ## Distribution of visa status, by type of legal status trajectory
# barplot(prop.table(table(legstat.17nov$novisa,cluster4.17nov.lcs),2), horiz=FALSE, beside=TRUE,#xlim=c(0,9),
#         legend.text=c(levels(legstat.17nov$novisa)),
#         args.legend=list(cex=.7,horiz=FALSE), cex.names=.7,names.arg=c(levels(cluster4n.17nov.lcs)),
#         ylab="proportion", xlab="Trajectory",
#         main=list("Distribution of inital legal status, by type of legal status trajectory", cex=1))## column percentages
# 
# ## FIGURE 17 Distribution of types of legal status trajectory, by period of entry
# prop.table(table(cluster4.17nov.lcs,legstat.17nov$firstmigdec_di_85),2)
# chisq.test(table(cluster4.17nov.lcs,legstat.17nov$firstmigdec_di_85))
# barplot(prop.table(table(cluster4.17nov.lcs,legstat.17nov$firstmigdec_di_85),2), horiz=FALSE, beside=TRUE,#xlim=c(0,3.3),
#         legend.text=c(levels(cluster4n.17nov.lcs)),
#         args.legend=list(cex=.9,horiz=FALSE), 
#         ylab="proportion",#col=cluster6.17nov.pal,
#         main="Distribution of types of legal status trajectory, by period of entry")## column percentages
# barplot(prop.table(table(legstat.17nov$firstmigdec_di2_nms,cluster4.17nov.lcs),2), horiz=FALSE, beside=TRUE,#xlim=c(0,9),
#         legend.text=c(levels(legstat.17nov$firstmigdec_di2_nms)),
#         args.legend=list(cex=.7,horiz=FALSE), cex.names=.7,names.arg=c(levels(cluster4n.17nov.lcs)),
#         ylab="proportion",
#         main="Distribution of types of legal status trajectory, by period of entry")## column percentages
# 
# 
# # Distribution of types of legal status trajectory, by sex ----------------
# 
# 
# ##NEED GENDER VARIABLE!!!!
# 
# q1new<-legstat.17nov$q1[legstat.17nov$q1!="Refuse to answer" | legstat.17nov$q1!="No answer" | legstat.17nov$q1!="Dont know"]
# q1new<-factor(q1new)
# legstat.17nov$q1<-q1new
# 
# prop.table(table(cluster4.17nov.lcs,legstat.17nov$firstmigdec_di_85),2)
# chisq.test(table(cluster4.17nov.lcs,legstat.17nov$firstmigdec_di_85))
# barplot(prop.table(table(cluster4.17nov.lcs,legstat.17nov$q1),2), horiz=FALSE, beside=TRUE,#xlim=c(0,3.3),
#         legend.text=c(levels(cluster4n.17nov.lcs)),
#         args.legend=list(cex=.9,horiz=FALSE), 
#         ylab="proportion",#col=cluster6.17nov.pal,
#         main="Distribution of types of legal status trajectory, by gender")## column percentages
# barplot(prop.table(table(legstat.17nov$q1,legstat.17nov$cluster4),2), horiz=FALSE, beside=TRUE,#xlim=c(0,9),
#         legend.text=c(levels(legstat.17nov$q1)),
#         args.legend=list(cex=.7,horiz=FALSE), cex.names=.7,names.arg=c(levels(cluster4n.17nov.lcs)),
#         ylab="proportion",
#         main="Distribution of types of legal status trajectory, by gender")## column percentages
# 
# 
# 
# 
# 	
# # prop.table(table(cluster6.apc.migyr.lcs,legstat.apc.migyr$male),2)
# # chisq.test(table(cluster6.apc.migyr.lcs,legstat.apc.migyr$male))
# # barplot(prop.table(table(cluster6.apc.migyr.lcs,legstat.apc.migyr$male),2), horiz=FALSE,
# 	# legend.text=c(levels(cluster6.apc.migyr.lcs)),
# 	# args.legend=list(x=.75,y=2.58,cex=.8,horiz=TRUE),
# 	# xlab="proportion",
# 	# main="Distribution of types of legal status trajectory, by sex")## column percentages
# 
# 
# 
# # Stata export ------------------------------------------------------------
# 
# 
# ## OUTPUT DATA TO STATA FOR FURTHER ANALYSIS (SEE STATA DO FILE FOR TABLE 8)
# # legstat.17nov.stataexp <- NULL
# # legstat.17nov.stataexp$ident_nmspell <- cbind(legstat.17nov$ident_nmspell)
# # ##legstat.17nov.stataexp$turb_17nov <- cbind(legstat.17nov.turb)
# # ##legstat.17nov.stataexp$ent_17nov <- cbind(legstat.17nov.ent)
# # ##legstat.17nov.stataexp$ci_17nov <- cbind(legstat.17nov.comp)
# # ##legstat.17nov.stataexp$clust4lcs_17nov <- cbind(cluster4.17nov.lcs)
# # #legstat.17nov.stataexp$clust4om_17nov <- cbind(cluster4.17nov.om)
# # legstat.17nov.stataexp$clust6lcs_17nov <- cbind(cluster6.17nov.lcs)
# # #legstat.17nov.stataexp$clust6om_17nov <- cbind(cluster6.17nov.om)
# # legstat.17nov$transn_17nov_nn <- seqtransn(legstat.17nov.seq, with.missing=FALSE)
# # legstat.17nov$transn_17nov_norm <- seqtransn(legstat.17nov.seq, with.missing=FALSE,norm=TRUE)
# # legstat.17nov.stataexp$transn_nn_17nov <- cbind(legstat.17nov$transn_17nov_nn)
# # legstat.17nov.stataexp$transn_n_17nov <- cbind(legstat.17nov$transn_17nov_norm)
# # legstat.17nov.stataexp$novisa <- cbind(legstat.17nov$novisa)
# # names(legstat.17nov.stataexp)
# # class(legstat.17nov.stataexp$clust6lcs_apc)
# # 
# # 
# # write.foreign(legstat.17nov,datafile="legstat_17nov_stataexp.csv",
# # 	codefile="legstat_17nov_stataexp_5april15.do", package="Stata")
# 
# legstat.17nov$turb2 <- legstat.17nov$turb
# legstat.17nov$ent2 <- legstat.17nov$ent
# write.dta(legstat.17nov,paste0(currentpath,"\\legstat_17nov_stataexp.dta"))
# 
# 
# # Discrepancy analysis ----------------------------------------------------
# 
# 
# ## DISCREPANCY ANALYSIS
# dissassoc(legstat.17nov.dist.lcs, group=legstat.17nov$novisaenter)
# dissassoc(legstat.17nov.dist.lcs, group=legstat.17nov$ctry_yr)
# plot(dissassoc(legstat.17nov.dist.lcs, group=legstat.17nov$ctry_yr))
# dissassoc(legstat.17nov.dist.lcs, group=legstat.17nov$firstmigdec_di2_nms)
# dissassoc(legstat.17nov.dist.lcs, group=legstat.17nov$q1)
# # dissassoc(legstat.17nov.dist.lcs, group=legstat.17nov$male)
# # dissassoc(legstat.apc.migyr.dist.lcs, group=legstat.apc.migyr$motive_work)
# # dissassoc(legstat.apc.migyr.dist.lcs, group=legstat.apc.migyr$villo_dk)
# # dissassoc(legstat.apc.migyr.dist.lcs, group=legstat.apc.migyr$fath_lsecsch)
# # dissassoc(legstat.apc.migyr.dist.lcs, group=legstat.apc.migyr$def_stay)
# 
# dissassoc(legstat.17nov.dist.lcs, group=cluster4.17nov.lcs)
# dissassoc(legstat.17nov.dist.lcs, group=cluster6.17nov.lcs)
# plot(dissassoc(legstat.17nov.dist.lcs, group=cluster6.17nov.lcs))
# 
# # dissassoc(legstat.apc.migyr.dist.lcs, group=cluster4pam.apc.migyr.lcs)
# 
# # dissassoc(legstat.apc.migyr.dist.lcs.wm, group=legstat.apc.migyr$undocent)
# # dissassoc(legstat.apc.migyr.dist.lcs.wm, group=legstat.apc.migyr$ctry_yr)
# # dissassoc(legstat.apc.migyr.dist.lcs.wm, group=legstat.apc.migyr$firstmigdec_di2_rev)
# 
# disctest.17nov<-dissmfacw(legstat.17nov.dist.lcs~novisaenter, data = legstat.17nov)
# print(disctest.17nov)
# 
# 
# # legstat.dissmfacw <- dissmfacw(legstat.apc.migyr.dist.lcs~undocent + ctry_yr + male + wolof + mouride + villo_dk
# 	# + married + kids + onepar_alive + siblings + eldest + fath_unemp
# 	# + fath_lsecsch + migpaidfam + motive_work + def_stay
# 	# , data = legstat.apc.migyr)
# # print(legstat.dissmfacw)
# 
# dissmfacw.legstat17nov <- dissmfacw(legstat.17nov.dist.lcs~novisaenter + ctry_yr + firstmigdec_di2_nms + q1, data = legstat.17nov)
# print(dissmfacw.legstat17nov)
# 
# # treetest <- seqtree(legstat.apc.migyr.nomiss.seq~undocent + ctry_yr + male + wolof + mouride + villo_dk
# 	# + married + kids + onepar_alive + siblings + eldest + fath_unemp
# 	# + fath_lsecsch + migpaidfam + motive_work + def_stay
# 	# , data = legstat.apc.migyr, diss = legstat.apc.migyr.dist.lcs)
# 
# # seqtreedisplay(treetest, type="I", sortv=cmdscale(sqrt(legstat.apc.migyr.dist.lcs),k=1))
# # shell("dot -Tsvg -O mytree.dot")
# 
# 
# plot(seqdiff(legstat.17nov.nomiss.seq,with.missing=TRUE,group=cluster4.17nov.lcs), stat="discrepancy",lwd=3,xtlab=c(1:54)) ## stat="discrepancy" gives same plot as stat="Variance" 
# 
# legstat.seqdif <- seqdiff(legstat.17nov.nomiss.seq,with.missing=TRUE,group=legstat.17nov$ctry_yr)
# print(legstat.seqdif)
# plot(legstat.seqdif, stat=c("Pseudo R2", "Levene"), xtstep=5)
# plot(legstat.seqdif, stat="discrepancy")	
# 
# legstat.seqdif.un <- seqdiff(legstat.17nov.nomiss.seq,with.missing=TRUE,group=legstat.17nov$novisaenter)
# print(legstat.seqdif.un)
# plot(legstat.seqdif.un, stat=c("Pseudo R2", "Levene"), xtstep=5)
# plot(legstat.seqdif.un, stat="discrepancy")	
# 
# legstat.seqdif.per <- seqdiff(legstat.17nov.nomiss.seq,with.missing=TRUE,group=legstat.17nov$firstmigdec_di2_nms)
# print(legstat.seqdif.per)
# plot(legstat.seqdif.per, stat=c("Pseudo R2", "Levene"), xtstep=5)
# plot(legstat.seqdif.per, stat="discrepancy")	
# 
# 
# # supplemental figures for calyr? ---------------------------------------------
# 
# 
# 
# ## supplemental figures
# ## 5 - READ STATA DATASET: CALENDAR YEAR
# legstat.17nov.calyr <- read.dta("C:\\Users\\Erik\\Dropbox\\MAFE\\sequence_legstat_17nov_wide_calyr_23apr2013.dta")
# names(legstat.17nov.calyr)
# legstat.17nov.calyr$ctry_yr <- factor(legstat.17nov.calyr$ctry_yr, levels=c("FRANCE","ITALIE","ESPAGNE"))
# 
# legstat.17nov.calyr$novisaenter <- legstat.17nov$novisaenter
# legstat.17nov.calyr$q1 <- legstat.17nov$q1
# 
# table(legstat.17nov.calyr$ctry_yr,legstat.17nov.calyr$id_country)
# addmargins(table(legstat.17nov.calyr$id_country,legstat.17nov.calyr$ctry_yr))
# addmargins(table(legstat.17nov.calyr$ctry_yr))
# 
# 
# seqdplot(legstat.17nov.calyr.seq,withlegend=TRUE,with.missing=TRUE,xtlab=c(1950:2010),cex.legend=.8)
# seqdplot(legstat.17nov.calyr.seq,withlegend=TRUE,with.missing=FALSE,xtlab=c(1950:2010),cex.legend=.9,group=legstat.17nov.calyr$ctry_yr)
# 
# seqHtplot(legstat.17nov.calyr.seq,with.missing=TRUE,xtlab=c(1950:2010))
# seqHtplot(legstat.17nov.calyr.seq,group=legstat.17nov.calyr$ctry_yr,xtlab=c(1950:2010))
# # legstat.apc.frsmyr.seq <- seqdef(legstat.apc.migyr,2:2,right=NA,xtstep=5)
# # seqdplot(legstat.apc.frsmyr.seq)
# # seqstatd(legstat.apc.frsmyr.seq)
# # barplot(seqstatd(legstat.apc.frsmyr.seq)$Frequencies)
# seqIplot(legstat.17nov.calyr.seq,withlegend=TRUE,cex.legend=.8,xtlab=c(1950:2010),missing.color="black")
# seqIplot(legstat.17nov.calyr.seq,withlegend=TRUE,cex.legend=.8,xtlab=c(1950:2010),missing.color="black",group=legstat.17nov.calyr$firstmigdec_di2_nms)
# seqdplot(legstat.17nov.calyr.seq,withlegend=FALSE,with.missing=FALSE,group=legstat.17nov.calyr$firstmigdec_di2_nms,xtlab=c(1950:2010))
# 
