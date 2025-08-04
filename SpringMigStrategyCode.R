# run spring mig metric ANOVAs, evaluate w/in sample variation, quantify relationships between winter mvmt and spring migration


rm(list = ls())

library(tidyverse)  ## data manipulation


# read in formatted data ----
# data for quantifying interspecific and interindividual variation in spring migration strategy and carryover effects
mig.metrics <- readRDS("./InterspecificInterindividualVariationData.RDS") %>% 
  glimpse()
  ## individual: individual identifier (transmitter ID number)
  ## species: abdu = American Black Duck, atbr = Atlantic Brant, grsc = Greater Scaup
  ## sex: F = female, M = male
  ## age: ASY = after second year (adult), SY = second year (juvenile)
  ## mig.dist.km: sum of distances for all 24 hr movements from initiation to completion of spring migration to a suspected breeding site (km)
  ## breed.lat: latitude of individuals identified settled breeding location (decimal degrees)
  ## initiation.odate: ordinal spring migration initiation date
  ## completion.odate: ordinal spring migration completion date
  ## mig.duration.days: spring migration duration; difference in days between ordinal spring migration completion and initiation dates
  ## numb.stopover: the total number of unique stopover events identified
  ## avg.stopdur: average duration (days) of unique stopover events
  ## prop.stop: proportion of migration time (days) spent in stopover; total stopover duration divided by migration duration
  ## stop.travel.rat: stopover to travel ratio (days/km); total number of days stopped over divided total migration distance
  ## wint.daydist.km: average daily winter movement distance (km)
  ## area.sqkm: winter home range (95% wAKDE) size (km^2)


# data for quantifying annual variation in spring migration strategy
annual.var <- readRDS("./AnnualVariationData.RDS") %>% 
  glimpse()
  ## individual: individual identifier (transmitter ID number)
  ## sex: F = female, M = male
  ## age: ASY = after second year (adult), SY = second year (juvenile)
  ## species: abdu = American Black Duck, atbr = Atlantic Brant, grsc = Greater Scaup
  ## mig.year: one = individual's first spring migration, two = individual's second spring migration
  ## initiation.odate: ordinal spring migration initiation date
  ## completion.odate: ordinal spring migration completion date
  ## mig.duration.days: spring migration duration; difference in days between ordinal spring migration completion and initiation dates
  ## numb.stopover: the total number of unique stopover events identified
  ## avg.stopdur: average duration (days) of unique stopover events
  ## prop.stop: proportion of migration time (days) spent in stopover; total stopover duration divided by migration duration
  ## stop.travel.rat: stopover to travel ratio (days/km); total number of days stopped over divided total migration distance




# quantify interspecific and interindividual variation in spring migration metrics ----
## ordinal spring migration initiation date
  # interspecific variation (ANOVA and TukeyHSD)
  initiation.aov <- aov(initiation.odate ~ species, data = mig.metrics)
  summary(initiation.aov)
  TukeyHSD(initiation.aov)
  
  # interindividual variation (SD)
  init.summary <- data.frame(aggregate(mig.metrics$initiation.odate, by = list(mig.metrics$species), FUN = mean)) %>% 
    rename(species = Group.1, avg = x)
  init.summary$sd <- data.frame(aggregate(mig.metrics$initiation.odate, by = list(mig.metrics$species), FUN = sd))[1:3,2]
  init.summary$metric <- "initiation.odate"
  init.summary


## ordinal spring migration completion date
  # interspecific variation (ANOVA and TukeyHSD)
  compl.aov <- aov(completion.odate ~ species, data = mig.metrics)
  summary(compl.aov)
  TukeyHSD(compl.aov)
  
  # interindividual variation (SD)
  compl.summary <- data.frame(aggregate(mig.metrics$completion.odate, by = list(mig.metrics$species), FUN = mean)) %>% 
    rename(species = Group.1, avg = x)
  compl.summary$sd <- data.frame(aggregate(mig.metrics$completion.odate, by = list(mig.metrics$species), FUN = sd))[1:3,2]
  compl.summary$metric <- "completion.odate"
  compl.summary


## spring migration duration
  # interspecific variation (ANOVA and TukeyHSD)
  migdur.aov <- aov(mig.duration.days ~ species, data = mig.metrics)
  summary(migdur.aov)
  TukeyHSD(migdur.aov)
  
  # interindividual variation (SD)
  duration.summary <- data.frame(aggregate(mig.metrics$mig.duration.days, by = list(mig.metrics$species), FUN = mean)) %>% 
    rename(species = Group.1, avg = x)
  duration.summary$sd <- data.frame(aggregate(mig.metrics$mig.duration.days, by = list(mig.metrics$species), FUN = sd))[1:3,2]
  duration.summary$metric <- "mig.duration.days"
  duration.summary


## number of stopovers
  # interspecific variation (ANOVA and TukeyHSD)
  numbstop.aov <- aov(numb.stopover ~ species, data = mig.metrics)
  summary(numbstop.aov)
  TukeyHSD(numbstop.aov)
  
  # interindividual variation (SD)
  nstop.summary <- data.frame(aggregate(mig.metrics$numb.stopover, by = list(mig.metrics$species), FUN = mean)) %>% 
    rename(species = Group.1, avg = x)
  nstop.summary$sd <- data.frame(aggregate(mig.metrics$numb.stopover, by = list(mig.metrics$species), FUN = sd))[1:3,2]
  nstop.summary$metric <- "numb.stopover"
  nstop.summary


## average stopover duration
  # interspecific variation (ANOVA and TukeyHSD)
  avgstopdur.aov <- aov(avg.stopdur ~ species, data = mig.metrics)
  summary(avgstopdur.aov)
  TukeyHSD(avgstopdur.aov)
  
  # interindividual variation (SD)
  avgstop.summary <- data.frame(aggregate(mig.metrics$avg.stopdur, by = list(mig.metrics$species), FUN = mean)) %>% 
    rename(species = Group.1, avg = x)
  avgstop.summary$sd <- data.frame(aggregate(mig.metrics$avg.stopdur, by = list(mig.metrics$species), FUN = sd))[1:3,2]
  avgstop.summary$metric <- "avg.stopdur"
  avgstop.summary


## proportion of migration in stopover
  # interspecific variation (ANOVA and TukeyHSD)
  propstop.aov <- aov(prop.stop ~ species, data = mig.metrics)
  summary(propstop.aov)
  TukeyHSD(propstop.aov)
  
  # interindividual variation (SD)
  propstop.summary <- data.frame(aggregate(mig.metrics$prop.stop, by = list(mig.metrics$species), FUN = mean)) %>% 
    rename(species = Group.1, avg = x)
  propstop.summary$sd <- data.frame(aggregate(mig.metrics$prop.stop, by = list(mig.metrics$species), FUN = sd))[1:3,2]
  propstop.summary$metric <- "prop.stop"
  propstop.summary


## stopover to travel ratio
  # interspecific variation (ANOVA and TukeyHSD)
  stoptravrat.aov <- aov(stop.travel.rat ~ species, data = mig.metrics)
  summary(stoptravrat.aov)
  TukeyHSD(stoptravrat.aov)
  
  # interindividual variation (SD)
  mig.metrics.nona <- na.omit(mig.metrics)
  stoptravel.summary <- data.frame(aggregate(mig.metrics.nona$stop.travel.rat, by = list(mig.metrics.nona$species), FUN = mean)) %>%
    rename(species = Group.1, avg = x)
  stoptravel.summary$sd <- data.frame(aggregate(mig.metrics.nona$stop.travel.rat, by = list(mig.metrics.nona$species), FUN = sd))[1:3,2]
  stoptravel.summary$metric <- "stop.travel.rat"
  stoptravel.summary
  


# quantify annual variation in spring migration metrics ----
## ordinal spring migration initiation date
  init.annual.var <- annual.var %>% 
    select(individual, species, initiation.odate, mig.year) %>% 
    tidyr::pivot_wider(names_from = mig.year, values_from = initiation.odate) %>% 
    mutate(initiation.reldiff = abs(one-two)/(one+two)) %>% 
    rename(initiation.1 = one, initiation.2 = two)
  
## ordinal spring migration completion date
  arriv.annual.var <- annual.var %>% 
    select(individual, completion.odate, mig.year) %>% 
    tidyr::pivot_wider(names_from = mig.year, values_from = completion.odate) %>% 
    mutate(completion.reldiff = abs(one-two)/(one+two)) %>% 
    rename(completion.1 = one, completion.2 = two)
  
## spring migration duration
  migdur.annual.var <- annual.var %>% 
    select(individual, mig.duration.days, mig.year) %>% 
    tidyr::pivot_wider(names_from = mig.year, values_from = mig.duration.days) %>% 
    mutate(migdur.reldiff = abs(one-two)/(one+two)) %>% 
    rename(migdur.1 = one, migdur.2 = two)
  
## number of stopovers
  numstop.annual.var <- annual.var %>% 
    select(individual, numb.stopover, mig.year) %>% 
    tidyr::pivot_wider(names_from = mig.year, values_from = numb.stopover) %>% 
    mutate(numstop.reldiff = abs(one-two)/(one+two)) %>% 
    rename(numstop.1 = one, numstop.2 = two)
  
## average stopover duration
  avgstop.annual.var <- annual.var %>% 
    select(individual, avg.stopdur, mig.year) %>% 
    tidyr::pivot_wider(names_from = mig.year, values_from = avg.stopdur) %>% 
    mutate(avgstop.reldiff = abs(one-two)/(one+two)) %>% 
    rename(avgstop.1 = one, avgstop.2 = two)
  
## proportion of migration in stopover
  propstop.annual.var <- annual.var %>% 
    select(individual, prop.stop, mig.year) %>% 
    tidyr::pivot_wider(names_from = mig.year, values_from = prop.stop) %>% 
    mutate(propstop.reldiff = abs(one-two)/(one+two)) %>% 
    rename(propstop.1 = one, propstop.2 = two)
  
## stopover to travel ratio
  stoptrav.annual.var <- annual.var %>% 
    select(individual, stop.travel.rat, mig.year) %>% 
    tidyr::pivot_wider(names_from = mig.year, values_from = stop.travel.rat) %>% 
    mutate(stoptrav.reldiff = abs(one-two)/(one+two)) %>% 
    rename(stoptrav.1 = one, stoptrav.2 = two)



# quantify relationships (correlations) between winter movement and spring migration ----
# subset data by species
metrics.abdu <- mig.metrics %>% filter(species == "abdu") %>% droplevels()
metrics.atbr <- mig.metrics %>% filter(species == "atbr") %>% droplevels()
metrics.grsc <- mig.metrics %>% filter(species == "grsc") %>% droplevels()


## ordinal spring migration initiation date
  abdu.init <- cor.test(metrics.abdu$wint.daydist.km, metrics.abdu$initiation.odate)
  atbr.init <- cor.test(metrics.atbr$wint.daydist.km, metrics.atbr$initiation.odate)
  grsc.init <- cor.test(metrics.grsc$wint.daydist.km, metrics.grsc$initiation.odate)
  
  init.cor <- data.frame(cor = c(abdu.init$estimate, atbr.init$estimate, grsc.init$estimate),
                         t = c(abdu.init$statistic, atbr.init$statistic, grsc.init$statistic),
                         df = c(abdu.init$parameter, atbr.init$parameter, grsc.init$parameter),
                         p = c(abdu.init$p.value, atbr.init$p.value, grsc.init$p.value))
  init.cor$metric <- as.factor(rep("initiation", times = 3))
  init.cor$species <- as.factor(c("abdu", "atbr", "grsc"))
  init.cor


## ordinal spring migration completion date
  abdu.compl <- cor.test(metrics.abdu$wint.daydist.km, metrics.abdu$completion.odate)
  atbr.compl <- cor.test(metrics.atbr$wint.daydist.km, metrics.atbr$completion.odate)
  grsc.compl <- cor.test(metrics.grsc$wint.daydist.km, metrics.grsc$completion.odate)
  
  compl.cor <- data.frame(cor = c(abdu.compl$estimate, atbr.compl$estimate, grsc.compl$estimate),
                         t = c(abdu.compl$statistic, atbr.compl$statistic, grsc.compl$statistic),
                         df = c(abdu.compl$parameter, atbr.compl$parameter, grsc.compl$parameter),
                         p = c(abdu.compl$p.value, atbr.compl$p.value, grsc.compl$p.value))
  compl.cor$metric <- as.factor(rep("complal", times = 3))
  compl.cor$species <- as.factor(c("abdu", "atbr", "grsc"))
  compl.cor


## spring migration duration
  abdu.migdur <- cor.test(metrics.abdu$wint.daydist.km, metrics.abdu$mig.duration.days)
  atbr.migdur <- cor.test(metrics.atbr$wint.daydist.km, metrics.atbr$mig.duration.days)
  grsc.migdur <- cor.test(metrics.grsc$wint.daydist.km, metrics.grsc$mig.duration.days)
  
  migdur.cor <- data.frame(cor = c(abdu.migdur$estimate, atbr.migdur$estimate, grsc.migdur$estimate),
                          t = c(abdu.migdur$statistic, atbr.migdur$statistic, grsc.migdur$statistic),
                          df = c(abdu.migdur$parameter, atbr.migdur$parameter, grsc.migdur$parameter),
                          p = c(abdu.migdur$p.value, atbr.migdur$p.value, grsc.migdur$p.value))
  migdur.cor$metric <- as.factor(rep("mig duration", times = 3))
  migdur.cor$species <- as.factor(c("abdu", "atbr", "grsc"))
  migdur.cor


## number of stopovers
  abdu.numbstop <- cor.test(metrics.abdu$wint.daydist.km, metrics.abdu$numb.stopover)
  atbr.numbstop <- cor.test(metrics.atbr$wint.daydist.km, metrics.atbr$numb.stopover)
  grsc.numbstop <- cor.test(metrics.grsc$wint.daydist.km, metrics.grsc$numb.stopover)
  
  numbstop.cor <- data.frame(cor = c(abdu.numbstop$estimate, atbr.numbstop$estimate, grsc.numbstop$estimate),
                           t = c(abdu.numbstop$statistic, atbr.numbstop$statistic, grsc.numbstop$statistic),
                           df = c(abdu.numbstop$parameter, atbr.numbstop$parameter, grsc.numbstop$parameter),
                           p = c(abdu.numbstop$p.value, atbr.numbstop$p.value, grsc.numbstop$p.value))
  numbstop.cor$metric <- as.factor(rep("number stopovers", times = 3))
  numbstop.cor$species <- as.factor(c("abdu", "atbr", "grsc"))
  numbstop.cor


## avg stopover duration
  abdu.avg.stopdur <- cor.test(metrics.abdu$wint.daydist.km, metrics.abdu$avg.stopdur)
  atbr.avg.stopdur <- cor.test(metrics.atbr$wint.daydist.km, metrics.atbr$avg.stopdur)
  grsc.avg.stopdur <- cor.test(metrics.grsc$wint.daydist.km, metrics.grsc$avg.stopdur)
  
  avg.stopdur.cor <- data.frame(cor = c(abdu.avg.stopdur$estimate, atbr.avg.stopdur$estimate, grsc.avg.stopdur$estimate),
                           t = c(abdu.avg.stopdur$statistic, atbr.avg.stopdur$statistic, grsc.avg.stopdur$statistic),
                           df = c(abdu.avg.stopdur$parameter, atbr.avg.stopdur$parameter, grsc.avg.stopdur$parameter),
                           p = c(abdu.avg.stopdur$p.value, atbr.avg.stopdur$p.value, grsc.avg.stopdur$p.value))
  avg.stopdur.cor$metric <- as.factor(rep("avg stop duration", times = 3))
  avg.stopdur.cor$species <- as.factor(c("abdu", "atbr", "grsc"))
  avg.stopdur.cor


## proportion of time in stopover
  abdu.propstop <- cor.test(metrics.abdu$wint.daydist.km, metrics.abdu$prop.stop)
  atbr.propstop <- cor.test(metrics.atbr$wint.daydist.km, metrics.atbr$prop.stop)
  grsc.propstop <- cor.test(metrics.grsc$wint.daydist.km, metrics.grsc$prop.stop)
  
  propstop.cor <- data.frame(cor = c(abdu.propstop$estimate, atbr.propstop$estimate, grsc.propstop$estimate),
                                t = c(abdu.propstop$statistic, atbr.propstop$statistic, grsc.propstop$statistic),
                                df = c(abdu.propstop$parameter, atbr.propstop$parameter, grsc.propstop$parameter),
                                p = c(abdu.propstop$p.value, atbr.propstop$p.value, grsc.propstop$p.value))
  propstop.cor$metric <- as.factor(rep("prop stopover", times = 3))
  propstop.cor$species <- as.factor(c("abdu", "atbr", "grsc"))
  propstop.cor


## stopover to travel ratio
  abdu.stoptravrat <- cor.test(metrics.abdu$wint.daydist.km, metrics.abdu$stop.travel.rat)
  atbr.stoptravrat <- cor.test(metrics.atbr$wint.daydist.km, metrics.atbr$stop.travel.rat)
  grsc.stoptravrat <- cor.test(metrics.grsc$wint.daydist.km, metrics.grsc$stop.travel.rat)
  
  stoptravrat.cor <- data.frame(cor = c(abdu.stoptravrat$estimate, atbr.stoptravrat$estimate, grsc.stoptravrat$estimate),
                                t = c(abdu.stoptravrat$statistic, atbr.stoptravrat$statistic, grsc.stoptravrat$statistic),
                                df = c(abdu.stoptravrat$parameter, atbr.stoptravrat$parameter, grsc.stoptravrat$parameter),
                                p = c(abdu.stoptravrat$p.value, atbr.stoptravrat$p.value, grsc.stoptravrat$p.value))
  stoptravrat.cor$metric <- as.factor(rep("stop trav ratio", times = 3))
  stoptravrat.cor$species <- as.factor(c("abdu", "atbr", "grsc"))
  stoptravrat.cor



# relationships between winter home range size and spring migration ----
## ordinal spring migration initiation date
  abdu.init.hr <- cor.test(metrics.abdu$area.sqkm, metrics.abdu$initiation.odate)
  atbr.init.hr <- cor.test(metrics.atbr$area.sqkm, metrics.atbr$initiation.odate)
  grsc.init.hr <- cor.test(metrics.grsc$area.sqkm, metrics.grsc$initiation.odate)
  
  init.hr.cor <- data.frame(cor = c(abdu.init.hr$estimate, atbr.init.hr$estimate, grsc.init.hr$estimate),
                         t = c(abdu.init.hr$statistic, atbr.init.hr$statistic, grsc.init.hr$statistic),
                         df = c(abdu.init.hr$parameter, atbr.init.hr$parameter, grsc.init.hr$parameter),
                         p = c(abdu.init.hr$p.value, atbr.init.hr$p.value, grsc.init.hr$p.value))
  init.hr.cor$metric <- as.factor(rep("initiation", times = 3))
  init.hr.cor$species <- as.factor(c("abdu", "atbr", "grsc"))
  init.hr.cor


## ordinal spring migration completion date
  abdu.compl.hr <- cor.test(metrics.abdu$area.sqkm, metrics.abdu$completion.odate)
  atbr.compl.hr <- cor.test(metrics.atbr$area.sqkm, metrics.atbr$completion.odate)
  grsc.compl.hr <- cor.test(metrics.grsc$area.sqkm, metrics.grsc$completion.odate)
  
  compl.hr.cor <- data.frame(cor = c(abdu.compl.hr$estimate, atbr.compl.hr$estimate, grsc.compl.hr$estimate),
                            t = c(abdu.compl.hr$statistic, atbr.compl.hr$statistic, grsc.compl.hr$statistic),
                            df = c(abdu.compl.hr$parameter, atbr.compl.hr$parameter, grsc.compl.hr$parameter),
                            p = c(abdu.compl.hr$p.value, atbr.compl.hr$p.value, grsc.compl.hr$p.value))
  compl.hr.cor$metric <- as.factor(rep("completion", times = 3))
  compl.hr.cor$species <- as.factor(c("abdu", "atbr", "grsc"))
  compl.hr.cor


## spring migration duration
  abdu.migdur.hr <- cor.test(metrics.abdu$area.sqkm, metrics.abdu$mig.duration.days)
  atbr.migdur.hr <- cor.test(metrics.atbr$area.sqkm, metrics.atbr$mig.duration.days)
  grsc.migdur.hr <- cor.test(metrics.grsc$area.sqkm, metrics.grsc$mig.duration.days)
  
  migdur.hr.cor <- data.frame(cor = c(abdu.migdur.hr$estimate, atbr.migdur.hr$estimate, grsc.migdur.hr$estimate),
                            t = c(abdu.migdur.hr$statistic, atbr.migdur.hr$statistic, grsc.migdur.hr$statistic),
                            df = c(abdu.migdur.hr$parameter, atbr.migdur.hr$parameter, grsc.migdur.hr$parameter),
                            p = c(abdu.migdur.hr$p.value, atbr.migdur.hr$p.value, grsc.migdur.hr$p.value))
  migdur.hr.cor$metric <- as.factor(rep("mig duration", times = 3))
  migdur.hr.cor$species <- as.factor(c("abdu", "atbr", "grsc"))
  migdur.hr.cor


## number of stopovers
  abdu.numbstop.hr <- cor.test(metrics.abdu$area.sqkm, metrics.abdu$numb.stopover)
  atbr.numbstop.hr <- cor.test(metrics.atbr$area.sqkm, metrics.atbr$numb.stopover)
  grsc.numbstop.hr <- cor.test(metrics.grsc$area.sqkm, metrics.grsc$numb.stopover)
  
  numbstop.hr.cor <- data.frame(cor = c(abdu.numbstop.hr$estimate, atbr.numbstop.hr$estimate, grsc.numbstop.hr$estimate),
                               t = c(abdu.numbstop.hr$statistic, atbr.numbstop.hr$statistic, grsc.numbstop.hr$statistic),
                               df = c(abdu.numbstop.hr$parameter, atbr.numbstop.hr$parameter, grsc.numbstop.hr$parameter),
                               p = c(abdu.numbstop.hr$p.value, atbr.numbstop.hr$p.value, grsc.numbstop.hr$p.value))
  numbstop.hr.cor$metric <- as.factor(rep("number stopover", times = 3))
  numbstop.hr.cor$species <- as.factor(c("abdu", "atbr", "grsc"))
  numbstop.hr.cor

## avgerage stopover duration
  abdu.avgstopdur.hr <- cor.test(metrics.abdu$area.sqkm, metrics.abdu$avg.stopdur)
  atbr.avgstopdur.hr <- cor.test(metrics.atbr$area.sqkm, metrics.atbr$avg.stopdur)
  grsc.avgstopdur.hr <- cor.test(metrics.grsc$area.sqkm, metrics.grsc$avg.stopdur)
  
  avgstopdur.hr.cor <- data.frame(cor = c(abdu.avgstopdur.hr$estimate, atbr.avgstopdur.hr$estimate, grsc.avgstopdur.hr$estimate),
                                t = c(abdu.avgstopdur.hr$statistic, atbr.avgstopdur.hr$statistic, grsc.avgstopdur.hr$statistic),
                                df = c(abdu.avgstopdur.hr$parameter, atbr.avgstopdur.hr$parameter, grsc.avgstopdur.hr$parameter),
                                p = c(abdu.avgstopdur.hr$p.value, atbr.avgstopdur.hr$p.value, grsc.avgstopdur.hr$p.value))
  avgstopdur.hr.cor$metric <- as.factor(rep("avg stop duration", times = 3))
  avgstopdur.hr.cor$species <- as.factor(c("abdu", "atbr", "grsc"))
  avgstopdur.hr.cor


## proportion of time in stopover
  abdu.propstop.hr <- cor.test(metrics.abdu$area.sqkm, metrics.abdu$prop.stop)
  atbr.propstop.hr <- cor.test(metrics.atbr$area.sqkm, metrics.atbr$prop.stop)
  grsc.propstop.hr <- cor.test(metrics.grsc$area.sqkm, metrics.grsc$prop.stop)
  
  propstop.hr.cor <- data.frame(cor = c(abdu.propstop.hr$estimate, atbr.propstop.hr$estimate, grsc.propstop.hr$estimate),
                                  t = c(abdu.propstop.hr$statistic, atbr.propstop.hr$statistic, grsc.propstop.hr$statistic),
                                  df = c(abdu.propstop.hr$parameter, atbr.propstop.hr$parameter, grsc.propstop.hr$parameter),
                                  p = c(abdu.propstop.hr$p.value, atbr.propstop.hr$p.value, grsc.propstop.hr$p.value))
  propstop.hr.cor$metric <- as.factor(rep("avg stop duration", times = 3))
  propstop.hr.cor$species <- as.factor(c("abdu", "atbr", "grsc"))
  propstop.hr.cor


## stopover to travel ratio
  abdu.stoptravrat.hr <- cor.test(metrics.abdu$area.sqkm, metrics.abdu$stop.travel.rat)
  atbr.stoptravrat.hr <- cor.test(metrics.atbr$area.sqkm, metrics.atbr$stop.travel.rat)
  grsc.stoptravrat.hr <- cor.test(metrics.grsc$area.sqkm, metrics.grsc$stop.travel.rat)
  
  stoptravrat.hr.cor <- data.frame(cor = c(abdu.stoptravrat.hr$estimate, atbr.stoptravrat.hr$estimate, grsc.stoptravrat.hr$estimate),
                                t = c(abdu.stoptravrat.hr$statistic, atbr.stoptravrat.hr$statistic, grsc.stoptravrat.hr$statistic),
                                df = c(abdu.stoptravrat.hr$parameter, atbr.stoptravrat.hr$parameter, grsc.stoptravrat.hr$parameter),
                                p = c(abdu.stoptravrat.hr$p.value, atbr.stoptravrat.hr$p.value, grsc.stoptravrat.hr$p.value))
  stoptravrat.hr.cor$metric <- as.factor(rep("stop trav ratio", times = 3))
  stoptravrat.hr.cor$species <- as.factor(c("abdu", "atbr", "grsc"))
  stoptravrat.hr.cor





