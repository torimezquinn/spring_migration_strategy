# Code used to implement analyses described in:
# Journal of Avian Biology
# 2026: e03571
# doi: 10.1002/jav.0357
# Spring migration strategies differ among three waterfowl species that winter in southern New England, USA


rm(list = ls())

library(tidyverse)  ## data manipulation


# read in formatted data ----
# data for quantifying interspecific and interindividual variation in spring migration strategy
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
  
  # interindividual variation (CV)
  init.summary <- data.frame(aggregate(mig.metrics$initiation.odate, by = list(mig.metrics$species), FUN = mean)) %>% 
    rename(species = Group.1, avg = x)
  init.summary$sd <- data.frame(aggregate(mig.metrics$initiation.odate, by = list(mig.metrics$species), FUN = sd))[1:3,2]
  init.summary$cv <- init.summary$sd/init.summary$avg
  init.summary$metric <- "initiation.odate"
  init.summary


## ordinal spring migration completion date
  # interspecific variation (ANOVA and TukeyHSD)
  compl.aov <- aov(completion.odate ~ species, data = mig.metrics)
  summary(compl.aov)
  TukeyHSD(compl.aov)
  
  # interindividual variation (CV)
  compl.summary <- data.frame(aggregate(mig.metrics$completion.odate, by = list(mig.metrics$species), FUN = mean)) %>% 
    rename(species = Group.1, avg = x)
  compl.summary$sd <- data.frame(aggregate(mig.metrics$completion.odate, by = list(mig.metrics$species), FUN = sd))[1:3,2]
  compl.summary$cv <- compl.summary$sd/compl.summary$avg
  compl.summary$metric <- "completion.odate"
  compl.summary
  

## spring migration duration
  # interspecific variation (ANOVA and TukeyHSD)
  migdur.aov <- aov(mig.duration.days ~ species, data = mig.metrics)
  summary(migdur.aov)
  TukeyHSD(migdur.aov)
  
  # interindividual variation (CV)
  duration.summary <- data.frame(aggregate(mig.metrics$mig.duration.days, by = list(mig.metrics$species), FUN = mean)) %>% 
    rename(species = Group.1, avg = x)
  duration.summary$sd <- data.frame(aggregate(mig.metrics$mig.duration.days, by = list(mig.metrics$species), FUN = sd))[1:3,2]
  duration.summary$cv <- duration.summary$sd/duration.summary$avg
  duration.summary$metric <- "mig.duration.days"
  duration.summary


## number of stopovers
  # interspecific variation (ANOVA and TukeyHSD)
  numbstop.aov <- aov(numb.stopover ~ species, data = mig.metrics)
  summary(numbstop.aov)
  TukeyHSD(numbstop.aov)
  
  # interindividual variation (CV)
  nstop.summary <- data.frame(aggregate(mig.metrics$numb.stopover, by = list(mig.metrics$species), FUN = mean)) %>% 
    rename(species = Group.1, avg = x)
  nstop.summary$sd <- data.frame(aggregate(mig.metrics$numb.stopover, by = list(mig.metrics$species), FUN = sd))[1:3,2]
  nstop.summary$cv <- nstop.summary$sd/nstop.summary$avg
  nstop.summary$metric <- "numb.stopover"
  nstop.summary


## average stopover duration
  # interspecific variation (ANOVA and TukeyHSD)
  avgstopdur.aov <- aov(avg.stopdur ~ species, data = mig.metrics)
  summary(avgstopdur.aov)
  TukeyHSD(avgstopdur.aov)
  
  # interindividual variation (CV)
  avgstop.summary <- data.frame(aggregate(mig.metrics$avg.stopdur, by = list(mig.metrics$species), FUN = mean)) %>% 
    rename(species = Group.1, avg = x)
  avgstop.summary$sd <- data.frame(aggregate(mig.metrics$avg.stopdur, by = list(mig.metrics$species), FUN = sd))[1:3,2]
  avgstop.summary$cv <- avgstop.summary$sd/avgstop.summary$avg
  avgstop.summary$metric <- "avg.stopdur"
  avgstop.summary


## proportion of migration in stopover
  # interspecific variation (ANOVA and TukeyHSD)
  propstop.aov <- aov(prop.stop ~ species, data = mig.metrics)
  summary(propstop.aov)
  TukeyHSD(propstop.aov)
  
  # interindividual variation (CV)
  propstop.summary <- data.frame(aggregate(mig.metrics$prop.stop, by = list(mig.metrics$species), FUN = mean)) %>% 
    rename(species = Group.1, avg = x)
  propstop.summary$sd <- data.frame(aggregate(mig.metrics$prop.stop, by = list(mig.metrics$species), FUN = sd))[1:3,2]
  propstop.summary$cv <- propstop.summary$sd/propstop.summary$avg
  propstop.summary$metric <- "prop.stop"
  propstop.summary


## stopover to travel ratio
  # interspecific variation (ANOVA and TukeyHSD)
  stoptravrat.aov <- aov(stop.travel.rat ~ species, data = mig.metrics)
  summary(stoptravrat.aov)
  TukeyHSD(stoptravrat.aov)
  
  # interindividual variation (CV)
  stoptravel.summary <- data.frame(aggregate(mig.metrics$stop.travel.rat, by = list(mig.metrics$species), FUN = mean)) %>%
    rename(species = Group.1, avg = x)
  stoptravel.summary$sd <- data.frame(aggregate(mig.metrics$stop.travel.rat, by = list(mig.metrics$species), FUN = sd))[1:3,2]
  stoptravel.summary$cv <- stoptravel.summary$sd/stoptravel.summary$avg
  stoptravel.summary$metric <- "stop.travel.rat"
  stoptravel.summary


# quantify annual variation in spring migration metrics ----
## ordinal spring migration initiation date
  init.annual.var <- annual.var %>% 
    select(individual, species, initiation.odate, mig.year) %>% 
    tidyr::pivot_wider(names_from = mig.year, values_from = initiation.odate) %>% 
    mutate(initiation.normdiff = abs(one-two)/(one+two)) %>% 
    rename(initiation.1 = one, initiation.2 = two)
  
## ordinal spring migration completion date
  arriv.annual.var <- annual.var %>% 
    select(individual, completion.odate, mig.year) %>% 
    tidyr::pivot_wider(names_from = mig.year, values_from = completion.odate) %>% 
    mutate(completion.normdiff = abs(one-two)/(one+two)) %>% 
    rename(completion.1 = one, completion.2 = two)
  
## spring migration duration
  migdur.annual.var <- annual.var %>% 
    select(individual, mig.duration.days, mig.year) %>% 
    tidyr::pivot_wider(names_from = mig.year, values_from = mig.duration.days) %>% 
    mutate(migdur.normdiff = abs(one-two)/(one+two)) %>% 
    rename(migdur.1 = one, migdur.2 = two)
  
## number of stopovers
  numstop.annual.var <- annual.var %>% 
    select(individual, numb.stopover, mig.year) %>% 
    tidyr::pivot_wider(names_from = mig.year, values_from = numb.stopover) %>% 
    mutate(numstop.normdiff = abs(one-two)/(one+two)) %>% 
    rename(numstop.1 = one, numstop.2 = two)
  
## average stopover duration
  avgstop.annual.var <- annual.var %>% 
    select(individual, avg.stopdur, mig.year) %>% 
    tidyr::pivot_wider(names_from = mig.year, values_from = avg.stopdur) %>% 
    mutate(avgstop.normdiff = abs(one-two)/(one+two)) %>% 
    rename(avgstop.1 = one, avgstop.2 = two)
  
## proportion of migration in stopover
  propstop.annual.var <- annual.var %>% 
    select(individual, prop.stop, mig.year) %>% 
    tidyr::pivot_wider(names_from = mig.year, values_from = prop.stop) %>% 
    mutate(propstop.normdiff = abs(one-two)/(one+two)) %>% 
    rename(propstop.1 = one, propstop.2 = two)
  
## stopover to travel ratio
  stoptrav.annual.var <- annual.var %>% 
    select(individual, stop.travel.rat, mig.year) %>% 
    tidyr::pivot_wider(names_from = mig.year, values_from = stop.travel.rat) %>% 
    mutate(stoptrav.normdiff = abs(one-two)/(one+two)) %>% 
    rename(stoptrav.1 = one, stoptrav.2 = two)




