Data files (InterspecificInterindividualVariationData.RDS and AnnualVariationData.RDS) and code (SpringMigStrategyCode) associated with Journal of Avian Biology manuscript "Spring migration strategies differ among three waterfowl species that winter in southern New England, USA". These methods were used to investigate interspecific and intraspecific variation in spring migration strategy of 3 sympatric-wintering waterfowl.

# Data file descriptions (can also be found within the code):
## InterspecificInterindividualVariationData.RDS
  ## data for quantifying interspecific and interindividual variation in spring migration strategy and carryover effects
  - individual: individual identifier (transmitter ID number)
  - species: abdu = American Black Duck, atbr = Atlantic Brant, grsc = Greater Scaup
  - sex: F = female, M = male
  - age: ASY = after second year (adult), SY = second year (juvenile)
  - mig.dist.km: sum of distances for all 24 hr movements from initiation to completion of spring migration to a suspected breeding site (km)
  - breed.lat: latitude of individuals identified settled breeding location (decimal degrees)
  - initiation.odate: ordinal spring migration initiation date
  - completion.odate: ordinal spring migration completion date
  - mig.duration.days: spring migration duration; difference in days between ordinal spring migration completion and initiation dates
  - numb.stopover: the total number of unique stopover events identified
  - avg.stopdur: average duration (days) of unique stopover events
  - prop.stop: proportion of migration time (days) spent in stopover; total stopover duration divided by migration duration
  - stop.travel.rat: stopover to travel ratio (days/km); total number of days stopped over divided total migration distance
  - wint.daydist.km: average daily winter movement distance (km)
  - area.sqkm: winter home range (95% wAKDE) size (km^2)

## AnnualVariationData.RDS
  ## data for quantifying annual variation in spring migration strategy
  - individual: individual identifier (transmitter ID number)
  - sex: F = female, M = male
  - age: ASY = after second year (adult), SY = second year (juvenile)
  - species: abdu = American Black Duck, atbr = Atlantic Brant, grsc = Greater Scaup
  - mig.year: one = individual's first spring migration, two = individual's second spring migration
  - initiation.odate: ordinal spring migration initiation date
  - completion.odate: ordinal spring migration completion date
  - mig.duration.days: spring migration duration; difference in days between ordinal spring migration completion and initiation dates
  - numb.stopover: the total number of unique stopover events identified
  - avg.stopdur: average duration (days) of unique stopover events
  - prop.stop: proportion of migration time (days) spent in stopover; total stopover duration divided by migration duration
  - stop.travel.rat: stopover to travel ratio (days/km); total number of days stopped over divided total migration distance

# Please cite as:
Mezebish Quinn, T., Paton, P. W., Kilburn, J. E., and McWilliams, S. R. 2026. Spring migration strategies differ among three waterfowl species that winter in southern New England, USA. - J. Avian Biol.
