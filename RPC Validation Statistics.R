## Analysis comparing Quadrat RPC data collected in 2022 to previous RPC method

#install.packages('tidyverse')
#install.packages('boot')
library(tidyverse)
library(boot)

#-----------------------------
#Grab data and convert Species Code to a Factor
#-----------------------------

quad_rpc_data <- read_csv("Quadrat_RPC_2022_Data.csv") 
rpc_data <- read_csv("RPC_2022_RawData_Export.csv") 
#leaving species as numeric because it makes the tables order more sensibly for me being familiar with the RPC species and their codes

#---------------------------------------------------------------------------
#-----------------------------
#Calculate average proportions for each meter point ("old" RPCs) or each quadrat point, then
#Calculate average proportions across entire site, then
#Combine data into single data frame
#-----------------------------

rpc_proportions <- rpc_data %>%
  mutate(meter_proportion = rowMeans(select(rpc_data, CountA, CountB, CountC, CountD))/10) %>% #not strictly "tidy" hence rowMeans, but
  group_by(IslandName, SiteName, Species, ScientificName, CommonName) %>%
  summarize(average_proportion = mean(meter_proportion)) %>%
  ungroup()

quad_rpc_proportions <- quad_rpc_data %>%
  mutate(meter_proportion = rowMeans(select(quad_rpc_data, CountA, CountB))/4) %>%
  group_by(IslandName, SiteName, Species, ScientificName, CommonName) %>%
  summarize(quad_average_proportion = mean(meter_proportion)) %>%
  ungroup()

Combined_Data <- left_join(rpc_proportions, quad_rpc_proportions)
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#-----------------------------
#Calculate Chi-Squared p values for each species using sites as replicates
#-----------------------------

#-----------------------------
#Lol Cullen help plz between these - # This is the attempt using "proper" pipes and tidy-logic for accomplishing this, and where I'm weirdly stuck
#-----------------------------

#Write function that returns chi-sq p value given a summarized dataset (IE what I was doing in summarize originally, so that I can use it in map2_dfr)

summarized_chisq <- function(data){
  return(data %>%
           group_by(Species, ScientificName, CommonName) %>%
           summarize(ChiSq_p_val = chisq.test(average_proportion, quad_average_proportion)$p.value)
  )
}
all_zeroes_chisq <- function(data){
  return(1) #there's gotta be a simipler way to do this but I'm at peak mental capacity getting my head around map2() so... this does what I want for now
}

# This is what I *thought* might work based on the below version, based on the Stack Overflow example below that - 

Pipe_ChiSq_By_Species <- Combined_Data %>%
  mutate(all_zeroes = case_when(average_proportion == 0 & quad_average_proportion == 0 ~ 'TRUE',
                                average_proportion != 0 | quad_average_proportion != 0 ~ 'FALSE')) %>%
  split(.,.$all_zeroes == "TRUE") %>%
  map2_dfr(c(summarized_chisq, all_zeroes_chisq), ~.x %>% group_by(Species, ScientificName, CommonName) %>% summarize(ChiSq_p_val = summarized_chisq(.)),.y)

#EXAMPLE FROM

example_df <- iris %>%
  arrange(Species=="setosa") %>%
  split(.,.$Species=="setosa") %>%
  map2_dfr(c(mean,sum),~.x %>% group_by(Species) %>% summarize_at("Petal.Width",.y))


#Previous attempt before trying to go to map2() was

Pipe_ChiSq_By_Species_2 <- Combined_Data %>%
  group_by(Species, ScientificName, CommonName) %>%
  summarize(ChiSq_p_val = case_when(average_proportion == 0 && quad_average_proportion == 0 ~ 1,
                                    average_proportion != 0 && quad_average_proportion != 0 ~ chisq.test(average_proportion, quad_average_proportion)$p.value)) #turns out this doesn't need to be .$average_proportion , etc

Testing_Summarized_ChiSq <- Combined_Data %>%
  filter(Species == 2014) %>%
  summarize(ChiSq_p_val = case_when(all(average_proportion) == 0 && all(quad_average_proportion) == 0 ~ 1,
                                    all(average_proportion) != 0 && all(quad_average_proportion) != 0 ~ chisq.test(average_proportion, quad_average_proportion)$p.value)) #turns out this doesn't need to be .$average_proportion , etc

#What's going on here, we're getting the same p-value for every single species, something isn't right in summarize
#as demonstrated by below - we get different p-values for individual species as expected.

Testing_SummarizedChiSquared <- Combined_Data %>%
  filter(Species == "11010") %>%
  summarize(ChiSq_p_val = chisq.test(average_proportion, quad_average_proportion)$p.value)

#AHH except for the cases with all zeroes
Testing_SummarizedChiSquared2 <- Combined_Data %>%
  filter(Species == "2013")
summarize(Testing_SummarizedChiSquared2, ChiSq_p_val = chisq.test(average_proportion, quad_average_proportion)$p.value)

#Trying to get the above function working...
Testing_SummarizedChiSquared3 <- Combined_Data %>%
  filter(Species == "11010") %>%
  summarize(ChiSq_p_val = summarized_chisq(.))

#-----------------------------
#Lol Cullen help plz between these
#-----------------------------

#-----------------------------
# So much for trying to be a proper-functional programming R user, back to object oriented/loops since clearly the filter works on each individual spp.
#-----------------------------

ChiSq_By_Species <- data.frame(Species=character(), ScientificName=character(), CommonName=character(), P_Value=double())
Species_List <- as.character(unique(Combined_Data$Species))

for(i in 1:Species_List){
  j <- Species_List[i]
  print(j)
  filtered_data <- Combined_Data[Combined_Data$Species == j,] 
  
  if(filtered_data$average_proportion == c(0, 0, 0, 0) && filtered_data$quad_average_proportion == c(0, 0, 0, 0)){
    print(j)
    list_i <- c(as.character(Combined_Data$Species[i]),
                Combined_Data$ScientificName[i],
                Combined_Data$CommonName[i],
                1)
    ChiSq_By_Species[i, ] <- list_i 
  } else {
    list_i <- c(as.character(Combined_Data$Species[i]),
              Combined_Data$ScientificName[i],
              Combined_Data$CommonName[i],
              (Combined_Data %>%
                filter(Species == j) %>%
                summarize(ChiSq_p_val = chisq.test(average_proportion, quad_average_proportion)$p.value)))
  ChiSq_By_Species[i, ] <- list_i 
  }
}

write_csv(ChiSq_By_Species, "ChiSq_By_Species_Averages.csv")

#Hold up, is this if/else acting right? I don't think so, because when you print j when it hits the first clause, you get - 
# 2013, 3005, 4001, 5001, 6003, 6004, 6005, 11008, but when you inspect those with - 

should_be_zeroes <- c(2013, 3005, 4001, 5001, 6003, 6004, 6005, 11008)

Not_Seen <- Combined_Data %>%
  filter(Species %in% should_be_zeroes)

# you find that 2013 (Pterygophora) and 6004 (Balanophyllia) are actually the only offending species - this is && so it should
# be only kicking into the if part of the if/else for these two species,but why is it doing it for several of these others??

#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#-----------------------------
# BOOTSTRAPPED SOLUTION - 
#Not the cause of the issues above, but for a potentially more robust analysis - 
#some sources suggest an N>=5 is necessary for accurate estimates in chi-Squared goodness of fit, so
#Generate bootstrapped estimates of site means for each species so that we have enough power to run chi-squared tests
#-----------------------------

#Bootstrap mean function
boot_mean <- function(data, indices){
  d <- data[indices]
  return(mean(d))
}

#calculate proportions for each meter, then summarize with a bootstrap estimate instead of actual average
boot_rpc_proportions <- rpc_data %>%
  mutate(meter_proportion = rowMeans(select(rpc_data, CountA, CountB, CountC, CountD))/10) %>% #not strictly "tidy" hence rowMeans, but
  group_by(IslandName, SiteName, Species, ScientificName, CommonName) %>%
  summarize(bootstrapped_mean = boot(.$meter_proportion, statistic=boot_mean, R=500)$t) %>%
  ungroup()

boot_quad_rpc_proportions <- quad_rpc_data %>%
  mutate(meter_proportion = rowMeans(select(quad_rpc_data, CountA, CountB))/4) %>%
  group_by(IslandName, SiteName, Species, ScientificName, CommonName) %>%
  summarize(quad_bootstrapped_mean = boot(.$meter_proportion, statistic=boot_mean, R=500)$t) %>%
  ungroup()

Boot_Combined_Data <- cbind(boot_rpc_proportions, boot_quad_rpc_proportions$quad_bootstrapped_mean) 
names(Boot_Combined_Data)[7] <- "quad_bootstrapped_mean"

#Chi-Squared test
Boot_ChiSq_By_Species <- Boot_Combined_Data %>%
  group_by(Species, ScientificName, CommonName) %>%
  summarize(ChiSq_p_val = chisq.test(bootstrapped_mean, quad_bootstrapped_mean)$p.value)

write_csv(Boot_ChiSq_By_Species, "ChiSq_By_Species_Bootstraps.csv")

# (Same issues as above, something's up with summarize, but built this thinking something was happening because of a low n(), 
#  but this is potentially more robust anyways so I'm not throwing away this code yet)

#  LATER NOTE - so my original issue with the summarize was I didn't need to do
# summarize(ChiSq_p_val = chisq.test(.$bootstrapped_mean, .$quad_bootstrapped_mean)$p.value)
# and that .$ wasn't necessary even switching out of tidyverse functions - so that was a cool note to learn, but that weirdly fixed
# my issues with this one, but not the above one - 

# LATER NOTE #2 my underlying issue, now that I'm not getting all the same p-values across the summarize 
# (IE .$ seems to eliminate the group_by() ), is that I figured out why chisq.test was misbehaving (obviously) on all zeroes.

#---------------------------------------------------------------------------
