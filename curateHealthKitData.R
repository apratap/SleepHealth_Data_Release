rm(list=ls())
options(stringsAsFactors = F)
library("install.load")
install_load("data.table", "gdata", "synapser", "jsonlite", "stringr")
install_load("plyr", "tidyverse", "doMC", "scales", "data.table")
install_load("gridExtra", "pheatmap", "printr", "ggthemes", "anytime")
synapser::synLogin()

metadata <- fread(synGet("syn19530098")$path, data.table = F)

# People who agreed to share
sharingInfo <- gdata::read.xls(synGet("syn21557215")$path)
healthCodes_with_broadsharing = sharingInfo %>% filter(sharing== 'all_qualified_researchers')
healthCodes_with_broadsharing = healthCodes_with_broadsharing$participant.id

# Underage participants who have to be excluded
underage_participants <- fread(synGet("syn21905452")$path, data.table=F)

### Data QA/QCs
HK.dataFiles <- synGetChildren("syn21243305", includeTypes=list("file"))
HK.dataFiles <- HK.dataFiles$asList()
HK.dataFiles <- do.call(rbind.data.frame, HK.dataFiles) %>%
  select(-type, -versionNumber, -benefactorId, -versionLabel, 
         -modifiedOn, -modifiedBy, -createdBy, -createdOn) %>%
  mutate(name = gsub('HKCategoryTypeIdentifier', '', name),
         name = gsub('.csv', '', name))
#Download and add the path to df
HK.dataFiles <- HK.dataFiles %>% mutate(path = map(id, function(id) synGet(id)$path))

#get back basic QA/QC
tmp_get_QAQC <- function(path){
  df <- fread(path, data.table = F, quote="", fill=T, header = T) %>%
    #Filter out underage participants
    filter(! participantId %in% underage_participants$participantId)
  
  #Cohort level 
  nUsers <- n_distinct(df$participantId)
  nUsers_broadShare <- length(intersect(unique(df$participantId), healthCodes_with_broadsharing))
  maxDate <- max(as.Date(lubridate::ymd_hms(df$timestamp)))
  minDate <- min(as.Date(lubridate::ymd_hms(df$timestamp)))
  #total number of participant days
  nDays <- df %>% group_by(participantId) %>% summarise(nDays = n_distinct(as.Date(lubridate::ymd_hms(df$timestamp))))
  nDays <- sum(df$nDays, na.rm = T)
  data.frame(nUsers, nUsers_broadShare, maxDate, minDate, nDays)
}
tmp_get_QAQC_safely <- purrr::safely(tmp_get_QAQC)
sh_hkSummary <- HK.dataFiles %>% mutate(output = map(path, tmp_get_QAQC_safely),
                                        result = map(output, function(x) x$result),
                                        errors = map(output, function(x) x$error)) %>%
  dplyr::select(-output, -path) %>% unnest(result)

#prop of people who contributed HK data
sh_hkSummary <- sh_hkSummary %>% mutate(percent = round( (nUsers/nrow(metadata))*100, digits=2),
                                        percent_broadShare = round(nUsers_broadShare/length(healthCodes_with_broadsharing)*100, digits=2)) 

### Arbritrary Cut-off aleast 1% of the sample ccontributed the HK data type
sh_hk_data_tobe_released <- sh_hkSummary %>% filter(percent_broadShare >= 1)
View(sh_hk_data_tobe_released %>% select(name, id, nUsers_broadShare, percent_broadShare, nDays))


########################################
##### Prepare data for release project
########################################
executed_code_GitHub_URL = "https://github.com/apratap/SleepHealth_Data_Release/blob/master/curateHealthKitData.R"

# ActiveEnergyBurned
View(sh_hk_data_tobe_released[1,])
df <- fread(synGet("syn21292879")$path, data.table = F)  %>% 
  select(-undefinedColumn, -source) %>%
  filter(participantId %in%  healthCodes_with_broadsharing)%>%
  #Filter out underage participants
  filter(! participantId %in% underage_participants$participantId)
table <- synBuildTable("HealthKit.ActiveEnergyBurned", "syn18492837", df)
table <- synStore(table)
synSetProvenance(table,
                 Activity(used = "syn21292879",
                          executed = executed_code_GitHub_URL))
                 



#HealthKit.AppleExerciseTime
View(sh_hk_data_tobe_released[2,])
AppleExerciseTime.df <- fread(synGet("syn21292876")$path, data.table = F) %>% 
  dplyr::select(-undefinedColumn, -source) %>%
  dplyr::filter(participantId %in%  healthCodes_with_broadsharing)%>%
  #Filter out underage participants
  filter(! participantId %in% underage_participants$participantId)
AppleExerciseTime.Table <- synBuildTable("HealthKit.AppleExerciseTime", "syn18492837", AppleExerciseTime.df)
AppleExerciseTime.Table <- synStore(AppleExerciseTime.Table)
synSetProvenance(AppleExerciseTime.Table,
                 Activity(used = "syn21292876",
                          executed = executed_code_GitHub_URL))


# BasalEnergyBurned
View(sh_hk_data_tobe_released[3,])
BasalEnergyBurned.df <- fread(synGet("syn21292877")$path, data.table = F) %>%
  dplyr::select(-undefinedColumn, -source) %>%
  dplyr::filter(participantId %in%  healthCodes_with_broadsharing)%>%
  #Filter out underage participants
  filter(! participantId %in% underage_participants$participantId)
BasalEnergyBurned.Table <- synBuildTable("HealthKit.BasalEnergyBurned", "syn18492837", BasalEnergyBurned.df)
BasalEnergyBurned.Table <- synStore(BasalEnergyBurned.Table)
synSetProvenance(BasalEnergyBurned.Table,
                 Activity(used = "syn21292877",
                          executed = executed_code_GitHub_URL))


# BodyMass
View(sh_hk_data_tobe_released[4,])
BodyMass.df <- fread(synGet("syn21292819")$path, data.table = F) %>%
  mutate(value = round(value, digits=3)) %>%
  dplyr::select(-undefinedColumn, -source) %>%
  dplyr::filter(participantId %in%  healthCodes_with_broadsharing)%>%
  #Filter out underage participants
  filter(! participantId %in% underage_participants$participantId)
BodyMass.Table <- synBuildTable("HealthKit.BodyMass", "syn18492837", BodyMass.df)
BodyMass.Table <- synStore(BodyMass.Table)
synSetProvenance(BodyMass.Table,
                 Activity(used = "syn21292819",
                          executed = executed_code_GitHub_URL))


# DistanceWalkingRunning
View(sh_hk_data_tobe_released[5,])
DistanceWalkingRunning.df <- fread(synGet("syn21292862")$path, data.table = F)  %>%
  mutate(value = round(value, digits=3)) %>%
  dplyr::select(-undefinedColumn, -source) %>%
  dplyr::filter(participantId %in%  healthCodes_with_broadsharing)%>%
  #Filter out underage participants
  filter(! participantId %in% underage_participants$participantId)
DistanceWalkingRunning.Table <- synBuildTable("HealthKit.DistanceWalkingRunning", "syn18492837", DistanceWalkingRunning.df)
DistanceWalkingRunning.Table <- synStore(DistanceWalkingRunning.Table)
synSetProvenance(DistanceWalkingRunning.Table,
                 Activity(used = "syn21292862",
                          executed = executed_code_GitHub_URL))


# FlightsClimbed
View(sh_hk_data_tobe_released[6,])
FlightsClimbed.df <- fread(synGet("syn21292863")$path, data.table = F) %>%
  dplyr::select(-undefinedColumn, -source) %>%
  dplyr::filter(participantId %in%  healthCodes_with_broadsharing)%>%
  #Filter out underage participants
  filter(! participantId %in% underage_participants$participantId)
FlightsClimbed.Table <- synBuildTable("HealthKit.FlightsClimbed", "syn18492837", FlightsClimbed.df)
FlightsClimbed.Table <- synStore(FlightsClimbed.Table)
synSetProvenance(FlightsClimbed.Table,
                 Activity(used = "syn21292863",
                          executed = executed_code_GitHub_URL))


# HeartRate
View(sh_hk_data_tobe_released[7,])
HeartRate.df <- fread(synGet("syn21292869")$path, data.table = F) %>%
  dplyr::select(-undefinedColumn, -source) %>%
  mutate(value = round(value, digits=3)) %>%
  dplyr::filter(participantId %in%  healthCodes_with_broadsharing)%>%
  #Filter out underage participants
  filter(! participantId %in% underage_participants$participantId)
HeartRate.Table <- synBuildTable("HealthKit.HeartRate", "syn18492837", HeartRate.df)
HeartRate.Table <- synStore(HeartRate.Table)
synSetProvenance(HeartRate.Table,
                 Activity(used = "syn21292869",
                          executed = executed_code_GitHub_URL))

# Height
View(sh_hk_data_tobe_released[8,])
Height.df <- fread(synGet("syn21292820")$path, data.table = F) %>%
  dplyr::select(-undefinedColumn, -source) %>%
  dplyr::filter(participantId %in%  healthCodes_with_broadsharing)%>%
  #Filter out underage participants
  filter(! participantId %in% underage_participants$participantId)
Height.Table <- synBuildTable("HealthKit.Height", "syn18492837", Height.df)
Height.Table <- synStore(Height.Table)
synSetProvenance(Height.Table,
                 Activity(used = "syn21292820",
                          executed = executed_code_GitHub_URL))


# StepCount
View(sh_hk_data_tobe_released[9,])
StepCount.df <- fread(synGet("syn21292818")$path, data.table = F) %>%
  dplyr::select(-undefinedColumn, -source) %>%
  dplyr::filter(participantId %in%  healthCodes_with_broadsharing)%>%
  #Filter out underage participants
  filter(! participantId %in% underage_participants$participantId)
StepCount.Table <- synBuildTable("HealthKit.StepCount", "syn18492837", StepCount.df)
StepCount.Table <- synStore(StepCount.Table)
synSetProvenance(StepCount.Table,
                 Activity(used = "syn21292818",
                          executed = executed_code_GitHub_URL))



