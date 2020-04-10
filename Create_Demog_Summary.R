rm(list=ls())
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)
Sys.setenv(TZ='GMT')
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr', 'synapser'))
install_load('ggthemes', 'devtools', 'gridExtra', "tableone", "table1")
synapser::synLogin()






### Potential errors in self-reported stats
### controls who report data that is indicative of MS patient 
baselineChar_flt <- baselineChar %>% filter(dataGroups %in% c('control', 'ms_patient'))
#Create a variable list which we want in Table 1
listVars <- c("age", "gender", "race", "education", "health_insurance", 
              "employment")
#Define categorical variables
catVars <- c("gender", "race", "education","health_insurance", "employment")
#### Demog table without missing 
demogTable_withOut_missing = tableone::CreateTableOne(data=baselineChar_flt, vars=listVars, factorVars = catVars, 
                                                      strata = c("group"), includeNA = F)
demogTable_withOut_missing_mat <- print(demogTable_withOut_missing, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(demogTable_withOut_missing_mat, file = "elevateMS_overall_demog_table_withoutMissingData.csv")