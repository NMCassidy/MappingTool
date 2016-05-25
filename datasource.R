##This is the code used to produce the data required for the shiny app

library(pacman)
p_load("readxl", "dplyr", "tidyr")
#Raw Data
RawDta <- read_excel(file.path("S:", "G - Governance & Performance Mngmt",
                               "Salaries-Analysis", "Received Data","Dumfries and Galloway", "D and G payroll data 0416 of Copy of ImpSvsData (2).xlsx"),
                     sheet = 1, col_names = FALSE, col_types = rep("text",9))
dtaGeoHigher <- read.csv(file.path("S:", "G - Governance & Performance Mngmt",
                                   "Salaries-Analysis", "RStudio-Salaries","data", "csv", "DZs to Higher Geos.csv"),
                         stringsAsFactors = FALSE) %>%
  mutate(DATAZONE = trimws(as.character(DATAZONE)))

#Data Tidy
dtaClnPay <- RawDta %>%
  setNames(gsub(".", "", make.names(.[1,]), fixed = TRUE)) %>%
  slice(-1) %>%
  mutate_each(funs(destring), matches("PayscaleSalaryAnnual|FTESalary|ActualSalary|FTE"))


# Change Scipen for Nice Cuts function (remove scientific notation)
ordScipenSet <- getOption("scipen")
options(scipen = 999)

ClnPaySum <- dtaClnPay %>%
  #Data zone summaries
  group_by(Datazone2001) %>%
  #Summary Statistics
  mutate(NoDistEmp = n_distinct(Ref)) %>%
  summarise_each(funs(min, max, mean, sum), 
                 matches("FTESalary|ActualSalary|FTE|NoDistEmp")) %>%
  ungroup() %>%
  ##calculate net values for salary indicators
  mutate_each(funs(computeNetPay(.)), matches("FTESalary","ActualSalary")) %>%
  #round these values to 2 decimal places
  mutate_each(funs(round(., 2)), matches("FTESalary|ActualSalary")) %>%
  left_join(y = dtaGeoHigher, by = c("Datazone2001" = "DATAZONE")) %>%
  slice(- 321)

saveRDS(ClnPaySum, file = "S:/G - Governance & Performance Mngmt/Salaries-Analysis/RStudio-Salaries/Shiny Data/AButeData.rds")
