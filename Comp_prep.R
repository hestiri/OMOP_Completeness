####################################
###preparations to run the analysis
####################################
packages <- c("data.table","dplyr")
install.packages(packages, dependencies = TRUE)
require(data.table);require(dplyr)
# set the path to the directory that contains OMOP .txt tables
path = "~/OneDrive UW/OneDriveBusiness/omop/UW_Evaluation_2015-01-14"


##reading and storing pre-ROSITA *.txt tables
for (i in 1:length(filenames)) assign(filenames[i], read.table(paste(path,"/",filenames[i],sep=""), header = T, sep="|",quote = "",fill = TRUE))


##creating a table that includes table names and column names to store level of importance, date of the DQ test, and frequencies

# ##this is not essential
# ##saving names of loaded tables in a new vecotor
# table.names <- 
#   c(ls())
# #crate a data frame to store table names
# table.nam <- data.frame(table.names)
# table.nam
# ##this is not essential

##store test date in mm-YYYY format
test_date <- as.character(format(Sys.Date(),"%m-%Y"))


##at this step, manually assign importance level values for each column in each table
### X=Extremely important, H= Highly Important M= Medium Importance, L=Low Importance
###make sure number of columns are correct
#provider has 14 columns
d1 <- data.frame(TabNam="provider.txt",ColNam=c(names(provider.txt)), DQLVL=c("H","L","L","L","L","L","L","L","L","L","L","L","L","L"), 
                 abbr="prvd", test_date)
#care site has 10 columns
d2 <- data.frame(TabNam="care_site.txt",ColNam=c(names(care_site.txt)), DQLVL=c("H","H","L","L","L","L","L","L","L","L"), 
                 abbr="crst", test_date)
#condition_occurrence has 15 columns
d3 <- data.frame(TabNam="condition_occurrence.txt",ColNam=c(names(condition_occurrence.txt)), DQLVL=c("H","L","L","L","L","L","L","L","L","L","L","L","L","L","L"),
                 abbr="cndoc", test_date)
#death has 9 columns
d4 <- data.frame(TabNam="death.txt",ColNam=c(names(death.txt)), DQLVL=c("H","L","L","L","L","L","L","L","L"),
                 abbr="deth", test_date)
#drug_exposure has 20 columns
d5 <- data.frame(TabNam="drug_exposure.txt",ColNam=c(names(drug_exposure.txt)), DQLVL=c("H","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L"),
                 abbr="drgex", test_date)
#observation has 21 columns
d6 <- data.frame(TabNam="observation.txt",ColNam=c(names(observation.txt)), DQLVL=c("H","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L"),
                 abbr="obsr", test_date)
#organization has 8 columns
d7 <- data.frame(TabNam="organization.txt",ColNam=c(names(organization.txt)), DQLVL=c("H","L","L","L","L","L","L","L"),
                 abbr="org", test_date)
#person has 17 columns
d8 <- data.frame(TabNam="person.txt",ColNam=c(names(person.txt)), DQLVL=c("X","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L"),
                 abbr="prsn", test_date)
#procedure_occurrence has 12 columns
d9 <- data.frame(TabNam="procedure_occurrence.txt",ColNam=c(names(procedure_occurrence.txt)), DQLVL=c("H","L","L","L","L","L","L","L","L","L","L","L"),
                 abbr="prcoc", test_date)
# visit_occurrence has 12 columns
d10 <- data.frame(TabNam="visit_occurrence.txt",ColNam=c(names(visit_occurrence.txt)), DQLVL=c("H","L","L","L","L","L","L","L","L","L","L","L"),
                  abbr="vstoc", test_date)

# create the DQTBL with all the values
DQTBL <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10)

#remove the single tables
rm(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10)

DQTBL$MSFRQ <- 1
DQTBL$FRQ <- 0
DQTBL$UNIQFRQ <- 0