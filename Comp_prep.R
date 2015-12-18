####################################
###preparations to run the analysis
####################################
install.packages("data.table");require(data.table)
# set the working directory
setwd("~/OneDrive UW/OneDriveBusiness/omop/UW_Evaluation_2015-01-14")


for (i in 1:length(dir())) assign(dir()[i], read.table(dir()[i], header = T, sep="|",quote = "",fill = TRUE))
