################################################################################
##extracting DQ Issues from the table and storing them into a long table called RESULTS
################################################################################
                # ULTIMATELY AN AUTOMATED PROCEDURE WILL REPLACE THE FOLLOWING CODES   
                # SOMETHING LIKE THE BELOW
                 # for (i in 1:length(filenames)) assign(paste("ISSUES",filenames[i],sep = ""), 
                                                              #WORKS UP TO ABOUT HERE
                      #                                       (sapply(
                      #                                         1:dim(get(filenames[i]))[2], function(j) {c(care_site.txt[grep("_MS_", care_site.txt[,j]), j])}
                      #                                               )
                      #                                        )
                      #                                       )


##extracting DQ Issues from the table and storing them into a new list
ISSUES1 <- sapply(1:dim(care_site.txt)[2], function(j) {c(care_site.txt[grep("_MS_", care_site.txt[,j]), j])})
ISSUES2 <- sapply(1:dim(condition_occurrence.txt)[2], function(j) {c(condition_occurrence.txt[grep("_MS_", condition_occurrence.txt[,j]), j])})
ISSUES3 <- sapply(1:dim(death.txt)[2], function(j) {c(death.txt[grep("_MS_", death.txt[,j]), j])})
ISSUES4 <- sapply(1:dim(drug_exposure.txt)[2], function(j) {c(drug_exposure.txt[grep("_MS_", drug_exposure.txt[,j]), j])})
ISSUES5 <- sapply(1:dim(observation.txt)[2], function(j) {c(observation.txt[grep("_MS_", observation.txt[,j]), j])})
ISSUES6 <- sapply(1:dim(organization.txt)[2], function(j) {c(organization.txt[grep("_MS_", organization.txt[,j]), j])})
ISSUES7 <- sapply(1:dim(person.txt)[2], function(j) {c(person.txt[grep("_MS_", person.txt[,j]), j])})
ISSUES8 <- sapply(1:dim(procedure_occurrence.txt)[2], function(j) {c(procedure_occurrence.txt[grep("_MS_", procedure_occurrence.txt[,j]), j])})
ISSUES9 <- sapply(1:dim(provider.txt)[2], function(j) {c(provider.txt[grep("_MS_", provider.txt[,j]), j])})
ISSUES10 <- sapply(1:dim(visit_occurrence.txt)[2], function(j) {c(visit_occurrence.txt[grep("_MS_", visit_occurrence.txt[,j]), j])})


#is.list(ISSUES)
##unlist the ISSUES list 
ISSUES1 <- unlist(ISSUES1)
ISSUES2 <- unlist(ISSUES2)
ISSUES3 <- unlist(ISSUES3)
ISSUES4 <- unlist(ISSUES4)
ISSUES5 <- unlist(ISSUES5)
ISSUES6 <- unlist(ISSUES6)
ISSUES7 <- unlist(ISSUES7)
ISSUES8 <- unlist(ISSUES8)
ISSUES9 <- unlist(ISSUES9)
ISSUES10 <- unlist(ISSUES10)
##store the unlisted ISSUES into data frames, RES(s)
RES1<- data.frame(ISSUES1)
colnames(RES1)[1] <- "ISSUES"
RES2<- data.frame(ISSUES2)
colnames(RES2)[1] <- "ISSUES"
RES3<- data.frame(ISSUES3)
colnames(RES3)[1] <- "ISSUES"
RES4<- data.frame(ISSUES4)
colnames(RES4)[1] <- "ISSUES"
RES5<- data.frame(ISSUES5)
colnames(RES5)[1] <- "ISSUES"
RES6<- data.frame(ISSUES6)
colnames(RES6)[1] <- "ISSUES"
RES7<- data.frame(ISSUES7)
colnames(RES7)[1] <- "ISSUES"
RES8<- data.frame(ISSUES8)
colnames(RES8)[1] <- "ISSUES"
RES9<- data.frame(ISSUES9)
colnames(RES9)[1] <- "ISSUES"
RES10<- data.frame(ISSUES10)
colnames(RES10)[1] <- "ISSUES"


##saving all error cells is one table, RESULTS
RESULTS <- rbind(RES1,RES2,RES3,RES4,RES5,RES6,RES7,RES8,RES9,RES10)
rm(RES1,RES2,RES3,RES4,RES5,RES6,RES7,RES8,RES9,RES10)

head(RESULTS, 100)



#####plotting

Flagplot <- ggplot(data=DQTBL, aes(x=ColNam, y=MSFRQ, fill=factor(DQLVL))) +
  geom_bar(stat="identity", width = 1) +
  scale_fill_manual(values=c("red","green","dark red")) +
  facet_wrap( ~ TabNam, ncol = 5) +
  ggtitle("Frequency of Missing Data") +
  xlab("Column") +
  ylab("Frequency") 

Flagplot

person_id<- filter(DQTBL, ColNam == "person_id")
person_id$flag <- as.factor(ifelse(person_id$FRQ > mean(person_id$FRQ), "Not OK", "OK" ))

plot2 <- ggplot(data=person_id, aes(x=TabNam, y=UNIQFRQ)) +
  geom_bar(stat="identity", width = .5, aes(fill=flag)) +
  scale_fill_manual(values=c("green", "red")) +
  ggtitle(paste("Count of Unique ",unique(person_id$ColNam)," in Tables with ",unique(person_id$ColNam),
                sep="")) +
  xlab(" Table Name") +
  ylab("Frequency of Unique Values") 

care_site_id<- filter(DQTBL, ColNam == "care_site_id")
care_site_id$flag <- as.factor(ifelse(care_site_id$FRQ > mean(care_site_id$FRQ), "Not OK", "OK" ))

plot3 <- ggplot(data=care_site_id, aes(x=TabNam, y=UNIQFRQ)) +
  geom_bar(stat="identity", width = .5, aes(fill=flag)) +
  scale_fill_manual(values=c("green", "red")) +
  ggtitle(paste("Count of Unique ",unique(care_site_id$ColNam)," in Tables with ",unique(care_site_id$ColNam),
                sep="")) +
  xlab(" Table Name") +
  ylab("Frequency of Unique Values")


visit_occurrence_id<- filter(DQTBL, ColNam == "visit_occurrence_id")
visit_occurrence_id$flag <- as.factor(ifelse(visit_occurrence_id$FRQ > mean(visit_occurrence_id$FRQ), "Not OK", "OK" ))

plot4 <- ggplot(data=visit_occurrence_id, aes(x=TabNam, y=UNIQFRQ)) +
  geom_bar(stat="identity", width = .5, aes(fill=flag)) +
  scale_fill_manual(values=c("green", "red")) +
  ggtitle(paste("Count of Unique ",unique(visit_occurrence_id$ColNam)," in Tables with ",unique(visit_occurrence_id$ColNam),
                sep="")) +
  xlab(" Table Name") +
  ylab("Frequency of Unique Values")

location_id<- filter(DQTBL, ColNam == "location_id")
location_id$flag <- as.factor(ifelse(location_id$FRQ > mean(location_id$FRQ), "Not OK", "OK" ))

plot5 <- ggplot(data=location_id, aes(x=TabNam, y=UNIQFRQ)) +
  geom_bar(stat="identity", width = .5, aes(fill=flag)) +
  scale_fill_manual(values=c("green", "red")) +
  ggtitle(paste("Count of Unique ",unique(location_id$ColNam)," in Tables with ",unique(location_id$ColNam),
                sep="")) +
  xlab(" Table Name") +
  ylab("Frequency of Unique Values")


organization_id<- filter(DQTBL, ColNam == "organization_id")
organization_id$flag <- as.factor(ifelse(organization_id$FRQ > mean(organization_id$FRQ), "Not OK", "OK" ))

plot6 <- ggplot(data=organization_id, aes(x=TabNam, y=UNIQFRQ)) +
  geom_bar(stat="identity", width = .5, aes(fill=flag)) +
  scale_fill_manual(values=c("green", "red")) +
  ggtitle(paste("Count of Unique ",unique(organization_id$ColNam)," in Tables with ",unique(organization_id$ColNam),
                sep="")) +
  xlab(" Table Name") +
  ylab("Frequency of Unique Values")



place_of_service_source_value<- filter(DQTBL, ColNam == "place_of_service_source_value")
place_of_service_source_value$flag <- as.factor(ifelse(place_of_service_source_value$FRQ > mean(place_of_service_source_value$FRQ), "Not OK", "OK" ))

plot7 <- ggplot(data=place_of_service_source_value, aes(x=TabNam, y=UNIQFRQ)) +
  geom_bar(stat="identity", width = .5, aes(fill=flag)) +
  scale_fill_manual(values=c("green", "red")) +
  ggtitle(paste("Count of Unique ",unique(place_of_service_source_value$ColNam)," in Tables with ",unique(place_of_service_source_value$ColNam),
                sep="")) +
  xlab(" Table Name") +
  ylab("Frequency of Unique Values")
# associated_provider_id
# place_of_service_source_value


grid.arrange(plot2, plot3, plot4, plot5, plot6, plot7, nrow=3)

