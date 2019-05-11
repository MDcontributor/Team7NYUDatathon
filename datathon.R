library(randomForest)
library(dplyr)
library(caret)
library(gtools)
library(ROCR)
setwd("/Users/av1936/Desktop/dataton")
pres <- read.table("pres.txt",as.is=T, sep="\t", header=T)
chart <- read.table("chart.txt",as.is=T, sep="\t", header=T,fill=T) 
rest <- read.table("admissions_joined.txt", sep="\t", header=T, fill=T, as.is=T)
population_adm <- read.csv("population_adm_v3.csv", sep=",", header=T)
names(population_adm) <- c("SUBJECT_ID", "HADM_ID","AGE", "MARITAL_STATUS",
                           "ETHNICITY", "GENDER", "EXPIRE_FLAG",  "LANGUAGE","RELIGION", "DIED_IN_HOSPITAL")
rest <- rest[,c(1,5:8,11:22)]
rest[rest$ADMISSION_TYPE=="",]$ADMISSION_TYPE <- "UNKNOWN"
rest[rest$INSURANCE=="",]$INSURANCE <- "UNKNOWN"
rest[is.na(rest$SEQ_NUM),]$SEQ_NUM <- "UNKNOWN"
rest[rest$COSTCENTER=="",]$COSTCENTER <- "UNKNOWN/NOT SPECIFIED"
rest <- rest[,-c(12)]
rest <- rest[,-c(14,15)]
rest[rest$SECTIONHEADER=="",]$SECTIONHEADER <- "UNKNOWN"


pres <- pres[,-2]
dmy <- dummyVars(" ~ .", data = pres)
trsf <- data.frame(predict(dmy, newdata = pres))
pres_trans <- aggregate(. ~ SUBJECT_ID, data=trsf, FUN=sum)

chart[is.na(chart$ITEMID),]$ITEMID <- "UNKNOWN"
chart <- chart[,-5]
chart <- chart[,-6]
chart <- chart[,-c(2,3)]
chart <- chart[!chart$LABEL%in%c(NA,""),]
chart[is.na(chart$CATEGORY),]$CATEGORY <- "UNKNOWN"
chart[chart$CATEGORY=="",]$CATEGORY <- "UNKNOWN"
#chart_num_subset <- chart[,c(2,)]
labels <- c(unique(chart$LABEL))
chart[chart$LABEL%in%c(labels[2]),]$VALUE
numbers <- c(1:7,10:18,21,24:30,34:37,39,40:46)
characters <- c(8,9,19,20,22,23,31:33,38)

chart_num <- chart[chart$LABEL%in%c(labels[c(numbers)]),]
#chart_num$VALUE <- as.numeric(chart$VALUE)
chart_num_values <- chart_num[,c(1,3,5)]
chart_num_values <- chart_num_values[ave(rep(1, nrow(chart_num_values)), chart_num_values$SUBJECT_ID, FUN=length)>1,]
subjects_df <- data.frame()
subjects <- c(unique(chart_num_values$SUBJECT_ID))#[-c(1191)]
#length(subjects[ave(seq_along(subjects), subjects, FUN = length) == 1])
for (i in subjects){
  #i <- "29894"
  chart_num_subject <- chart_num_values[chart_num_values$SUBJECT_ID%in%c(i),]
  chart_num_subject$VALUE <- as.numeric(chart_num_subject$VALUE)
  chart_num_subject <- chart_num_subject[!is.na(chart_num_subject$VALUE),]
  if (nrow(chart_num_subject)>0){
  num_trans_min <- aggregate(. ~ LABEL, data=chart_num_subject[,c(2,3)], FUN=min)
  labels <- as.character(num_trans_min$LABEL)
  num_trans_min <- as.data.frame(t(num_trans_min))
  num_trans_mean <- as.data.frame(t(aggregate(. ~ LABEL, data=chart_num_subject[,c(2,3)], FUN=mean)))
  num_trans_max  <- as.data.frame(t(aggregate(. ~ LABEL, data=chart_num_subject[,c(2,3)], FUN=max)))
  names(num_trans_max) <- c(paste(labels, "MAX", sep=" "))
  names(num_trans_min) <- c(paste(labels, "MIN", sep=" "))
  names(num_trans_mean) <- c(paste(labels, "MEAN", sep=" "))
  subject_var_df <- cbind(as.data.frame(i),num_trans_max,num_trans_min, num_trans_mean)[2,]
  row.names(subject_var_df) <- NULL
  subjects_df <- smartbind(subjects_df,subject_var_df,fill=0)}
}


#chart_char <- chart[chart$LABEL%in%c(labels[as.numeric(characters)]),]
#chart_char_no_subjects <- chart_char[,-1]
#dmy_char <- dummyVars("~ .", data = chart_char_no_subjects)
##trans_char <- data.frame(predict(dmy_char, newdata = chart_char_no_subjects))
##trans_chart$SUBJECT_ID <- c(chart_char$SUBJECT_ID)
#trans_char <- aggregate(. ~ SUBJECT_ID, data=trans_char, FUN=sum)

names(subjects_df)[1] <- "SUBJECT_ID"
subjects_df$SUBJECT_ID <- as.character(subjects_df$SUBJECT_ID)
population_adm$SUBJECT_ID <- as.character(population_adm$SUBJECT_ID)
pres_trans$SUBJECT_ID <- as.character(pres_trans$SUBJECT_ID)

join1 <- merge(subjects_df,population_adm, by="SUBJECT_ID")
join2 <- merge(join1,pres_trans, by="SUBJECT_ID")
#join2$DIED_IN_HOSPITAL <- as.character(join2$DIED_IN_HOSPITAL)
write.table(join2, "/Users/av1936/Desktop/dataton/table.txt", sep="\t", col.names = T, row.names = F,quote=F)
table <- read.table("/Users/av1936/Desktop/dataton/table.txt",sep="\t",header=T)
#table[is.na(table)]<- "NA"

#row.has.na <- apply(join2, 1, function(x){any(is.na(x))})
#predictors_no_NA <- join2[!row.has.na, ]

partition_auto <- createDataPartition(y=table$DIED_IN_HOSPITAL, p=0.9, list=FALSE)
train_set_auto <- table[partition_auto,]
test_set_auto <- table[-partition_auto,]

###Run Random Forest
control <- trainControl(method="repeatedcv", number=9, repeats=3, verbose = TRUE)
rf_model_auto <- train(DIED_IN_HOSPITAL~.,data=train_set_auto, method="rf", importance=TRUE, 
                       trControl = control,ntree = 1000)

rf_pred_auto_studies <- predict(rf_model_auto, test_set_auto)
prediction_auto_studies <- predict(rf_model_auto, test_set_auto)
rf_prediction_auto_studies <- prediction(rf_pred_auto_studies,test_set_auto$DIED_IN_HOSPITAL)
rf_perf_auto_studies <- performance(rf_prediction_auto_studies, "tpr", "fpr")
rf_auc_perf_auto_studies <- performance(rf_prediction_auto_studies, measure="auc")
rf_auc_auto_studies <- round(as.numeric(rf_auc_perf_auto_studies@y.values), digits = 3)
rf_auc_auto_studies

rf_importance <- as.data.frame(varImp(rf_model_auto)$importance)
rf_importance$variables <- row.names(rf_importance)



