rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(rworldmap)
library(rpart)
oscar <- read.csv("~/Desktop/data analytics/Oscar_data/oscar.csv")
str(oscar)
colnames(oscar)
#EDA PART
# category information
sapply(oscar, function(x) sum(is.na(x)))
# drop unnecessary features
oscar[,c('release_date.year','release_date.month','release_date.day.of.month','release_date.day.of.week')] <- list(NULL)
colnames(oscar)
oscar<- na.omit(oscar) 
# exploraroty data analysis
#sapply(oscar, function(x) sum(is.na(x)))
#oscar certificate
table(oscar$certificate)
oscar$certificate[oscar$certificate=='Not Rated'] <- 'Unrated'
oscar$certificate[oscar$certificate==''] <- 'Unrated'
table(oscar$certificate)
counts <- table(oscar$certificate)
freq1 <- oscar %>%group_by(certificate) %>%summarize(count=n()) %>%arrange(desc(count))
freq1$certificate <- factor(freq1$certificate, levels=freq1$certificate)
ggplot(freq1, aes(x=certificate, y= count, fill=count)) + geom_bar(stat = "identity",position = "dodge")+geom_text(aes(label=count))+labs(title='the certificant of the movie')

#catogory of movie 
freq2 <- oscar%>%group_by(genre) %>%summarize(count=n()) %>%arrange(desc(count))%>%top_n(10)
freq2$genre<- factor(freq2$genre, levels=freq2$genre)     
g<-ggplot(freq2, aes(x=genre, y= count, fill=count)) + geom_bar(stat = "identity",position = "dodge")
g+theme(axis.text.x = element_text(angle=90))+geom_text(aes(label=count))+labs(title='the catogory of the movie')+scale_fill_gradient(low="blue", high="red")  
# duration distribution
d=density(oscar$duration)
plot(density(oscar$duration),main=" Density of movie duration")
polygon(d, col="blue", border="black")
min(oscar$duration)
max(oscar$duration)
mean(oscar$duration)
# awards nominated vs win
p1 <- hist(oscar$awards_nominations,main = "Histogram for nominations and wins",xlab = "number",bins=1)
min(oscar$awards_nominations)
min(oscar$awards_wins)
p2 <- hist(oscar$awards_wins,bins=1)                    
plot( p1, col=rgb(0,0,1,1/5), xlim=c(0,100))
plot( p2, col=rgb(1,0,0,1/5), xlim=c(0,100), add=T)
legend('topright',c('Nominations','Win'),
       fill = c(NCol, WCol), bty = 'n',
       border = NA)
#Oscar Win vs Nominations
oscar<- oscar %>%mutate(Oscar_Best_Picture_won = ifelse(Oscar_Best_Picture_won== "No",0,1))
oscar<- oscar %>%mutate(Oscar_Best_Picture_nominated = ifelse(Oscar_Best_Picture_nominated== "No",0,1))
oscar<- oscar %>%mutate(Oscar_Best_Actor_won = ifelse(Oscar_Best_Actor_won== "No",0,1))
oscar<- oscar %>%mutate(Oscar_Best_Actor_nominated = ifelse(Oscar_Best_Actor_nominated== "No",0,1))
oscar<- oscar %>%mutate(Oscar_Best_Director_won = ifelse(Oscar_Best_Director_won== "No",0,1))
oscar<- oscar %>%mutate(Oscar_Best_Director_nominated = ifelse(Oscar_Best_Director_nominated== "No",0,1))
oscar<- oscar %>%mutate(Oscar_Best_Actress_won = ifelse(Oscar_Best_Actress_won== "No",0,1))
oscar<- oscar %>%mutate(Oscar_Best_Actress_nominated = ifelse(Oscar_Best_Actress_nominated== "No",0,1))
oscar<- oscar %>%mutate(Oscar_Best_Supporting_Actor_won = ifelse(Oscar_Best_Supporting_Actor_won== "No",0,1))
oscar<- oscar %>%mutate(Oscar_Best_Supporting_Actor_nominated = ifelse(Oscar_Best_Supporting_Actor_nominated== "No",0,1))
oscar<- oscar %>%mutate(Oscar_Best_Supporting_Actress_won = ifelse(Oscar_Best_Supporting_Actress_won== "No",0,1))
oscar<- oscar %>%mutate(Oscar_Best_Supporting_Actress_nominated = ifelse(Oscar_Best_Supporting_Actress_nominated== "No",0,1))
oscar<- oscar %>%mutate(Oscar_Best_AdaScreen_won = ifelse(Oscar_Best_AdaScreen_won== "No",0,1))
oscar<- oscar %>%mutate(Oscar_Best_AdaScreen_nominated = ifelse(Oscar_Best_AdaScreen_nominated== "No",0,1))
oscar<- oscar %>%mutate(Oscar_Best_OriScreen_won = ifelse(Oscar_Best_OriScreen_won== "No",0,1))
oscar<- oscar %>%mutate(Oscar_Best_OriScreen_nominated = ifelse(Oscar_Best_OriScreen_nominated== "No",0,1))
oscar$oscar_win<-oscar$Oscar_Best_Picture_won+oscar$Oscar_Best_Director_won+oscar$Oscar_Best_Actor_won+oscar$Oscar_Best_Actress_won+
oscar$Oscar_Best_Supporting_Actor_won+oscar$Oscar_Best_Supporting_Actress_won+oscar$Oscar_Best_AdaScreen_won+oscar$Oscar_Best_OriScreen_won
p4 <- hist(oscar$oscar_win,main = "Histogram for oscar nominations and wins",xlab = "number")
p3 <- hist(oscar$Oscar_nominated)                    
plot( p4, col=rgb(1,0,0,1/5), xlim=c(0,10))
plot( p3, col=rgb(0,0,1,1/5), xlim=c(0,10),add=T)
legend('topright',c('Wins','Nominations'),bty = 'n',border = NA,fill = c(NCol, WCol))
#title(main="Histogram for oscar nominations and wins",xlab = "number")
oscar$Win <-oscar %>%mutate(oscar_win = ifelse(oscar_win== 0,0,1))
#oscar win socre
win_rate<-oscar %>% group_by(oscar_win) %>% summarise(n=n()) %>% arrange(desc(n))%>%top_n(5,wt=n)
win_rate$number <- factor(win_rate$win_rate, levels=win_rate$win_rate)
ggplot(win_rate, aes(x=oscar_win, y= n, fill=n)) + geom_bar(stat = "identity",position = "dodge")+geom_text(aes(label=n)) +labs(title='the number of oscar a movie won')+scale_fill_gradient(low="red", high="orange")       
oscar[oscar$oscar_win == 4,"movie"]                                                                     
#rationship between oscar win and gross income
sp3<-ggplot(oscar, aes(x=oscar_win, y=gross, color=gross)) + geom_point()
sp3
# Gradient between n colors
sp3+scale_color_gradientn(colours = rainbow(5))+labs(title='rationship between oscar win and gross income')
#boxplot for oscar win and movie rate
box <- ggplot(oscar, aes(oscar_win, rate))
box + geom_boxplot(aes(group=oscar_win))+labs(title='boxplot for oscar win and movie rate')

#Model part
#benchmodel- logistic regression
oscar$win[oscar$oscar_win !=0] <- 1
oscar$win[oscar$oscar_win ==0] <- 0
y<-select(oscar,win)
X<-select(oscar,year,certificate,duration,rate,metascore,votes,gross,popularity,user_reviews,critic_reviews,awards_wins,awards_nominations,win)
X1<-select(oscar,BAFTA_won,BAFTA_nominated,Screen_Actors_Guild_won,Screen_Actors_Guild_nominated,Critics_Choice_won,Critics_Choice_nominated,Directors_Guild_won,Directors_Guild_nominated,Golden_Globes_won,Golden_Globes_nominated,
           Directors_Guild_won,Directors_Guild_nominated,Producers_Guild_won,Producers_Guild_nominated,Art_Directors_Guild_won,Art_Directors_Guild_nominated,Writers_Guild_won,Writers_Guild_nominated,Costume_Designers_Guild_won,
           Costume_Designers_Guild_nominated,Online_Film_Television_Association_won,Online_Film_Television_Association_nominated,Online_Film_Critics_Society_won,Online_Film_Critics_Society_nominated,People_Choice_won,People_Choice_nominated,
           London_Critics_Circle_Film_won,London_Critics_Circle_Film_nominated,American_Cinema_Editors_won,American_Cinema_Editors_nominated,Hollywood_Film_won,Hollywood_Film_nominated,Austin_Film_Critics_Association_won,Austin_Film_Critics_Association_nominated,
           Denver_Film_Critics_Society_won,Denver_Film_Critics_Society_nominated,Boston_Society_of_Film_Critics_won,Boston_Society_of_Film_Critics_nominated,New_York_Film_Critics_Circle_won,New_York_Film_Critics_Circle_nominated,Los_Angeles_Film_Critics_Association_won,Los_Angeles_Film_Critics_Association_nominated)
library(caret)
set.seed(20)
trainIndex <- createDataPartition(X$win, p = .7,list = FALSE,times = 1)
Train <- X[ trainIndex,]
Test  <- X[-trainIndex,]
logisticmodel <- glm(win ~.,data=Train, family=binomial)
summary(logisticmodel)
Test$model_prob <- predict(logisticmodel, Test, type = "response")
Test <- Test  %>% mutate(model_pred = 1*(model_prob > .5) + 0)
Test <- Test %>% mutate(accurate = 1*(model_pred == win))
Accuracy <- sum(Test$accurate)/nrow(Test) 
Accuracy
library(pROC)
roc_log <- roc(Test$win,Test$model_prob) 
roc_log<-roc(response=Test$win, predictor= Test$model_prob, plot=TRUE, print.auc=TRUE)
auc_log<-auc(roc_log)
library(broom)
result<-tidy(logisticmodel)
#add x1 to x
nrow(X)
nrow(X1)
X_final<-cbind(X,X1)
trainIndex2 <- createDataPartition(X_final$win, p = .7,list = FALSE,times = 1)
Train_final <- X_final[ trainIndex2,]
Test_final  <- X_final[-trainIndex2,]
#Naive Bayers
data_for_NB<-X_final
data_for_NB$win <- as.factor(data_for_NB$win)
Train1 <- data_for_NB[ trainIndex2,]
Test1  <- data_for_NB[-trainIndex2,]
library(e1071)
Naive_Bayes_Model=naiveBayes(win ~., data=Train1)
Naive_Bayes_Model
nb_pred <- predict(Naive_Bayes_Model, Test1[-13], type="class")
class(nb_pred)
Test1$pred<- predict(Naive_Bayes_Model, Test1[-13], type="class")
class(Test1$pred)
Test1$pred<-as.numeric(Test1$pred)
#Confusion matrix to check accuracy
confusionMatrix(nb_pred, Test1$win )
roc_nb <- roc(Test1$win,Test1$pred) 
roc_nb$auc
roc_nb<-roc(response=Test1$win, predictor= factor(nb_pred, ordered = TRUE), plot=TRUE, print.auc=TRUE)
plot(roc_nb, col="red", lwd=3, main="ROC curve nb")
auc_nb<-auc(roc_nb)

#random forest
library("randomForest")
set.seed(100)
rf<-randomForest(as.factor(Train_final$win)~.,data=Train_final,mtry=10,proximity=TRUE)
err<-mean(rf$err.rate)
plot(rf)
#choose mtry
n<-length(names(X_final))
Error<-NULL
for(i in 1:(n-1)){
  rf<-randomForest(as.factor(Train_final$win)~.,data=Train_final,mtry=n,proximity=TRUE)
  err<-mean(rf$err.rate)
  Error[i]<-err
}
plot(Error,type="l")
m=which.min(Error) 
m
Error[41]
#m=41 #error=0.0736

#choose ntree
rf_train<-randomForest(as.factor(Train_final$win)~.,data=Train_final,mtry=41,ntree=500)
plot(rf_train)#choose tree=100
#run the model
library(caret)
win_rf<-randomForest(as.factor(Train_final$win)~.,data=Train_final,mtry=41,ntree=200,proximity=TRUE)
print(win_rf)
importance<-importance(win_rf)
importance<-importance%>% arrange(desc(MeandescreaseGiNi))%>%top_n(20)
library(caret)
varImp(win_rf)
varImpPlot(win_rf,type=2)

rf_prediction<-predict(win_rf, Test_final, type='class')
rf_value<-Test_final$win
rf_roc<-roc(rf_value,as.numeric(rf_prediction))
plot(rf_roc, print.auc = TRUE, auc.polygon = TRUE, legacy.axes = TRUE, 
     grid = c(0.1, 0.2), grid.col = c("green", "red"), max.auc.polygon = TRUE,  
     auc.polygon.col = "grey", print.thres = TRUE, xlab = "specificities", ylab = "sensitivities",
     main = "RandomForest-ROC Curve")
confusionMatrix(rf_prediction, as.factor(rf_value) )
#decision tree
rpart.fit <- rpart(as.factor(Train_final$win) ~ . , data =Train_final)
rpart_predict<-predict(rpart.fit, Test_final, type='class')
rpart_value<-Test_final$win
rpart_roc<-roc(rpart_value,as.numeric(rpart_predict))
plot(rpart_roc, print.auc = TRUE, auc.polygon = TRUE, legacy.axes = TRUE, 
     grid = c(0.1, 0.2), grid.col = c("green", "red"), max.auc.polygon = TRUE,  
     auc.polygon.col = "grey", print.thres = TRUE, xlab = "specificities", ylab = "sensitivities",
     main = "DecisionTree-ROC Curve")
library(rpart.plot)
rpart.plot(rpart.fit,branch=1, extra=106, under=TRUE, faclen=0,
           cex=0.6,main="DecisionTree")
confusionMatrix(rpart_predict, as.factor(rpart_value) )
# text mining in r
library(tm)
library(data.table) 
#create a corpus of descriptions
text_corpus <- Corpus(VectorSource(oscar$synopsis))
#check first 4 documents
inspect(text_corpus[1:4])
print(lapply(text_corpus[1:2], as.character))
#tolower
text_corpus <- tm_map(text_corpus, tolower)
print(as.character(text_corpus[[1]]))
#remove punctuation
text_corpus <- tm_map(text_corpus, removePunctuation)
print(as.character(text_corpus[[1]]))
#remove number
text_corpus <- tm_map(text_corpus, removeNumbers)
print(as.character(text_corpus[[1]]))
#remove whitespaces
text_corpus <- tm_map(text_corpus, stripWhitespace)
print(as.character(text_corpus[[1]]))
#remove stopwords
text_corpus <- tm_map(text_corpus, removeWords, c(stopwords('english')))
print(as.character(text_corpus[[1]]))
#convert to text document
text_corpus <- tm_map(text_corpus, PlainTextDocument)
#install.packages("SnowballC")
library('SnowballC')
library(data.table) 
library(ggthemes)
library(wordcloud)
#Using quanteda
library(quanteda)
my_corpus <- corpus(text_corpus)
summary(my_corpus)
myStemMat <- dfm(my_corpus, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
myStemMat[, 1:5]
myStemMat
topfeatures(myStemMat, 20)
mydfm<-dfm_trim(myStemMat, sparsity = 0.96)
#
docterm_corpus <- DocumentTermMatrix(text_corpus)
dim(docterm_corpus)
as.matrix(docterm_corpus)
new_docterm_corpus <- removeSparseTerms(docterm_corpus,sparse =0.96)
dim(new_docterm_corpus)
colS <- colSums(as.matrix(new_docterm_corpus))
length(colS)
doc_features <- data.table(name = attributes(colS)$names, count = colS)
#most frequent and least frequent words
library(ggthemes)
doc_features[order(-count)][1:10] #top 10 most frequent words
doc_features[order(count)][1:10] #least 10 frequent words
ggplot(doc_features[count>50],aes(name, count)) +geom_bar(stat = "identity",fill='lightblue',color='black')+theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme_economist()+scale_color_economist()
findAssocs(new_docterm_corpus,"love",corlimit = 0.1)
findAssocs(new_docterm_corpus,"find",corlimit = 0.5)
#create wordcloud
library(wordcloud)
wordcloud(names(colS), colS, min.freq = 5, scale = c(6,.1), colors = brewer.pal(6, 'Dark2'))

#create data set for training
processed_data <- as.data.table(as.matrix(mydfm))
names(processed_data)[names(processed_data) == "year"] <- "YEAR"
#combing the data
data_one <- cbind(X_final,processed_data)
trainIndex3 <- createDataPartition(data_one$win, p = .7,list = FALSE,times = 1)
Train_one <- data_one[ trainIndex3,]
Test_one  <- data_one[-trainIndex3,]
#adaboost
library(gbm)
library(pROC)
dimnames(data_one)
adaboost.gbm <- gbm(as.factor(win) ~ ., data=Train_one, dist="adaboost", n.tree = 25, interaction.depth = 10)
gbm_roc_predict <- predict(adaboost.gbm, Test_one, n.trees=25,type='response')
gbm_roc_value<-Test_one$win
gbm_roc<-roc(gbm_roc_value,gbm_roc_predict)
plot(gbm_roc, print.auc = TRUE, auc.polygon = TRUE, legacy.axes = TRUE, 
     grid = c(0.1, 0.2), grid.col = c("green", "red"), max.auc.polygon = TRUE,  
     auc.polygon.col = "grey", print.thres = TRUE, xlab = "specificities", ylab = "sensitivities",
     main = "Adaboost-ROC Curve")
confusionMatrix(as.factor(gbm_roc_predict) ,as.factor(gbm_roc_value))
