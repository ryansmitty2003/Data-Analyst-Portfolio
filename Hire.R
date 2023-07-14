Hire <- read.csv("~/Desktop/HireRTrain1.csv")
HireT <- read.csv("~/Desktop/HireRTrain1.csv")

colnames(Hire)
tree <- rpart(Hired ~  ., data = Hire, method = "class",control=rpart.control(minsplit = 100))
tree

model1<-rpart(Hired~., data=Hire[Hire$TikTokFOLLOWERS>4710 & Hire$Coding=='Weak',]); 
model2<-rpart(Hired~., data=Hire[Hire$TikTokFOLLOWERS<=4710 & Hire$Coding=='Weak',]); 
model3<-rpart(Hired~., data=Hire[Hire$TikTokFOLLOWERS>4710 & Hire$Coding=='OK',]); 
model4<-rpart(Hired~., data=Hire[Hire$TikTokFOLLOWERS<=4710 & Hire$Coding=='OK',]); 
model5<-rpart(Hired~., data=Hire[Hire$TikTokFOLLOWERS>4710 & Hire$Coding=='Excellent',]); 
model6<-rpart(Hired~., data=Hire[Hire$TikTokFOLLOWERS<=4710 & Hire$Coding=='Excellent',]); 




pred1 <- predict(model1, newdata=HireT[HireT$TikTokFOLLOWERS>4710 & HireT$Coding=='Weak',], type="class") 
pred2 <- predict(model2, newdata=HireT[HireT$TikTokFOLLOWERS<=4710 & HireT$Coding=='Weak',], type="class") 
pred3 <- predict(model3, newdata=HireT[HireT$TikTokFOLLOWERS>4710 & HireT$Coding=='OK',], type="class") 
pred4 <- predict(model4, newdata=HireT[HireT$TikTokFOLLOWERS<=4710 & HireT$Coding=='OK',], type="class") 
pred5 <- predict(model5, newdata=HireT[HireT$TikTokFOLLOWERS>4710 & HireT$Coding=='Excellent',], type="class") 
pred6 <- predict(model6, newdata=HireT[HireT$TikTokFOLLOWERS<=4710 & HireT$Coding=='Excellent',], type="class") 

myprediction<-HireT 

decision <- rep('No',nrow(myprediction)) 

decision[myprediction$TikTokFOLLOWERS>4710 & myprediction$Coding=='Weak'] <- as.character(pred1) 
decision[myprediction$TikTokFOLLOWERS<=4710 & myprediction$Coding=='Weak'] <- as.character(pred2) 
decision[myprediction$TikTokFOLLOWERS>4710 & myprediction$Coding=='OK'] <- as.character(pred3) 
decision[myprediction$TikTokFOLLOWERS<=4710 & myprediction$Coding=='OK'] <- as.character(pred4) 
decision[myprediction$TikTokFOLLOWERS>4710 & myprediction$Coding=='Excellent'] <- as.character(pred5) 
decision[myprediction$TikTokFOLLOWERS<=4710 & myprediction$Coding=='Excellent'] <- as.character(pred6)



myprediction$Hired <-decision 


CrossValidation::cross_validate(myprediction, tree, 4, .8)

error <- mean(Hire$Hired!= myprediction$Hired) 
error
