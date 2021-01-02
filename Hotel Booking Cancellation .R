### EDA of Hotel Booking ###

# Load data and make a data frame called “HotelBooking”
HotelBooking <- read.csv(file = "/Users/yuki0416/Desktop/Data Mining Applications/hotel_bookings.csv")

# #---Step 1: Variable Identification---

# Find data type
str(HotelBooking)

# Adjust data type (Int -> Factor)
HotelBooking$is_repeated_guest <- as.factor(ifelse(HotelBooking$is_repeated_guest==1, "Yes", "No"))
HotelBooking$arrival_date_year <- as.factor(HotelBooking$arrival_date_year) 
HotelBooking$arrival_date_week_number <- as.factor(HotelBooking$arrival_date_week_number) 
HotelBooking$arrival_date_day_of_month <- as.factor(HotelBooking$arrival_date_day_of_month) 
HotelBooking$is_canceled=as.factor(HotelBooking$is_canceled)

# Adjust data type (Int -> Numeric)
HotelBooking$is_canceled=as.numeric(ifelse(HotelBooking$is_canceled==1, 1, 0))

# Adjust data type (Character -> Factor)
HotelBooking$arrival_date_month <- as.factor(HotelBooking$arrival_date_month)
# Change the order of levels 
HotelBooking$arrival_date_month <- factor(HotelBooking$arrival_date_month,
                                          levels = c("January", "February", "March", "April", 
                                                     "May", "June", "July", "August", "September", 
                                                     "October", "November", "December"))
# #---Step 2: Deal With Missing Value---

# Test Missing Value
colSums(is.na(HotelBooking)) #( Column Children has 4 missing value)

# Remove rows with NA
HotelBooking <- na.omit(HotelBooking)
sum(is.na(HotelBooking))

# #---Step 3: Univariate Analysis ---

# Load package
library(ggplot2)
library(scales)

# 1) Continuous Variables
# a) lead_time 
summary(HotelBooking$lead_time)

# Box plot 
leadtime_bp <- ggplot(data = HotelBooking, mapping = aes(x =lead_time , y = " ")) +
  geom_boxplot(fill = "dark gray",outlier.color = "red", outlier.shape = 1) +
  ggtitle(expression(atop("Boxplot of lead time", atop(italic("Number of days from booking to arriving"))))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))+ labs(x = "Days (lead_time)", y= "") 
leadtime_bp

OutVals = boxplot(HotelBooking$lead_time)$out
a <- which(HotelBooking$lead_time %in% OutVals)
length(a) #checking outliers

# Histogram
leadtime_his <- ggplot(data = HotelBooking, aes(x = lead_time)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)),bins = 30, colour="black", fill = "dark gray")+
  ggtitle("Histogram of lead time")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(name = "Proportion", labels=scales::percent, limits=c(0,0.15), breaks = seq(0,0.15,0.05)) +
  scale_x_continuous(name = " Days (lead_time)",limits=c(0,500), breaks=seq(0,500,50)) 
leadtime_his 

# b) ADR
summary(HotelBooking$adr)

# Box plot (check outliers)
leadtime_bp <- ggplot(data = HotelBooking, mapping = aes(x =adr , y = " ")) +
  geom_boxplot(fill = "dark gray",outlier.color = "red", outlier.shape = 1) +
  ggtitle(expression(atop("Boxplot of lead time", atop(italic("Number of days from booking to arriving"))))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))+ labs(x = "Average Daily Rate", y= "") 
leadtime_bp

OutVals = boxplot(HotelBooking$adr)$out
a <- which(HotelBooking$adr %in% OutVals)
length(a) #checking outliers

# Histogram
adr_his <- ggplot(data = HotelBooking, aes(x = adr)) + 
  geom_histogram(aes(y=(..count..)/sum(..count..)), bins = 30,colour="black", fill="dark gray") + 
  ggtitle("Histogram of Average Daily Rate")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(name = " Average Daily Rate ",limits=c(0,300), breaks=seq(0,300,20))   +
  scale_y_continuous(name = "Proportion  ", labels=scales::percent, limits=c(0,0.12)) 
adr_his


# 2) Categorical Variables

# a) Cancellation
Cancel_y <- sum(HotelBooking$is_canceled=="0")
Cancel_n <- sum(HotelBooking$is_canceled=="1")
slices <- c(Cancel_n,Cancel_y)
pct <- round(slices/sum(slices)*100,3)
pie_churn<- data.frame(
  group = c("Canaelled", "Not Cancelled"),
  value = c(pct)) #create data frame for pie 

Cancellation_pie = ggplot(pie_churn, aes(x="", y=value, fill=group))+geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0) + geom_text(aes(label =sprintf("%.2f%%", value)), 
                                        position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values=c("Blue","gray"))+ 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        panel.grid=element_blank(),
        plot.title = element_text(hjust=0.5)) +
  labs(fill="Cancellation",
       x=NULL,y=NULL,title="Pie Chart of Cancellation")
Cancellation_pie

# #---Step 4: Deal With Outliers---

# leadt_ime
leadtime_OutVals <- boxplot(HotelBooking$lead_time)$out
HotelBooking <- subset(HotelBooking, !(lead_time %in% leadtime_OutVals))

# adr
adr_OutVals <- boxplot(HotelBooking$adr)$out
HotelBooking <- subset(HotelBooking, !(adr %in% adr_OutVals))
HotelBooking  <- HotelBooking[which(HotelBooking$adr >=0),]


#---Step 5: Bi-variate Analysis--- 

# 1) Categorical & Categorical Variable
# 100% Stacked Bar Chart (sbc)
library(scales)
# a) hotel & is_canceled
Hotel.cancel_sbc <- ggplot(HotelBooking, aes(hotel,fill=factor(is_canceled))) + 
  geom_bar(position = 'fill') + scale_fill_manual(values=c("grey","blue"),labels = c("No", "Yes")) +
  labs(x=NULL, y = NULL, fill="Canceled") + scale_y_continuous(labels = percent)
Hotel.cancel_sbc

# b) deposit_type & is_canceled
Deposit.cancel_sbc <- ggplot(HotelBooking, aes(deposit_type,fill=factor(is_canceled))) + 
  geom_bar(position = 'fill') + scale_fill_manual(values=c("grey","blue"),labels = c("No", "Yes")) +
  labs(x=NULL, y = NULL, fill="Canceled") + scale_y_continuous(labels = percent)
Deposit.cancel_sbc

# c) is_repeated_guest & is_canceled
Guest_labels <- c("New Guest", "Repeated Guest") # Create X label of "is_repeated_guest"
RepeatedGuest.cancel_sbc <- ggplot(HotelBooking, aes(is_repeated_guest,fill=factor(is_canceled))) + 
  geom_bar(position = 'fill') + scale_fill_manual(values=c("grey","blue"),labels = c("No", "Yes")) +
  labs(x=NULL, y = NULL, fill="Canceled") + scale_y_continuous(labels = percent) +
  scale_x_discrete(labels=Guest_labels) 
RepeatedGuest.cancel_sbc

# d) market_segment & is_canceled
market_segment_sbc <- ggplot(HotelBooking, aes(market_segment,fill=factor(is_canceled))) + 
  geom_bar(position = 'fill') + scale_fill_manual(values=c("grey","blue"),labels = c("No", "Yes")) +
  labs(x=NULL, y = NULL, fill="Canceled") + scale_y_continuous(labels = percent)
market_segment_sbc


# 2) Categorical & Numerical Variable
# Boxplot (bp)
Cancel_labels <- c("Not Canceled", "Canceled") # Create X label of "is_canceled"

# a) lead_time & is_canceled
leadtime.cancel_bp <- ggplot(HotelBooking, aes(y= lead_time, x = factor(is_canceled), fill = factor(is_canceled))) +
  geom_boxplot(outlier.colour = "yellow") + theme(legend.position = "none") + labs(x = NULL, y="Lead  Time (# of Days)") +
  scale_fill_manual(values=c("grey","blue")) + scale_x_discrete(labels=Cancel_labels) 
leadtime.cancel_bp 

# b) adr & is_canceled
adr.cancel_bp <- ggplot(HotelBooking, aes(y= adr, x = factor(is_canceled), fill = factor(is_canceled))) +
  geom_boxplot(outlier.colour = "yellow") + theme(legend.position = "none") + labs(x = NULL, y="Average Daily Rate") +
  scale_fill_manual(values=c("grey","blue")) + scale_x_discrete(labels=Cancel_labels) 
adr.cancel_bp 

# c) total_of_special_requests & is_canceled
total_of_special_requests_bp <- ggplot(HotelBooking, aes(y= total_of_special_requests, x = factor(is_canceled), fill = factor(is_canceled))) +
  geom_boxplot(outlier.colour = "yellow") + theme(legend.position = "none") + labs(x = NULL, y="Total of Special Requests") +
  scale_fill_manual(values=c("grey","blue")) + scale_x_discrete(labels=Cancel_labels) 
total_of_special_requests_bp+ylim(0,2)

# Table
#total_of_special_requests & is_canceled
a<-subset(HotelBooking,is_canceled=="1")
b<-subset(HotelBooking,is_canceled=="0")
fivenum(a$previous_cancellations)
fivenum(b$previous_cancellations)
table(a$previous_cancellations)
table(b$previous_cancellations)
transform(as.data.frame(table(a$previous_cancellations)),percentage_column=Freq/nrow(a)*100)
transform(as.data.frame(table(b$previous_cancellations)),percentage_column=Freq/nrow(b)*100)

# #---Step 6: Check Predictor---

# Check for Null
nrow(subset(HotelBooking, company=="NULL"))/nrow(HotelBooking) # 93.97% Null

# Number of Categories <agent, country>
str(HotelBooking$agent) # 334
str(HotelBooking$country) # 178

# Percentage of Each Categories
transform(as.data.frame(table(HotelBooking$agent)),percentage_column=Freq/nrow(HotelBooking)*100)
transform(as.data.frame(table(HotelBooking$country)),percentage_column=Freq/nrow(HotelBooking)*100)

# Remove company, agent, country, reservation_status, reservation_status_date variables
samples <- subset( HotelBooking, select = -c(company, agent, country, reservation_status, reservation_status_date) )

### Logistic Regression ###

# 1) Change Data Type before Computing Information Values
# a) Character -> Factor
samples <- as.data.frame(unclass(samples))

# b) Int -> Numeric
int_var <- c(3,8,9,10,11,12,17,18,21,23,26,27)
samples[,int_var] <- lapply(samples[int_var], as.numeric)########
str(samples)

#c) Compute Information Values (IV)
library(Information)
infoTables <- create_infotables(data = samples,y = 'is_canceled',parallel = FALSE)
print(infoTables$Summary, row.names=FALSE)


# 2) Create Training and Test Samples
# b) Split samples into Training and Testing data
set.seed(100)
ind <- sample(2, nrow(samples), replace=T, prob=c(0.7, 0.3))
train.samples <- samples[ind==1, ]
test.samples<- samples[ind==2, ]

# c) Double check if the Cancellation Rate of two sets are close
prop.table(table(samples$is_canceled))
prop.table(table(train.samples$is_canceled))
prop.table(table(test.samples$is_canceled))


# 3) Logit Model 1 
# a) Build model 1 (IV > 0.02)
log.Mod1_start.time <- Sys.time()
log.Mod1 <- glm(is_canceled ~ deposit_type+lead_time+previous_cancellations+
                  market_segment+total_of_special_requests+booking_changes+
                  distribution_channel+hotel+adr+customer_type+stays_in_week_nights+
                  is_repeated_guest+adults+reserved_room_type,
                data=train.samples, family=binomial)
log.Mod1_end.time <- Sys.time()
log.Mod1_time.taken <- log.Mod1_end.time - log.Mod1_start.time
summary(log.Mod1) 

# b) Diagnose model 1 - VIF check multicollinearity
library(car)
vif(log.Mod1) # VIF of market_segment and distribution_channel > 5

# 4) Logit Model 2
# a) Build model 2 (remove distribution_channel from model 1)
log.Mod2_start.time <- Sys.time()
log.Mod2 <- glm(is_canceled ~ deposit_type+lead_time+previous_cancellations+
                  market_segment+total_of_special_requests+booking_changes+
                  hotel+adr+customer_type+stays_in_week_nights+
                  is_repeated_guest+adults+reserved_room_type,
                data=train.samples, family=binomial)
log.Mod2_end.time <- Sys.time()
log.Mod2_time.taken <- log.Mod2_end.time - log.Mod2_start.time
summary(log.Mod2) 

# b) Diagnose model 2 - VIF check multicollinearity
vif(log.Mod2) # all VIF < 5

# c) Predict on test samples
log.Mod2.predict <- plogis(predict(log.Mod2, test.samples))

# d) Decide on optimal prediction probability cutoff
library(InformationValue)
optCutOff <- optimalCutoff(test.samples$is_canceled, log.Mod2.predict)[1] 
optCutOff # 0.48: score above this is "Yes", below is "No"

# e) ROC Curve
library(ggplot2)
plotROC(test.samples$is_canceled, log.Mod2.predict) # 0.8236

# f) Confusion Matrix (CM)
log.Mod2_CM <- confusionMatrix(test.samples$is_canceled, log.Mod2.predict, threshold = optCutOff)

# Columns: actuals ; Rows: predicteds

# g) Sensitivity and Specificity
sensitivity(test.samples$is_canceled, log.Mod2.predict, threshold = optCutOff) # 0.5689783
specificity(test.samples$is_canceled, log.Mod2.predict, threshold = optCutOff) # 0.9254044

# h) F1 Score
library(MLmetrics)
pred <- ifelse(log.Mod2.predict < optCutOff, 0, 1)
log.Mod2_f1 <- F1_Score(y_pred = pred, y_true = test.samples$is_canceled, positive = "1") # 0.669026

### Decision tree ###
library(rpart)
library(rpart.plot)
# 1) Build decision tree (dt)
tree.formula <- is_canceled ~.
dt1_start.time <- Sys.time()
dt1 <- rpart(tree.formula, data=train.samples, method = 'class',parms=list(split="information"))
dt1_end.time <- Sys.time()
dt1_time.taken <- dt1_end.time - dt1_start.time

# 2) Plot tree
rpart.plot(dt1)

# 3) Features Importance
dt1$variable.importance
dt1.importance <- as.data.frame(dt1$variable.importance)
dt1.importance
#install.packages("vip")
library(vip)
vip(dt1)

# 4) Predict on test samples
dt1.predict <- predict(dt1, newdata = test.samples, type='class')

# 5) Diagnose model 
# a) ROC Curve
dt1_predict <- predict(dt1,type = "prob", newdata = test.samples)
plotROC(test.samples$is_canceled, dt1_predict[,2]) # 0.8261

# b) Confusion Matrix (CM)
dt1_CM<-confusionMatrix(test.samples$is_canceled,as.numeric(as.character(dt1.predict)))

# c) Sensitivity and Specificity
sensitivity(test.samples$is_canceled, as.numeric(as.character(dt1.predict))) # 0.6043539
specificity(test.samples$is_canceled, as.numeric(as.character(dt1.predict))) # 0.9276889

# d) F1 Score
dt1_f1<-F1_Score(y_pred = dt1.predict, y_true = test.samples$is_canceled, positive = "1") # 0.6977674


### Random Forest Model ###
library(randomForest)
# 1) Make dependent variable as a factor (categorical) 
samples$is_canceled = as.factor(samples$is_canceled)
train.samples$is_canceled = as.factor(train.samples$is_canceled)
test.samples$is_canceled = as.factor(test.samples$is_canceled)


# 2) Run the Random Forest model (rf)
# Set ntree =100
set.seed(100)
rf1_start.time <- Sys.time()
rf1 <-randomForest(is_canceled~., data=train.samples, ntree=100) 
rf1_end.time <- Sys.time()
rf1_time.taken <- rf1_end.time - rf1_start.time
rf1_time.taken # 47.65503 secs
print(rf1) # OOB error rate: 14.05%


# 3) Fine tune random forest (rf1): ntree
# Set ntree = 500 (default)
set.seed(100)
rf2_start.time <- Sys.time()
rf2 <-randomForest(is_canceled~., data=train.samples, ntree=500) 
rf2_end.time <- Sys.time()
rf2_time.taken <- rf2_end.time - rf2_start.time
rf2_time.taken # 4.248535 mins
print(rf2) # OOB error rate: 13.67%
plot(rf2)
abline(h=0.1367, col="blue", lty=2)

# Set ntree=300
set.seed(100)
rf3_start.time <- Sys.time()
rf3 <-randomForest(is_canceled~., data=train.samples, ntree=300) 
rf3_end.time <- Sys.time()
rf3_time.taken <- rf3_end.time - rf3_start.time
rf3_time.taken # 2.65993 mins
print(rf3) # OOB error rate: 13.73%


# 4) Fine tune random forest (rf2) : mtry
rf2_mtry <- tuneRF(train.samples[-c(2)],train.samples$is_canceled, 
                   ntreeTry=500, stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
rf2_mtry
plot(rf2_mtry)


# 5) Random Forest Feature Importance
importance(rf2)
varImpPlot(rf2, sort=T, n.var = 10, main = 'Top 10 Feature Importance')


# 6) Predict on test samples
rf2.predict <- predict(rf2, newdata = test.samples, type = "class") 


# 7) Diagnose model 
# a) ROC Curve
rf2_predict <- predict(rf2,type = "prob", newdata = test.samples)
plotROC(test.samples$is_canceled, rf2_predict[,2]) # 0.9254
table((rf2_predict[,2]>=0.5))
# b) Confusion Matrix (CM)
rf2_CM <- confusionMatrix(test.samples$is_canceled, as.numeric(as.character(rf2.predict)))

# c) Sensitivity and Specificity
sensitivity(test.samples$is_canceled, as.numeric(as.character(rf2.predict))) # 0.7776862
specificity(test.samples$is_canceled, as.numeric(as.character(rf2.predict))) # 0.9155672

# d) F1 Score
rf2_f1<-F1_Score(y_pred = rf2.predict, y_true = test.samples$is_canceled, positive = "1") # 0.8071377



### GBM ###
# 1)Adjust data type (change all the predictors to factor)


# 2) Create Hyperparameter Grid
library(gbm)
hyper_grid <- expand.grid(
  shrinkage = c(.1, .3),
  interaction.depth = c(1, 3),
  n.minobsinnode = c(5, 10), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0 )                    # a place to dump results
# total number of combinations
nrow(hyper_grid) #8
# randomize data
random_index <- sample(1:nrow(train.samples), nrow(train.samples))
random_train_samples <- train.samples[random_index, ]


# 3) Grid Search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(100)
  # train model
  gbm.tune <- gbm(
    formula = is_canceled ~ .,
    distribution = "bernoulli",
    data = random_train_samples,
    n.trees = 1000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE)
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))}

hyper_grid


# 4) Modify Hyperparameter Grid
library(gbm)
hyper_grid <- expand.grid(
  shrinkage = c(.1, .01),
  interaction.depth = c(3, 5),
  n.minobsinnode = c(5, 10), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0 )                    # a place to dump results
# total number of combinations
nrow(hyper_grid) #8
# randomize data
random_index <- sample(1:nrow(train.samples), nrow(train.samples))
random_train_samples <- train.samples[random_index, ]


# 5) Grid Search <2>
gbm_start.time <- Sys.time()
for(i in 1:nrow(hyper_grid)) {
  # reproducibility
  set.seed(100)
  # train model
  gbm.tune <- gbm(
    formula = is_canceled ~ .,
    distribution = "bernoulli",
    data = random_train_samples,
    n.trees = 2000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE)
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))}
gbm_end.time <- Sys.time()
gbm_time.taken <- gbm_end.time - gbm_start.time #9.051648 mins

hyper_grid

# (6) Fit Model
set.seed(100)
gbm_start.time <- Sys.time()
gbm.fit <- gbm(
  formula = is_canceled ~ .,
  distribution = "bernoulli",
  data = train.samples,
  n.trees = 1962,
  interaction.depth = 5,
  shrinkage = 0.1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)
gbm_end.time <- Sys.time()
gbm_time.taken <- gbm_end.time - gbm_start.time #9.708033 mins


# (7) Feature Importance
par(mar = c(5, 13, 1,5 ))
summary(gbm.fit, cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm#
  las = 2)


# (8) Prediction
pred <- predict(gbm.fit, n.trees = gbm.fit$n.trees, test.samples, type = "response")


# (9) F1 Score
GBM_f1 <- F1_Score(y_pred = pred, y_true = test.samples$is_canceled, positive = "1") #0.7204387

# (10) Draw ROC curve
library(pROC)
roc_curve <- roc(test.samples$is_canceled,pred)
names(roc_curve)
x <- 1-roc_curve$specificities
y <- roc_curve$sensitivities
library(ggplot2)
p <- ggplot(data = NULL, mapping = aes(x= x, y = y))
p + geom_line(colour = 'red') +geom_abline(intercept = 0, slope = 1) + 
  annotate('text', x = 0.4, y = 0.5, label =paste('AUC=',round(roc_curve$auc,2))) + 
  labs(x = '1-specificities',y = 'sensitivities', title = 'ROC Curve')


# (11) ConfusionMatrix
library(InformationValue)
pred <- ifelse(pred < 0.5, 0, 1)
confusionMatrix(pred, test.samples$is_canceled)
##   Sensitivity and Specificity
sensitivity(test.samples$is_canceled, pred) # 0.6998366
specificity(test.samples$is_canceled, pred) # 0.9205393

