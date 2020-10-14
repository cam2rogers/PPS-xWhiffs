# Partitioning the data for modeling

bigtrain <- sctot %>% filter(game_year %in% c(2015, 2016, 2017, 2018))
bigtrain$launch_angle <- ifelse(is.na(bigtrain$launch_angle)==T, bigtrain$reg_la, bigtrain$launch_angle)

pitches_LLF <- bigtrain %>% filter(p_throws == "L", stand == "L", pitch_type %in% fast)
pitches_LLF <- data.frame(lapply(pitches_LLF,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_LRF <- bigtrain %>% filter(p_throws == "L", stand == "R", pitch_type %in% fast)
pitches_LRF <- data.frame(lapply(pitches_LRF,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_LLB <- bigtrain %>% filter(p_throws == "L", stand == "L", pitch_type %in% breaking)
pitches_LLB <- data.frame(lapply(pitches_LLB,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_LRB <- bigtrain %>% filter(p_throws == "L", stand == "R", pitch_type %in% breaking)
pitches_LRB <- data.frame(lapply(pitches_LRB,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_LLO <- bigtrain %>% filter(p_throws == "L", stand == "L", pitch_type %in% offspeed)
pitches_LLO <- data.frame(lapply(pitches_LLO,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_LRO <- bigtrain %>% filter(p_throws == "L", stand == "R", pitch_type %in% offspeed)
pitches_LRO <- data.frame(lapply(pitches_LRO,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))

pitches_RLF <- bigtrain %>% filter(p_throws == "R", stand == "L", pitch_type %in% fast)
pitches_RLF <- data.frame(lapply(pitches_RLF,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_RRF <- bigtrain %>% filter(p_throws == "R", stand == "R", pitch_type %in% fast)
pitches_RRF <- data.frame(lapply(pitches_RRF,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_RLB <- bigtrain %>% filter(p_throws == "R", stand == "L", pitch_type %in% breaking)
pitches_RLB <- data.frame(lapply(pitches_RLB,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_RRB <- bigtrain %>% filter(p_throws == "R", stand == "R", pitch_type %in% breaking)
pitches_RRB <- data.frame(lapply(pitches_RRB,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_RLO <- bigtrain %>% filter(p_throws == "R", stand == "L", pitch_type %in% offspeed)
pitches_RLO <- data.frame(lapply(pitches_RLO,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_RRO <- bigtrain %>% filter(p_throws == "R", stand == "R", pitch_type %in% offspeed)
pitches_RRO <- data.frame(lapply(pitches_RRO,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))

#### Swing ####

swing_LLF <- pitches_LLF %>% filter(Swing == 1)
swing_LRF <- pitches_LRF %>% filter(Swing == 1)
swing_LLB <- pitches_LLB %>% filter(Swing == 1)
swing_LRB <- pitches_LRB %>% filter(Swing == 1)
swing_LLO <- pitches_LLO %>% filter(Swing == 1)
swing_LRO <- pitches_LRO %>% filter(Swing == 1)

swing_RLF <- pitches_RLF %>% filter(Swing == 1)
swing_RRF <- pitches_RRF %>% filter(Swing == 1)
swing_RLB <- pitches_RLB %>% filter(Swing == 1)
swing_RRB <- pitches_RRB %>% filter(Swing == 1)
swing_RLO <- pitches_RLO %>% filter(Swing == 1)
swing_RRO <- pitches_RRO %>% filter(Swing == 1)


#### No swing / taken ####

taken_LLF <- pitches_LLF %>% filter(Swing == 0)
taken_LRF <- pitches_LRF %>% filter(Swing == 0)
taken_LLB <- pitches_LLB %>% filter(Swing == 0)
taken_LRB <- pitches_LRB %>% filter(Swing == 0)
taken_LLO <- pitches_LLO %>% filter(Swing == 0)
taken_LRO <- pitches_LRO %>% filter(Swing == 0)

taken_RLF <- pitches_RLF %>% filter(Swing == 0)
taken_RRF <- pitches_RRF %>% filter(Swing == 0)
taken_RLB <- pitches_RLB %>% filter(Swing == 0)
taken_RRB <- pitches_RRB %>% filter(Swing == 0)
taken_RLO <- pitches_RLO %>% filter(Swing == 0)
taken_RRO <- pitches_RRO %>% filter(Swing == 0)

# Model Selection/Validation

control <- trainControl(method="repeatedcv", number=5, repeats=3, verboseIter = T, classProbs=TRUE, summaryFunction = twoClassSummary)
control2 <- trainControl(method="adaptive_cv", number=3, repeats=3, verboseIter = T, adaptive=list(min=3, alpha=0.05, method="BT", complete=T), classProbs = TRUE, summaryFunction = twoClassSummary)

# Example: Testing out different model types for Lefty-on-Lefty fastballs (Swing Model)

set.seed(123)
LLFind <- createDataPartition(pitches_LLF$Swing, p=0.8, list=FALSE, times=1)
LLFtrain <- pitches_LLF[LLFind, ]
LLFvalid <- pitches_LLF[-LLFind, ]
SwLLF <- train(make.names(Swing) ~ I(plate_x^2) + plate_z + release_pos_x + release_pos_y + vx0 + vy0 + ax + ay + az + sz_top + sz_bot + Count, data=LLFtrain, method="glm", trControl=control, metric="ROC")
SwLLF
confusionMatrix(SwLLF)
LLF_pred <- predict(SwLLF, newdata=LLFvalid[, -93], type="prob")
auc(LLFvalid$Swing, LLF_pred$X1) # 0.8125423 ROC

SwLLF2 <- gam(Swing ~ s(plate_x, plate_z) + release_pos_x + release_pos_y + s(vx0, vy0, vz0) + s(ax, ay, az) + s(sz_top) + s(sz_bot) + Count, data=LLFtrain, family="binomial", method="REML")
SwLLF2_pred <- predict(SwLLF2, newdata=LLFvalid[, -93], type="response")
auc(LLFvalid$Swing, SwLLF2_pred)
confusionMatrix(LLFvalid$Swing, as.factor(ifelse(SwLLF2_pred > 0.5, 1, 0))) # 0.8660651 ROC

SwLLF3 <- train(make.names(Swing) ~ I(plate_x^2) + plate_z + release_pos_x + release_pos_y + vx0 + vy0 + ax + ay + az + sz_top + sz_bot + Count, data=LLFtrain, method="glmnet", trControl=control, tuneGrid=expand.grid(alpha=0:1, lambda=seq(0.0001, 1, length=20)), metric="ROC")
SwLLF3
confusionMatrix(SwLLF3)
SwLLF3_pred <- predict(SwLLF3, newdata=LLFvalid[, -93], type="prob")
auc(LLFvalid$Swing, SwLLF3_pred$X1) # 0.8139754 ROC

SwLLF4 <- train(make.names(Swing) ~ plate_x + plate_z + release_pos_x + release_pos_y + vx0 + vy0 + ax + ay + az + sz_top + sz_bot + Count, data=LLFtrain, method="rf", trControl=control2, tuneGrid=expand.grid(mtry=4:9), metric="ROC")
SwLLF4
confusionMatrix(SwLLF4)
SwLLF4_pred <- predict(SwLLF4, newdata=LLFvalid[, -93], type="prob")
auc(LLFvalid$Swing, SwLLF4_pred$X1) # 0.8714514 ROC

customGrid <- expand.grid(n.trees=400:1000, shrinkage=c(0.001, 0.01, 0.05, 0.1, 0.15, 0.25, 0.35), interaction.depth=3:10, n.minobsinnode=10:25)
samp <- sample(1:nrow(customGrid), 10)
SwLLF5 <- train(make.names(Swing) ~ plate_x + plate_z + release_pos_x + release_pos_y + vx0 + vy0 + ax + ay + az + sz_top + sz_bot + Count, data=LLFtrain, method="gbm",  trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
SwLLF5
confusionMatrix(SwLLF5)
SwLLF5_pred <- predict(SwLLF5, newdata=LLFvalid[, -93], type="prob")
auc(LLFvalid$Swing, SwLLF5_pred$X1) # 0.8878951 ROC

# Training Full Models

customGrid <- expand.grid(n.trees=400:1000, shrinkage=c(0.001, 0.01, 0.05, 0.1, 0.15, 0.25, 0.35), interaction.depth=3:10, n.minobsinnode=10:25)

# Swing Models

samp <- sample(1:nrow(customGrid), 10)
FullswLLF <- train(make.names(Swing) ~ plate_x + plate_z + release_pos_x + release_pos_y + vx0 + vy0 + ax + ay + az + sz_top + sz_bot + Count, data=pitches_LLF, method="gbm",  trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullswLLF
confusionMatrix(FullswLLF) 

samp <- sample(1:nrow(customGrid), 10)
FullswLRF <- train(make.names(Swing) ~ plate_x + plate_z + release_pos_x + release_pos_y + vx0 + vy0 + ax + ay + az + sz_top + sz_bot + Count, data=pitches_LRF, method="gbm",  trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullswLRF
confusionMatrix(FullswLRF) 

samp <- sample(1:nrow(customGrid), 10)
FullswLLB <- train(make.names(Swing) ~ plate_x + plate_z + release_pos_x + release_pos_y + vx0 + vy0 + ax + ay + az + sz_top + sz_bot + Count, data=pitches_LLB, method="gbm",  trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullswLLB
confusionMatrix(FullswLLB) 

samp <- sample(1:nrow(customGrid), 10)
FullswLRB <- train(make.names(Swing) ~ plate_x + plate_z + release_pos_x + release_pos_y + vx0 + vy0 + ax + ay + az + sz_top + sz_bot + Count, data=pitches_LRB, method="gbm",  trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullswLRB
confusionMatrix(FullswLRB) 

samp <- sample(1:nrow(customGrid), 10)
FullswLLO <- train(make.names(Swing) ~ plate_x + plate_z + release_pos_x + release_pos_y + vx0 + vy0 + ax + ay + az + sz_top + sz_bot + Count, data=pitches_LLO, method="gbm",  trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullswLLO
confusionMatrix(FullswLLO) 

samp <- sample(1:nrow(customGrid), 10)
FullswLRO <- train(make.names(Swing) ~ plate_x + plate_z + release_pos_x + release_pos_y + vx0 + vy0 + ax + ay + az + sz_top + sz_bot + Count, data=pitches_LRO, method="gbm",  trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullswLRO
confusionMatrix(FullswLRO) 

samp <- sample(1:nrow(customGrid), 10)
FullswRLF <- train(make.names(Swing) ~ plate_x + plate_z + release_pos_x + release_pos_y + vx0 + vy0 + ax + ay + az + sz_top + sz_bot + Count, data=pitches_RLF, method="gbm",  trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullswRLF
confusionMatrix(FullswRLF)

samp <- sample(1:nrow(customGrid), 10)
FullswRRF <- train(make.names(Swing) ~ plate_x + plate_z + release_pos_x + release_pos_y + vx0 + vy0 + ax + ay + az + sz_top + sz_bot + Count, data=pitches_RRF, method="gbm",  trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullswRRF
confusionMatrix(FullswRRF) 

samp <- sample(1:nrow(customGrid), 10)
FullswRLB <- train(make.names(Swing) ~ plate_x + plate_z + release_pos_x + release_pos_y + vx0 + vy0 + ax + ay + az + sz_top + sz_bot + Count, data=pitches_RLB, method="gbm",  trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullswRLB
confusionMatrix(FullswRLB) 

samp <- sample(1:nrow(customGrid), 10)
FullswRRB <- train(make.names(Swing) ~ plate_x + plate_z + release_pos_x + release_pos_y + vx0 + vy0 + ax + ay + az + sz_top + sz_bot + Count, data=pitches_RRB, method="gbm",  trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullswRRB
confusionMatrix(FullswRRB) 

samp <- sample(1:nrow(customGrid), 10)
FullswRLO <- train(make.names(Swing) ~ plate_x + plate_z + release_pos_x + release_pos_y + vx0 + vy0 + ax + ay + az + sz_top + sz_bot + Count, data=pitches_RLO, method="gbm",  trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullswRLO
confusionMatrix(FullswRLO) 

samp <- sample(1:nrow(customGrid), 10)
FullswRRO <- train(make.names(Swing) ~ plate_x + plate_z + release_pos_x + release_pos_y + vx0 + vy0 + ax + ay + az + sz_top + sz_bot + Count, data=pitches_RRO, method="gbm",  trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullswRRO
confusionMatrix(FullswRRO)

# Whiff Models

samp <- sample(1:nrow(customGrid), 10)
FullwLLF <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + ax + az + reg_whiff, data=swing_LLF, method="gbm", trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullwLLF
confusionMatrix(FullwLLF) 

samp <- sample(1:nrow(customGrid), 7)
FullwLRF <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + vx0 + vy0 + vz0 + sz_top + sz_bot + reg_whiff, data=swing_LRF, method="gbm", trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullwLRF
confusionMatrix(FullwLRF) 

samp <- sample(1:nrow(customGrid), 7)
FullwLLB <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + vx0 + vy0 + vz0 + sz_top + sz_bot + reg_whiff, data=swing_LLB, method="gbm", trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullwLLB
confusionMatrix(FullwLLB) 

samp <- sample(1:nrow(customGrid), 10)
FullwLRB <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + vx0 + vy0 + vz0 + sz_top + sz_bot + reg_whiff, data=swing_LRB, method="gbm", trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullwLRB
confusionMatrix(FullwLRB) 

samp <- sample(1:nrow(customGrid), 10)
FullwLLO <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + vx0 + vy0 + vz0 + sz_top + sz_bot + reg_whiff, data=swing_LLO, method="gbm", trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullwLLO
confusionMatrix(FullwLLO)

samp <- sample(1:nrow(customGrid), 7)
FullwLRO <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + vx0 + vy0 + vz0 + sz_top + sz_bot + reg_whiff, data=swing_LRO, method="gbm", trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullwLRO
confusionMatrix(FullwLRO)

samp <- sample(1:nrow(customGrid), 7)
FullwRLF <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + vx0 + vy0 + vz0 + sz_top + sz_bot + reg_whiff, data=swing_RLF, method="gbm", trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullwRLF
confusionMatrix(FullwRLF)

samp <- sample(1:nrow(customGrid), 7)
FullwRRF <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + vx0 + vy0 + vz0 + sz_top + sz_bot + reg_whiff, data=swing_RRF, method="gbm", trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullwRRF
confusionMatrix(FullwRRF) 

samp <- sample(1:nrow(customGrid), 10)
FullwRLB <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + vx0 + vy0 + vz0 + sz_top + sz_bot + reg_whiff, data=swing_RLB, method="gbm", trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullwRLB
confusionMatrix(FullwRLB) 

samp <- sample(1:nrow(customGrid), 10)
FullwRRB <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + vx0 + vy0 + vz0 + sz_top + sz_bot + reg_whiff, data=swing_RRB, method="gbm", trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullwRRB
confusionMatrix(FullwRRB) 

samp <- sample(1:nrow(customGrid), 10)
FullwRLO <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + vx0 + vy0 + vz0 + sz_top + sz_bot + reg_whiff, data=swing_RLO, method="gbm", trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullwRLO
confusionMatrix(FullwRLO) 

samp <- sample(1:nrow(customGrid), 10)
FullwRRO <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + vx0 + vy0 + vz0 + sz_top + sz_bot + reg_whiff, data=swing_RRO, method="gbm", trControl=control2, tuneGrid=customGrid[samp, ], metric="ROC")
FullwRRO
confusionMatrix(FullwRRO) 

# Called Strike Models

library(mgcv) 

FullcsLLF <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_LLF, family="binomial", method="REML")

FullcsLRF <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_LRF, family="binomial", method="REML")

FullcsLLB <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_LLB, family="binomial", method="REML")

FullcsLRB <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_LRB, family="binomial", method="REML")

FullcsLLO <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_LLO, family="binomial", method="REML")

FullcsLRO <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_LRO, family="binomial", method="REML")

FullcsRLF <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_RLF, family="binomial", method="REML")

FullcsRRF <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_RRF, family="binomial", method="REML")

FullcsRLB <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_RLB, family="binomial", method="REML")

FullcsRRB <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_RRB, family="binomial", method="REML")

FullcsRLO <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_RLO, family="binomial", method="REML")

FullcsRRO <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_RRO, family="binomial", method="REML")


# Partitioning Test Data

bigtest <- sctot %>% filter(game_year == 2019)
bigtest$launch_angle <- ifelse(is.na(bigtest$launch_angle)==T, bigtest$reg_la, bigtest$launch_angle)

test_LLF <- bigtest %>% filter(p_throws == "L", stand == "L", pitch_type %in% fb)
test_LLF <- data.frame(lapply(test_LLF,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_LRF <- bigtest %>% filter(p_throws == "L", stand == "R", pitch_type %in% fb)
test_LRF <- data.frame(lapply(test_LRF,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_LLSC <- bigtest %>% filter(p_throws == "L", stand == "L", pitch_type %in% slct)
test_LLSC <- data.frame(lapply(test_LLSC,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_LRSC <- bigtest %>% filter(p_throws == "L", stand == "R", pitch_type %in% slct)
test_LRSC <- data.frame(lapply(test_LRSC,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_LLC <- bigtest %>% filter(p_throws == "L", stand == "L", pitch_type %in% cu)
test_LLC <- data.frame(lapply(test_LLC,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_LRC <- bigtest %>% filter(p_throws == "L", stand == "R", pitch_type %in% cu)
test_LRC <- data.frame(lapply(test_LRC,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_LLO <- bigtest %>% filter(p_throws == "L", stand == "L", pitch_type %in% ch)
test_LLO <- data.frame(lapply(test_LLO,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_LRO <- bigtest %>% filter(p_throws == "L", stand == "R", pitch_type %in% ch)
test_LRO <- data.frame(lapply(test_LRO,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))


test_RLF <- bigtest %>% filter(p_throws == "R", stand == "L", pitch_type %in% fb)
test_RLF <- data.frame(lapply(test_RLF,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_RRF <- bigtest %>% filter(p_throws == "R", stand == "R", pitch_type %in% fb)
test_RRF <- data.frame(lapply(test_RRF,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_RRF <- test_RRF %>% filter(Count != "4-1")
test_RLSC <- bigtest %>% filter(p_throws == "R", stand == "L", pitch_type %in% slct)
test_RLSC <- data.frame(lapply(test_RLSC,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_RRSC <- bigtest %>% filter(p_throws == "R", stand == "R", pitch_type %in% slct)
test_RRSC <- data.frame(lapply(test_RRSC,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_RLC <- bigtest %>% filter(p_throws == "R", stand == "L", pitch_type %in% cu)
test_RLC <- data.frame(lapply(test_RLC,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_RRC <- bigtest %>% filter(p_throws == "R", stand == "R", pitch_type %in% cu)
test_RRC <- data.frame(lapply(test_RRC,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_RLO <- bigtest %>% filter(p_throws == "R", stand == "L", pitch_type %in% ch)
test_RLO <- data.frame(lapply(test_RLO,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_RRO <- bigtest %>% filter(p_throws == "R", stand == "R", pitch_type %in% ch)
test_RRO <- data.frame(lapply(test_RRO,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))

# Swing Probability

swLLFpred <- predict(FullswLLF, test_LLF[, -91], type="prob")
auc(test_LLF$Swing, swLLFpred$X1) 

swLRFpred <- predict(FullswLRF, test_LRF[, -91], type="prob")
auc(test_LRF$Swing, swLRFpred$X1) 

swLLSCpred <- predict(FullswLLSC, test_LLSC[, -91], type="prob")
auc(test_LLSC$Swing, swLLSCpred$X1)

swLRSCpred <- predict(FullswLRSC, test_LRSC[, -91], type="prob")
auc(test_LRSC$Swing, swLRSCpred$X1) 

swLLCpred <- predict(FullswLLC, test_LLC[, -91], type="prob")
auc(test_LLC$Swing, swLLCpred$X1)

swLRCpred <- predict(FullswLRC, test_LRC[, -91], type="prob")
auc(test_LRC$Swing, swLRCpred$X1) 

swLLOpred <- predict(FullswLLO, test_LLO[, -91], type="prob")
auc(test_LLO$Swing, swLLOpred$X1) 

swLROpred <- predict(FullswLRO, test_LRO[, -91], type="prob")
auc(test_LRO$Swing, swLROpred$X1) 

swRLFpred <- predict(FullswRLF, test_RLF[, -91], type="prob")
auc(test_RLF$Swing, swRLFpred$X1) 

swRRFpred <- predict(FullswRRF, test_RRF[, -91], type="prob")
auc(test_RRF$Swing, swRRFpred$X1) 

swRLSCpred <- predict(FullswRLSC, test_RLSC[, -91], type="prob")
auc(test_RLSC$Swing, swRLSCpred$X1) 

swRRSCpred <- predict(FullswRRSC, test_RRSC[, -91], type="prob")
auc(test_RRSC$Swing, swRRSCpred$X1) 

swRLCpred <- predict(FullswRLC, test_RLC[, -91], type="prob")
auc(test_RLC$Swing, swRLCpred$X1) 

swRRCpred <- predict(FullswRRC, test_RRC[, -91], type="prob")
auc(test_RRC$Swing, swRRCpred$X1) 

swRLOpred <- predict(FullswRLO, test_RLO[, -91], type="prob")
auc(test_RLO$Swing, swRLOpred$X1) 

swRROpred <- predict(FullswRRO, test_RRO[, -91], type="prob")
auc(test_RRO$Swing, swRROpred$X1)

# Assigning swing probabilities to test datasets

test_LLF$swingprob <- swLLFpred$X1

test_LRF$swingprob <- swLRFpred$X1

test_LLSC$swingprob <- swLLSCpred$X1

test_LRSC$swingprob <- swLRSCpred$X1

test_LLC$swingprob <- swLLCpred$X1

test_LRC$swingprob <- swLRCpred$X1

test_LLO$swingprob <- swLLOpred$X1

test_LRO$swingprob <- swLROpred$X1

test_RLF$swingprob <- swRLFpred$X1

test_RRF$swingprob <- swRRFpred$X1

test_RLSC$swingprob <- swRLSCpred$X1

test_RRSC$swingprob <- swRRSCpred$X1

test_RLC$swingprob <- swRLCpred$X1

test_RRC$swingprob <- swRRCpred$X1

test_RLO$swingprob <- swRLOpred$X1

test_RRO$swingprob <- swRROpred$X1


## Further Partitioning

tswing_LLF <- test_LLF %>% filter(Swing == 1)
tswing_LRF <- test_LRF %>% filter(Swing == 1)
tswing_LLSC <- test_LLSC %>% filter(Swing == 1)
tswing_LRSC <- test_LRSC %>% filter(Swing == 1)
tswing_LLC <- test_LLC %>% filter(Swing == 1)
tswing_LRC <- test_LRC %>% filter(Swing == 1)
tswing_LLO <- test_LLO %>% filter(Swing == 1)
tswing_LRO <- test_LRO %>% filter(Swing == 1)

tswing_RLF <- test_RLF %>% filter(Swing == 1)
tswing_RRF <- test_RRF %>% filter(Swing == 1)
tswing_RLSC <- test_RLSC %>% filter(Swing == 1)
tswing_RRSC <- test_RRSC %>% filter(Swing == 1)
tswing_RLC <- test_RLC %>% filter(Swing == 1)
tswing_RRC <- test_RRC %>% filter(Swing == 1)
tswing_RLO <- test_RLO %>% filter(Swing == 1)
tswing_RRO <- test_RRO %>% filter(Swing == 1)

# Called Ball/Strike Probability

csLLFpred <- predict(FullcsLLF, ttaken_LLF[, -116], type="response")
auc(ttaken_LLF$calledStrike, csLLFpred)

csLRFpred <- predict(FullcsLRF, ttaken_LRF[, -116], type="response")
auc(ttaken_LRF$calledStrike, csLRFpred) 

csLLSCpred <- predict(FullcsLLSC, ttaken_LLSC[, -116], type="response")
auc(ttaken_LLSC$calledStrike, csLLSCpred) 

csLRSCpred <- predict(FullcsLRSC, ttaken_LRSC[, -116], type="response")
auc(ttaken_LRSC$calledStrike, csLRSCpred) 

csLLCpred <- predict(FullcsLLC, ttaken_LLC[, -116], type="response")
auc(ttaken_LLC$calledStrike, csLLCpred) 

csLRCpred <- predict(FullcsLRC, ttaken_LRC[, -116], type="response")
auc(ttaken_LRC$calledStrike, csLRCpred) 

csLLOpred <- predict(FullcsLLO, ttaken_LLO[, -116], type="response")
auc(ttaken_LLO$calledStrike, csLLOpred) 

csLROpred <- predict(FullcsLRO, ttaken_LRO[, -116], type="response")
auc(ttaken_LRO$calledStrike, csLROpred) 

csRLFpred <- predict(FullcsRLF, ttaken_RLF[, -116], type="response")
auc(ttaken_RLF$calledStrike, csRLFpred) 

csRRFpred <- predict(FullcsRRF, ttaken_RRF[, -116], type="response")
auc(ttaken_RRF$calledStrike, csRRFpred) 

csRLSCpred <- predict(FullcsRLSC, ttaken_RLSC[, -116], type="response")
auc(ttaken_RLSC$calledStrike, csRLSCpred) 

csRRSCpred <- predict(FullcsRRSC, ttaken_RRSC[, -116], type="response")
auc(ttaken_RRSC$calledStrike, csRRSCpred) 

csRLCpred <- predict(FullcsRLC, ttaken_RLC[, -116], type="response")
auc(ttaken_RLC$calledStrike, csRLCpred)

csRRCpred <- predict(FullcsRRC, ttaken_RRC[, -116], type="response")
auc(ttaken_RRC$calledStrike, csRRCpred) 

csRLOpred <- predict(FullcsRLO, ttaken_RLO[, -116], type="response")
auc(ttaken_RLO$calledStrike, csRLOpred)

csRROpred <- predict(FullcsRRO, ttaken_RRO[, -116], type="response")
auc(ttaken_RRO$calledStrike, csRROpred) 

# Whiff probability

whiffLLFpred <- predict(FullwLLF, tswing_LLF[, -92], type="prob")
auc(tswing_LLF$Miss, whiffLLFpred$X1) 

whiffLRFpred <- predict(FullwLRF, tswing_LRF[, -92], type="prob")
auc(tswing_LRF$Miss, whiffLRFpred$X1) 

whiffLLSCpred <- predict(FullwLLSC, tswing_LLSC[, -92], type="prob")
auc(tswing_LLSC$Miss, whiffLLSCpred$X1) 

whiffLRSCpred <- predict(FullwLRSC, tswing_LRSC[, -92], type="prob")
auc(tswing_LRSC$Miss, whiffLRSCpred$X1)

whiffLLCpred <- predict(FullwLLC, tswing_LLC[, -92], type="prob")
auc(tswing_LLC$Miss, whiffLLCpred$X1)

whiffLRCpred <- predict(FullwLRC, tswing_LRC[, -92], type="prob")
auc(tswing_LRC$Miss, whiffLRCpred$X1) 

whiffLLOpred <- predict(FullwLLO, tswing_LLO[, -92], type="prob")
auc(tswing_LLO$Miss, whiffLLOpred$X1) 

whiffLROpred <- predict(FullwLRO, tswing_LRO[, -92], type="prob")
auc(tswing_LRO$Miss, whiffLROpred$X1) 

whiffRLFpred <- predict(FullwRLF, tswing_RLF[, -92], type="prob")
auc(tswing_RLF$Miss, whiffRLFpred$X1)

whiffRRFpred <- predict(FullwRRF, tswing_RRF[, -92], type="prob")
auc(tswing_RRF$Miss, whiffRRFpred$X1)

whiffRLSCpred <- predict(FullwRLSC, tswing_RLSC[, -92], type="prob")
auc(tswing_RLSC$Miss, whiffRLSCpred$X1) 

whiffRRSCpred <- predict(FullwRRSC, tswing_RRSC[, -92], type="prob")
auc(tswing_RRSC$Miss, whiffRRSCpred$X1) 

whiffRLCpred <- predict(FullwRLC, tswing_RLC[, -92], type="prob")
auc(tswing_RLC$Miss, whiffRLCpred$X1) 

whiffRRCpred <- predict(FullwRRC, tswing_RRC[, -92], type="prob")
auc(tswing_RRC$Miss, whiffRRCpred$X1) 

whiffRLOpred <- predict(FullwRLO, tswing_RLO[, -92], type="prob")
auc(tswing_RLO$Miss, whiffRLOpred$X1) 

whiffRROpred <- predict(FullwRRO, tswing_RRO[, -92], type="prob")
auc(tswing_RRO$Miss, whiffRROpred$X1) 

# Assigning CS, Whiff probabilities

ttaken_LLF$csprob <- csLLFpred
tswing_LLF$whiffprob <- whiffLLFpred$X1

ttaken_LRF$csprob <- csLRFpred
tswing_LRF$whiffprob <- whiffLRFpred$X1

ttaken_LLSC$csprob <- csLLSCpred
tswing_LLSC$whiffprob <- whiffLLSCpred$X1

ttaken_LRSC$csprob <- csLRSCpred
tswing_LRSC$whiffprob <- whiffLRSCpred$X1

ttaken_LLC$csprob <- csLLCpred
tswing_LLC$whiffprob <- whiffLLCpred$X1

ttaken_LRC$csprob <- csLRCpred
tswing_LRC$whiffprob <- whiffLRCpred$X1

ttaken_LLO$csprob <- csLLOpred
tswing_LLO$whiffprob <- whiffLLOpred$X1

ttaken_LRO$csprob <- csLROpred
tswing_LRO$whiffprob <- whiffLROpred$X1

ttaken_RLF$csprob <- csRLFpred
tswing_RLF$whiffprob <- whiffRLFpred$X1

ttaken_RRF$csprob <- csRRFpred
tswing_RRF$whiffprob <- whiffRRFpred$X1

ttaken_RLSC$csprob <- csRLSCpred
tswing_RLSC$whiffprob <- whiffRLSCpred$X1

ttaken_RRSC$csprob <- csRRSCpred
tswing_RRSC$whiffprob <- whiffRRSCpred$X1

ttaken_RLC$csprob <- csRLCpred
tswing_RLC$whiffprob <- whiffRLCpred$X1

ttaken_RRC$csprob <- csRRCpred
tswing_RRC$whiffprob <- whiffRRCpred$X1

ttaken_RLO$csprob <- csRLOpred
tswing_RLO$whiffprob <- whiffRLOpred$X1

ttaken_RRO$csprob <- csRROpred
tswing_RRO$whiffprob <- whiffRROpred$X1

# Assigning rest of probabilites to 0

ttaken_LLF$whiffprob <- 0
tswing_LLF$csprob <- 0

ttaken_LRF$whiffprob <- 0
tswing_LRF$csprob <- 0

ttaken_LLSC$whiffprob <- 0
tswing_LLSC$csprob <- 0

ttaken_LRSC$whiffprob <- 0
tswing_LRSC$csprob <- 0

ttaken_LLC$whiffprob <- 0
tswing_LLC$csprob <- 0

ttaken_LRC$whiffprob <- 0
tswing_LRC$csprob <- 0

ttaken_LLO$whiffprob <- 0
tswing_LLO$csprob <- 0

ttaken_LRO$whiffprob <- 0
tswing_LRO$csprob <- 0

ttaken_RLF$whiffprob <- 0
tswing_RLF$csprob <- 0

ttaken_RRF$whiffprob <- 0
tswing_RRF$csprob <- 0

ttaken_RLSC$whiffprob <- 0
tswing_RLSC$csprob <- 0

ttaken_RRSC$whiffprob <- 0
tswing_RRSC$csprob <- 0

ttaken_RLC$whiffprob <- 0
tswing_RLC$csprob <- 0

ttaken_RRC$whiffprob <- 0
tswing_RRC$csprob <- 0

ttaken_RLO$whiffprob <- 0
tswing_RLO$csprob <- 0

ttaken_RRO$whiffprob <- 0
tswing_RRO$csprob <- 0

# Joining Data

final_pitches <- rbind(ttaken_LLF, ttaken_LRF, ttaken_LLSC, ttaken_LRSC, ttaken_LLC, ttaken_LRC, ttaken_LLO, ttaken_LRO, ttaken_RLF, ttaken_RRF, ttaken_RLSC, ttaken_RRSC, ttaken_RLC, ttaken_RRC, ttaken_RLO, ttaken_RRO, tswing_LLF, tswing_LRF, tswing_LLSC, tswing_LRSC, tswing_LLC, tswing_LRC, tswing_LLO, tswing_LRO, tswing_RLF, tswing_RRF, tswing_RLSC, tswing_RRSC, tswing_RLC, tswing_RRC, tswing_RLO, tswing_RRO)



#### Reapeated same process as above to get 2018 data ####

bigtrain <- sctot %>% filter(game_year %in% c(2015, 2016, 2017, 2019))
bigtrain$launch_angle <- ifelse(is.na(bigtrain$launch_angle)==T, bigtrain$reg_la, bigtrain$launch_angle)

pitches_LLF <- bigtrain %>% filter(p_throws == "L", stand == "L", pitch_type %in% fast)
pitches_LLF <- data.frame(lapply(pitches_LLF,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_LRF <- bigtrain %>% filter(p_throws == "L", stand == "R", pitch_type %in% fast)
pitches_LRF <- data.frame(lapply(pitches_LRF,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_LLB <- bigtrain %>% filter(p_throws == "L", stand == "L", pitch_type %in% breaking)
pitches_LLB <- data.frame(lapply(pitches_LLB,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_LRB <- bigtrain %>% filter(p_throws == "L", stand == "R", pitch_type %in% breaking)
pitches_LRB <- data.frame(lapply(pitches_LRB,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_LLO <- bigtrain %>% filter(p_throws == "L", stand == "L", pitch_type %in% offspeed)
pitches_LLO <- data.frame(lapply(pitches_LLO,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_LRO <- bigtrain %>% filter(p_throws == "L", stand == "R", pitch_type %in% offspeed)
pitches_LRO <- data.frame(lapply(pitches_LRO,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))

pitches_RLF <- bigtrain %>% filter(p_throws == "R", stand == "L", pitch_type %in% fast)
pitches_RLF <- data.frame(lapply(pitches_RLF,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_RRF <- bigtrain %>% filter(p_throws == "R", stand == "R", pitch_type %in% fast)
pitches_RRF <- data.frame(lapply(pitches_RRF,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_RLB <- bigtrain %>% filter(p_throws == "R", stand == "L", pitch_type %in% breaking)
pitches_RLB <- data.frame(lapply(pitches_RLB,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_RRB <- bigtrain %>% filter(p_throws == "R", stand == "R", pitch_type %in% breaking)
pitches_RRB <- data.frame(lapply(pitches_RRB,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_RLO <- bigtrain %>% filter(p_throws == "R", stand == "L", pitch_type %in% offspeed)
pitches_RLO <- data.frame(lapply(pitches_RLO,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
pitches_RRO <- bigtrain %>% filter(p_throws == "R", stand == "R", pitch_type %in% offspeed)
pitches_RRO <- data.frame(lapply(pitches_RRO,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))

#### Swing ####

swing_LLF <- pitches_LLF %>% filter(Swing == 1)
swing_LRF <- pitches_LRF %>% filter(Swing == 1)
swing_LLB <- pitches_LLB %>% filter(Swing == 1)
swing_LRB <- pitches_LRB %>% filter(Swing == 1)
swing_LLO <- pitches_LLO %>% filter(Swing == 1)
swing_LRO <- pitches_LRO %>% filter(Swing == 1)

swing_RLF <- pitches_RLF %>% filter(Swing == 1)
swing_RRF <- pitches_RRF %>% filter(Swing == 1)
swing_RLB <- pitches_RLB %>% filter(Swing == 1)
swing_RRB <- pitches_RRB %>% filter(Swing == 1)
swing_RLO <- pitches_RLO %>% filter(Swing == 1)
swing_RRO <- pitches_RRO %>% filter(Swing == 1)


#### No swing / taken ####

taken_LLF <- pitches_LLF %>% filter(Swing == 0)
taken_LRF <- pitches_LRF %>% filter(Swing == 0)
taken_LLB <- pitches_LLB %>% filter(Swing == 0)
taken_LRB <- pitches_LRB %>% filter(Swing == 0)
taken_LLO <- pitches_LLO %>% filter(Swing == 0)
taken_LRO <- pitches_LRO %>% filter(Swing == 0)

taken_RLF <- pitches_RLF %>% filter(Swing == 0)
taken_RRF <- pitches_RRF %>% filter(Swing == 0)
taken_RLB <- pitches_RLB %>% filter(Swing == 0)
taken_RRB <- pitches_RRB %>% filter(Swing == 0)
taken_RLO <- pitches_RLO %>% filter(Swing == 0)
taken_RRO <- pitches_RRO %>% filter(Swing == 0)

# Model Training with optimal hyperparameters from the first iteration

# Swings

customGrid <- expand.grid(n.trees=913, shrinkage=0.05, interaction.depth=6, n.minobsinnode=33)
FullswLLF <- train(make.names(Swing) ~ plate_x + plate_z + pfx_x + pfx_z + release_speed + release_spin_rate + release_pos_x + release_pos_y + release_pos_z + Count, data=pitches_LLF, method="gbm",  trControl=control, tuneGrid=customGrid, metric="ROC")
FullswLLF
confusionMatrix(FullswLLF)

customGrid <- expand.grid(n.trees=681, shrinkage=0.15, interaction.depth=4, n.minobsinnode=26)
FullswLRF <- train(make.names(Swing) ~ plate_x + plate_z + pfx_x + pfx_z + release_speed + release_spin_rate + release_pos_x + release_pos_y + release_pos_z + Count, data=pitches_LRF, method="gbm",  trControl=control, tuneGrid=customGrid, metric="ROC")
FullswLRF
confusionMatrix(FullswLRF)

customGrid <- expand.grid(n.trees=918, shrinkage=0.01, interaction.depth=9, n.minobsinnode=42)
FullswLLSC <- train(make.names(Swing) ~ plate_x + plate_z + pfx_x + pfx_z + release_speed + release_spin_rate + release_pos_x + release_pos_y + release_pos_z + Count, data=pitches_LLSC, method="gbm",  trControl=control, tuneGrid=customGrid, metric="ROC")
FullswLLSC
confusionMatrix(FullswLLSC)

customGrid <- expand.grid(n.trees=918, shrinkage=0.1, interaction.depth=3, n.minobsinnode=49)
FullswLRSC <- train(make.names(Swing) ~ plate_x + plate_z + pfx_x + pfx_z + release_speed + release_spin_rate + release_pos_x + release_pos_y + release_pos_z + Count, data=pitches_LRSC, method="gbm",  trControl=control, tuneGrid=customGrid, metric="ROC")
FullswLRSC
confusionMatrix(FullswLRSC)

customGrid <- expand.grid(n.trees=736, shrinkage=0.01, interaction.depth=10, n.minobsinnode=19)
FullswLLC <- train(make.names(Swing) ~ plate_x + plate_z + pfx_x + pfx_z + release_speed + release_spin_rate + release_pos_x + release_pos_y + release_pos_z + Count, data=pitches_LLC, method="gbm",  trControl=control, tuneGrid=customGrid, metric="ROC")
FullswLLC
confusionMatrix(FullswLLC)

customGrid <- expand.grid(n.trees=778, shrinkage=0.05, interaction.depth=10, n.minobsinnode=40)
FullswLRC <- train(make.names(Swing) ~ plate_x + plate_z + pfx_x + pfx_z + release_speed + release_spin_rate + release_pos_x + release_pos_y + release_pos_z + Count, data=pitches_LRC, method="gbm",  trControl=control, tuneGrid=customGrid, metric="ROC")
FullswLRC
confusionMatrix(FullswLRC)

customGrid <- expand.grid(n.trees=863, shrinkage=0.1, interaction.depth=4, n.minobsinnode=35)
FullswLLO <- train(make.names(Swing) ~ plate_x + plate_z + pfx_x + pfx_z + diff_fb + release_spin_rate + release_pos_x + release_pos_y + release_pos_z + Count, data=pitches_LLO, method="gbm",  trControl=control, tuneGrid=customGrid, metric="ROC")
FullswLLO
confusionMatrix(FullswLLO)

customGrid <- expand.grid(n.trees=901, shrinkage=0.1, interaction.depth=5, n.minobsinnode=47)
FullswLRO <- train(make.names(Swing) ~ plate_x + plate_z + pfx_x + pfx_z + diff_fb + release_spin_rate + release_pos_x + release_pos_y + release_pos_z + Count, data=pitches_LRO, method="gbm",  trControl=control, tuneGrid=customGrid, metric="ROC")
FullswLRO
confusionMatrix(FullswLRO)

customGrid <- expand.grid(n.trees=556, shrinkage=0.1, interaction.depth=6, n.minobsinnode=47)
FullswRLF <- train(make.names(Swing) ~ plate_x + plate_z + pfx_x + pfx_z + release_speed + release_spin_rate + release_pos_x + release_pos_y + release_pos_z + Count, data=pitches_RLF, method="gbm",  trControl=control, tuneGrid=customGrid, metric="ROC")
FullswRLF
confusionMatrix(FullswRLF)

customGrid <- expand.grid(n.trees=693, shrinkage=0.1, interaction.depth=9, n.minobsinnode=40)
FullswRRF <- train(make.names(Swing) ~ plate_x + plate_z + pfx_x + pfx_z + release_speed + release_spin_rate + release_pos_x + release_pos_y + release_pos_z + Count, data=pitches_RRF, method="gbm",  trControl=control, tuneGrid=customGrid, metric="ROC")
FullswRRF
confusionMatrix(FullswRRF)

customGrid <- expand.grid(n.trees=481, shrinkage=0.1, interaction.depth=7, n.minobsinnode=36)
FullswRLSC <- train(make.names(Swing) ~ plate_x + plate_z + pfx_x + pfx_z + release_speed + release_spin_rate + release_pos_x + release_pos_y + release_pos_z + Count, data=pitches_RLSC, method="gbm",  trControl=control, tuneGrid=customGrid, metric="ROC")
FullswRLSC
confusionMatrix(FullswRLSC)

customGrid <- expand.grid(n.trees=499, shrinkage=0.1, interaction.depth=7, n.minobsinnode=13)
FullswRRSC <- train(make.names(Swing) ~ plate_x + plate_z + pfx_x + pfx_z + release_speed + release_spin_rate + release_pos_x + release_pos_y + release_pos_z + Count, data=pitches_RRSC, method="gbm",  trControl=control, tuneGrid=customGrid, metric="ROC")
FullswRRSC
confusionMatrix(FullswRRSC)

customGrid <- expand.grid(n.trees=839, shrinkage=0.05, interaction.depth=8, n.minobsinnode=14)
FullswRLC <- train(make.names(Swing) ~ plate_x + plate_z + pfx_x + pfx_z + release_speed + release_spin_rate + release_pos_x + release_pos_y + release_pos_z + Count, data=pitches_RLC, method="gbm",  trControl=control, tuneGrid=customGrid, metric="ROC")
FullswRLC
confusionMatrix(FullswRLC)

customGrid <- expand.grid(n.trees=400, shrinkage=0.1, interaction.depth=10, n.minobsinnode=12)
FullswRRC <- train(make.names(Swing) ~ plate_x + plate_z + pfx_x + pfx_z + release_speed + release_spin_rate + release_pos_x + release_pos_y + release_pos_z + Count, data=pitches_RRC, method="gbm",  trControl=control, tuneGrid=customGrid, metric="ROC")
FullswRRC
confusionMatrix(FullswRRC)

customGrid <- expand.grid(n.trees=553, shrinkage=0.05, interaction.depth=10, n.minobsinnode=40)
FullswRLO <- train(make.names(Swing) ~ plate_x + plate_z + pfx_x + pfx_z + diff_fb + release_spin_rate + release_pos_x + release_pos_y + release_pos_z + Count, data=pitches_RLO, method="gbm",  trControl=control, tuneGrid=customGrid, metric="ROC")
FullswRLO
confusionMatrix(FullswRLO)

customGrid <- expand.grid(n.trees=880, shrinkage=0.05, interaction.depth=8, n.minobsinnode=42)
FullswRRO <- train(make.names(Swing) ~ plate_x + plate_z + pfx_x + pfx_z + diff_fb + release_spin_rate + release_pos_x + release_pos_y + release_pos_z + Count, data=pitches_RRO, method="gbm",  trControl=control, tuneGrid=customGrid, metric="ROC")
FullswRRO
confusionMatrix(FullswRRO)

# Whiffs

customGrid <- expand.grid(n.trees=448, shrinkage=0.05, interaction.depth=5, n.minobsinnode=28)
FullwLLF <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + reg_whiff, data=swing_LLF, method="gbm", trControl=control, tuneGrid=customGrid, metric="ROC")
FullwLLF
confusionMatrix(FullwLLF)

customGrid <- expand.grid(n.trees=742, shrinkage=0.05, interaction.depth=3, n.minobsinnode=13)
FullwLRF <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + reg_whiff, data=swing_LRF, method="gbm", trControl=control, tuneGrid=customGrid, metric="ROC")
FullwLRF
confusionMatrix(FullwLRF)

customGrid <- expand.grid(n.trees=882, shrinkage=0.01, interaction.depth=9, n.minobsinnode=15)
FullwLLSC <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + reg_whiff, data=swing_LLSC, method="gbm", trControl=control, tuneGrid=customGrid, metric="ROC")
FullwLLSC
confusionMatrix(FullwLLSC)

customGrid <- expand.grid(n.trees=646, shrinkage=0.05, interaction.depth=6, n.minobsinnode=38)
FullwLRSC <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + reg_whiff, data=swing_LRSC, method="gbm", trControl=control, tuneGrid=customGrid, metric="ROC")
FullwLRSC
confusionMatrix(FullwLRSC)

customGrid <- expand.grid(n.trees=675, shrinkage=0.05, interaction.depth=5, n.minobsinnode=42)
FullwLLC <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + reg_whiff, data=swing_LLC, method="gbm", trControl=control, tuneGrid=customGrid, metric="ROC")
FullwLLC
confusionMatrix(FullwLLC)

customGrid <- expand.grid(n.trees=609, shrinkage=0.01, interaction.depth=7, n.minobsinnode=33)
FullwLRC <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + reg_whiff, data=swing_LRC, method="gbm", trControl=control, tuneGrid=customGrid, metric="ROC")
FullwLRC
confusionMatrix(FullwLRC)

customGrid <- expand.grid(n.trees=767, shrinkage=0.05, interaction.depth=6, n.minobsinnode=47)
FullwLLO <- train(make.names(Miss) ~ plate_x + plate_z + diff_fb + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + reg_whiff, data=swing_LLO, method="gbm", trControl=control, tuneGrid=customGrid, metric="ROC")
FullwLLO
confusionMatrix(FullwLLO)

customGrid <- expand.grid(n.trees=951, shrinkage=0.01, interaction.depth=7, n.minobsinnode=45)
FullwLRO <- train(make.names(Miss) ~ plate_x + plate_z + diff_fb + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + reg_whiff, data=swing_LRO, method="gbm", trControl=control, tuneGrid=customGrid, metric="ROC")
FullwLRO
confusionMatrix(FullwLRO)

customGrid <- expand.grid(n.trees=996, shrinkage=0.1, interaction.depth=6, n.minobsinnode=47)
FullwRLF <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + reg_whiff, data=swing_RLF, method="gbm", trControl=control, tuneGrid=customGrid, metric="ROC")
FullwRLF
confusionMatrix(FullwRLF)

customGrid <- expand.grid(n.trees=542, shrinkage=0.1, interaction.depth=9, n.minobsinnode=43)
FullwRRF <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + reg_whiff, data=swing_RRF, method="gbm", trControl=control, tuneGrid=customGrid, metric="ROC")
FullwRRF
confusionMatrix(FullwRRF)

customGrid <- expand.grid(n.trees=741, shrinkage=0.01, interaction.depth=9, n.minobsinnode=18)
FullwRLSC <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + reg_whiff, data=swing_RLSC, method="gbm", trControl=control, tuneGrid=customGrid, metric="ROC")
FullwRLSC
confusionMatrix(FullwRLSC)

customGrid <- expand.grid(n.trees=667, shrinkage=0.05, interaction.depth=9, n.minobsinnode=46)
FullwRRSC <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + reg_whiff, data=swing_RRSC, method="gbm", trControl=control, tuneGrid=customGrid, metric="ROC")
FullwRRSC
confusionMatrix(FullwRRSC)

customGrid <- expand.grid(n.trees=726, shrinkage=0.01, interaction.depth=9, n.minobsinnode=20)
FullwRLC <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + reg_whiff, data=swing_RLC, method="gbm", trControl=control, tuneGrid=customGrid, metric="ROC")
FullwRLC
confusionMatrix(FullwRLC)

customGrid <- expand.grid(n.trees=993, shrinkage=0.01, interaction.depth=7, n.minobsinnode=24)
FullwRRC <- train(make.names(Miss) ~ plate_x + plate_z + release_speed + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + reg_whiff, data=swing_RRC, method="gbm", trControl=control, tuneGrid=customGrid, metric="ROC")
FullwRRC
confusionMatrix(FullwRRC)

customGrid <- expand.grid(n.trees=488, shrinkage=0.05, interaction.depth=10, n.minobsinnode=14)
FullwRLO <- train(make.names(Miss) ~ plate_x + plate_z + diff_fb + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + reg_whiff, data=swing_RLO, method="gbm", trControl=control, tuneGrid=customGrid, metric="ROC")
FullwRLO
confusionMatrix(FullwRLO)

customGrid <- expand.grid(n.trees=712, shrinkage=0.01, interaction.depth=4, n.minobsinnode=29)
FullwRRO <- train(make.names(Miss) ~ plate_x + plate_z + diff_fb + release_spin_rate + pfx_x + pfx_z + release_pos_x + release_pos_y + release_pos_z + reg_whiff, data=swing_RRO, method="gbm", trControl=control, tuneGrid=customGrid, metric="ROC")
FullwRRO
confusionMatrix(FullwRRO)

# CS

FullcsLLF <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_LLF, family="binomial", method="REML")

FullcsLRF <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_LRF, family="binomial", method="REML")

FullcsLLSC <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_LLSC, family="binomial", method="REML")

FullcsLRSC <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_LRSC, family="binomial", method="REML")

FullcsLLC <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_LLC, family="binomial", method="REML")

FullcsLRC <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_LRC, family="binomial", method="REML")

FullcsLLO <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_LLO, family="binomial", method="REML")

FullcsLRO <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_LRO, family="binomial", method="REML")

FullcsRLF <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_RLF, family="binomial", method="REML")

FullcsRRF <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_RRF, family="binomial", method="REML")

FullcsRLSC <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_RLSC, family="binomial", method="REML")

FullcsRRSC <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_RRSC, family="binomial", method="REML")

FullcsRLC <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_RLC, family="binomial", method="REML")

FullcsRRC <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_RRC, family="binomial", method="REML")

FullcsRLO <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_RLO, family="binomial", method="REML")

FullcsRRO <- bam(calledStrike ~ s(plate_x, plate_z, by=Count), data=taken_RRO, family="binomial", method="REML")

# Partitioning Test Data

bigtest <- sctot %>% filter(game_year == 2019)
bigtest$launch_angle <- ifelse(is.na(bigtest$launch_angle)==T, bigtest$reg_la, bigtest$launch_angle)

test_LLF <- bigtest %>% filter(p_throws == "L", stand == "L", pitch_type %in% fb)
test_LLF <- data.frame(lapply(test_LLF,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_LRF <- bigtest %>% filter(p_throws == "L", stand == "R", pitch_type %in% fb)
test_LRF <- data.frame(lapply(test_LRF,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_LLSC <- bigtest %>% filter(p_throws == "L", stand == "L", pitch_type %in% slct)
test_LLSC <- data.frame(lapply(test_LLSC,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_LRSC <- bigtest %>% filter(p_throws == "L", stand == "R", pitch_type %in% slct)
test_LRSC <- data.frame(lapply(test_LRSC,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_LLC <- bigtest %>% filter(p_throws == "L", stand == "L", pitch_type %in% cu)
test_LLC <- data.frame(lapply(test_LLC,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_LRC <- bigtest %>% filter(p_throws == "L", stand == "R", pitch_type %in% cu)
test_LRC <- data.frame(lapply(test_LRC,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_LLO <- bigtest %>% filter(p_throws == "L", stand == "L", pitch_type %in% ch)
test_LLO <- data.frame(lapply(test_LLO,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_LRO <- bigtest %>% filter(p_throws == "L", stand == "R", pitch_type %in% ch)
test_LRO <- data.frame(lapply(test_LRO,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))


test_RLF <- bigtest %>% filter(p_throws == "R", stand == "L", pitch_type %in% fb)
test_RLF <- data.frame(lapply(test_RLF,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_RRF <- bigtest %>% filter(p_throws == "R", stand == "R", pitch_type %in% fb)
test_RRF <- data.frame(lapply(test_RRF,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_RRF <- test_RRF %>% filter(Count != "4-1")
test_RLSC <- bigtest %>% filter(p_throws == "R", stand == "L", pitch_type %in% slct)
test_RLSC <- data.frame(lapply(test_RLSC,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_RRSC <- bigtest %>% filter(p_throws == "R", stand == "R", pitch_type %in% slct)
test_RRSC <- data.frame(lapply(test_RRSC,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_RLC <- bigtest %>% filter(p_throws == "R", stand == "L", pitch_type %in% cu)
test_RLC <- data.frame(lapply(test_RLC,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_RRC <- bigtest %>% filter(p_throws == "R", stand == "R", pitch_type %in% cu)
test_RRC <- data.frame(lapply(test_RRC,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_RLO <- bigtest %>% filter(p_throws == "R", stand == "L", pitch_type %in% ch)
test_RLO <- data.frame(lapply(test_RLO,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
test_RRO <- bigtest %>% filter(p_throws == "R", stand == "R", pitch_type %in% ch)
test_RRO <- data.frame(lapply(test_RRO,function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))

# Swing Probability

swLLFpred <- predict(FullswLLF, test_LLF[, -91], type="prob")
auc(test_LLF$Swing, swLLFpred$X1) 

swLRFpred <- predict(FullswLRF, test_LRF[, -91], type="prob")
auc(test_LRF$Swing, swLRFpred$X1) 

swLLSCpred <- predict(FullswLLSC, test_LLSC[, -91], type="prob")
auc(test_LLSC$Swing, swLLSCpred$X1)

swLRSCpred <- predict(FullswLRSC, test_LRSC[, -91], type="prob")
auc(test_LRSC$Swing, swLRSCpred$X1) 

swLLCpred <- predict(FullswLLC, test_LLC[, -91], type="prob")
auc(test_LLC$Swing, swLLCpred$X1)

swLRCpred <- predict(FullswLRC, test_LRC[, -91], type="prob")
auc(test_LRC$Swing, swLRCpred$X1) 

swLLOpred <- predict(FullswLLO, test_LLO[, -91], type="prob")
auc(test_LLO$Swing, swLLOpred$X1) 

swLROpred <- predict(FullswLRO, test_LRO[, -91], type="prob")
auc(test_LRO$Swing, swLROpred$X1) 

swRLFpred <- predict(FullswRLF, test_RLF[, -91], type="prob")
auc(test_RLF$Swing, swRLFpred$X1) 

swRRFpred <- predict(FullswRRF, test_RRF[, -91], type="prob")
auc(test_RRF$Swing, swRRFpred$X1) 

swRLSCpred <- predict(FullswRLSC, test_RLSC[, -91], type="prob")
auc(test_RLSC$Swing, swRLSCpred$X1) 

swRRSCpred <- predict(FullswRRSC, test_RRSC[, -91], type="prob")
auc(test_RRSC$Swing, swRRSCpred$X1) 

swRLCpred <- predict(FullswRLC, test_RLC[, -91], type="prob")
auc(test_RLC$Swing, swRLCpred$X1) 

swRRCpred <- predict(FullswRRC, test_RRC[, -91], type="prob")
auc(test_RRC$Swing, swRRCpred$X1) 

swRLOpred <- predict(FullswRLO, test_RLO[, -91], type="prob")
auc(test_RLO$Swing, swRLOpred$X1) 

swRROpred <- predict(FullswRRO, test_RRO[, -91], type="prob")
auc(test_RRO$Swing, swRROpred$X1)

# Assigning swing probabilities to test datasets

test_LLF$swingprob <- swLLFpred$X1

test_LRF$swingprob <- swLRFpred$X1

test_LLSC$swingprob <- swLLSCpred$X1

test_LRSC$swingprob <- swLRSCpred$X1

test_LLC$swingprob <- swLLCpred$X1

test_LRC$swingprob <- swLRCpred$X1

test_LLO$swingprob <- swLLOpred$X1

test_LRO$swingprob <- swLROpred$X1

test_RLF$swingprob <- swRLFpred$X1

test_RRF$swingprob <- swRRFpred$X1

test_RLSC$swingprob <- swRLSCpred$X1

test_RRSC$swingprob <- swRRSCpred$X1

test_RLC$swingprob <- swRLCpred$X1

test_RRC$swingprob <- swRRCpred$X1

test_RLO$swingprob <- swRLOpred$X1

test_RRO$swingprob <- swRROpred$X1


## Further Partitioning

tswing_LLF <- test_LLF %>% filter(Swing == 1)
tswing_LRF <- test_LRF %>% filter(Swing == 1)
tswing_LLSC <- test_LLSC %>% filter(Swing == 1)
tswing_LRSC <- test_LRSC %>% filter(Swing == 1)
tswing_LLC <- test_LLC %>% filter(Swing == 1)
tswing_LRC <- test_LRC %>% filter(Swing == 1)
tswing_LLO <- test_LLO %>% filter(Swing == 1)
tswing_LRO <- test_LRO %>% filter(Swing == 1)

tswing_RLF <- test_RLF %>% filter(Swing == 1)
tswing_RRF <- test_RRF %>% filter(Swing == 1)
tswing_RLSC <- test_RLSC %>% filter(Swing == 1)
tswing_RRSC <- test_RRSC %>% filter(Swing == 1)
tswing_RLC <- test_RLC %>% filter(Swing == 1)
tswing_RRC <- test_RRC %>% filter(Swing == 1)
tswing_RLO <- test_RLO %>% filter(Swing == 1)
tswing_RRO <- test_RRO %>% filter(Swing == 1)

# Called Ball/Strike Probability

csLLFpred <- predict(FullcsLLF, ttaken_LLF[, -116], type="response")
auc(ttaken_LLF$calledStrike, csLLFpred)

csLRFpred <- predict(FullcsLRF, ttaken_LRF[, -116], type="response")
auc(ttaken_LRF$calledStrike, csLRFpred) 

csLLSCpred <- predict(FullcsLLSC, ttaken_LLSC[, -116], type="response")
auc(ttaken_LLSC$calledStrike, csLLSCpred) 

csLRSCpred <- predict(FullcsLRSC, ttaken_LRSC[, -116], type="response")
auc(ttaken_LRSC$calledStrike, csLRSCpred) 

csLLCpred <- predict(FullcsLLC, ttaken_LLC[, -116], type="response")
auc(ttaken_LLC$calledStrike, csLLCpred) 

csLRCpred <- predict(FullcsLRC, ttaken_LRC[, -116], type="response")
auc(ttaken_LRC$calledStrike, csLRCpred) 

csLLOpred <- predict(FullcsLLO, ttaken_LLO[, -116], type="response")
auc(ttaken_LLO$calledStrike, csLLOpred) 

csLROpred <- predict(FullcsLRO, ttaken_LRO[, -116], type="response")
auc(ttaken_LRO$calledStrike, csLROpred) 

csRLFpred <- predict(FullcsRLF, ttaken_RLF[, -116], type="response")
auc(ttaken_RLF$calledStrike, csRLFpred) 

csRRFpred <- predict(FullcsRRF, ttaken_RRF[, -116], type="response")
auc(ttaken_RRF$calledStrike, csRRFpred) 

csRLSCpred <- predict(FullcsRLSC, ttaken_RLSC[, -116], type="response")
auc(ttaken_RLSC$calledStrike, csRLSCpred) 

csRRSCpred <- predict(FullcsRRSC, ttaken_RRSC[, -116], type="response")
auc(ttaken_RRSC$calledStrike, csRRSCpred) 

csRLCpred <- predict(FullcsRLC, ttaken_RLC[, -116], type="response")
auc(ttaken_RLC$calledStrike, csRLCpred)

csRRCpred <- predict(FullcsRRC, ttaken_RRC[, -116], type="response")
auc(ttaken_RRC$calledStrike, csRRCpred) 

csRLOpred <- predict(FullcsRLO, ttaken_RLO[, -116], type="response")
auc(ttaken_RLO$calledStrike, csRLOpred)

csRROpred <- predict(FullcsRRO, ttaken_RRO[, -116], type="response")
auc(ttaken_RRO$calledStrike, csRROpred) 

# Whiff probability

whiffLLFpred <- predict(FullwLLF, tswing_LLF[, -92], type="prob")
auc(tswing_LLF$Miss, whiffLLFpred$X1) 

whiffLRFpred <- predict(FullwLRF, tswing_LRF[, -92], type="prob")
auc(tswing_LRF$Miss, whiffLRFpred$X1) 

whiffLLSCpred <- predict(FullwLLSC, tswing_LLSC[, -92], type="prob")
auc(tswing_LLSC$Miss, whiffLLSCpred$X1) 

whiffLRSCpred <- predict(FullwLRSC, tswing_LRSC[, -92], type="prob")
auc(tswing_LRSC$Miss, whiffLRSCpred$X1)

whiffLLCpred <- predict(FullwLLC, tswing_LLC[, -92], type="prob")
auc(tswing_LLC$Miss, whiffLLCpred$X1)

whiffLRCpred <- predict(FullwLRC, tswing_LRC[, -92], type="prob")
auc(tswing_LRC$Miss, whiffLRCpred$X1) 

whiffLLOpred <- predict(FullwLLO, tswing_LLO[, -92], type="prob")
auc(tswing_LLO$Miss, whiffLLOpred$X1) 

whiffLROpred <- predict(FullwLRO, tswing_LRO[, -92], type="prob")
auc(tswing_LRO$Miss, whiffLROpred$X1) 

whiffRLFpred <- predict(FullwRLF, tswing_RLF[, -92], type="prob")
auc(tswing_RLF$Miss, whiffRLFpred$X1)

whiffRRFpred <- predict(FullwRRF, tswing_RRF[, -92], type="prob")
auc(tswing_RRF$Miss, whiffRRFpred$X1)

whiffRLSCpred <- predict(FullwRLSC, tswing_RLSC[, -92], type="prob")
auc(tswing_RLSC$Miss, whiffRLSCpred$X1) 

whiffRRSCpred <- predict(FullwRRSC, tswing_RRSC[, -92], type="prob")
auc(tswing_RRSC$Miss, whiffRRSCpred$X1) 

whiffRLCpred <- predict(FullwRLC, tswing_RLC[, -92], type="prob")
auc(tswing_RLC$Miss, whiffRLCpred$X1) 

whiffRRCpred <- predict(FullwRRC, tswing_RRC[, -92], type="prob")
auc(tswing_RRC$Miss, whiffRRCpred$X1) 

whiffRLOpred <- predict(FullwRLO, tswing_RLO[, -92], type="prob")
auc(tswing_RLO$Miss, whiffRLOpred$X1) 

whiffRROpred <- predict(FullwRRO, tswing_RRO[, -92], type="prob")
auc(tswing_RRO$Miss, whiffRROpred$X1) 

# Assigning CS, Whiff probabilities

ttaken_LLF$csprob <- csLLFpred
tswing_LLF$whiffprob <- whiffLLFpred$X1

ttaken_LRF$csprob <- csLRFpred
tswing_LRF$whiffprob <- whiffLRFpred$X1

ttaken_LLSC$csprob <- csLLSCpred
tswing_LLSC$whiffprob <- whiffLLSCpred$X1

ttaken_LRSC$csprob <- csLRSCpred
tswing_LRSC$whiffprob <- whiffLRSCpred$X1

ttaken_LLC$csprob <- csLLCpred
tswing_LLC$whiffprob <- whiffLLCpred$X1

ttaken_LRC$csprob <- csLRCpred
tswing_LRC$whiffprob <- whiffLRCpred$X1

ttaken_LLO$csprob <- csLLOpred
tswing_LLO$whiffprob <- whiffLLOpred$X1

ttaken_LRO$csprob <- csLROpred
tswing_LRO$whiffprob <- whiffLROpred$X1

ttaken_RLF$csprob <- csRLFpred
tswing_RLF$whiffprob <- whiffRLFpred$X1

ttaken_RRF$csprob <- csRRFpred
tswing_RRF$whiffprob <- whiffRRFpred$X1

ttaken_RLSC$csprob <- csRLSCpred
tswing_RLSC$whiffprob <- whiffRLSCpred$X1

ttaken_RRSC$csprob <- csRRSCpred
tswing_RRSC$whiffprob <- whiffRRSCpred$X1

ttaken_RLC$csprob <- csRLCpred
tswing_RLC$whiffprob <- whiffRLCpred$X1

ttaken_RRC$csprob <- csRRCpred
tswing_RRC$whiffprob <- whiffRRCpred$X1

ttaken_RLO$csprob <- csRLOpred
tswing_RLO$whiffprob <- whiffRLOpred$X1

ttaken_RRO$csprob <- csRROpred
tswing_RRO$whiffprob <- whiffRROpred$X1

# Assigning rest of probabilites to 0

ttaken_LLF$whiffprob <- 0
tswing_LLF$csprob <- 0

ttaken_LRF$whiffprob <- 0
tswing_LRF$csprob <- 0

ttaken_LLSC$whiffprob <- 0
tswing_LLSC$csprob <- 0

ttaken_LRSC$whiffprob <- 0
tswing_LRSC$csprob <- 0

ttaken_LLC$whiffprob <- 0
tswing_LLC$csprob <- 0

ttaken_LRC$whiffprob <- 0
tswing_LRC$csprob <- 0

ttaken_LLO$whiffprob <- 0
tswing_LLO$csprob <- 0

ttaken_LRO$whiffprob <- 0
tswing_LRO$csprob <- 0

ttaken_RLF$whiffprob <- 0
tswing_RLF$csprob <- 0

ttaken_RRF$whiffprob <- 0
tswing_RRF$csprob <- 0

ttaken_RLSC$whiffprob <- 0
tswing_RLSC$csprob <- 0

ttaken_RRSC$whiffprob <- 0
tswing_RRSC$csprob <- 0

ttaken_RLC$whiffprob <- 0
tswing_RLC$csprob <- 0

ttaken_RRC$whiffprob <- 0
tswing_RRC$csprob <- 0

ttaken_RLO$whiffprob <- 0
tswing_RLO$csprob <- 0

ttaken_RRO$whiffprob <- 0
tswing_RRO$csprob <- 0

# Joining Data

pitches_2018 <- rbind(ttaken_LLF, ttaken_LRF, ttaken_LLSC, ttaken_LRSC, ttaken_LLC, ttaken_LRC, ttaken_LLO, ttaken_LRO, ttaken_RLF, ttaken_RRF, ttaken_RLSC, ttaken_RRSC, ttaken_RLC, ttaken_RRC, ttaken_RLO, ttaken_RRO, tswing_LLF, tswing_LRF, tswing_LLSC, tswing_LRSC, tswing_LLC, tswing_LRC, tswing_LLO, tswing_LRO, tswing_RLF, tswing_RRF, tswing_RLSC, tswing_RRSC, tswing_RLC, tswing_RRC, tswing_RLO, tswing_RRO)

