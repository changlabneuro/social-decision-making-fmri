#This code takes three components:
#1) A text files with two columns...
#a list of binary labels high or low value trials in one, then...
#a list of binary labels for Self or Other trials in the other column
#2) Betas for voxels across the whole brain, modeling the appropriate period in each trial
#3) A mask or set of masks for the ROIs you're interested in
#The code trains on either Self or Other trials and tests on the other using cross-validation
#Most of this is hard-coded for 120 Self trials, 120 Other trials, four-fold validation...
#but you can change this to fit your needs
#Run this file first, then social_decision_MVPA_permutations_pt1.R, and then...
#social_decision_MVPA_permutations_pt2.R to get signficance measures
#Adapted from code Kayla Velnoskey wrote in 2017
#Written by Matt Piva in 2018

# First, load necessary libraries
library(caret)
library(RNifti)
library(glmnet)
library(pROC)
library(doMC)
library(gplots)
library(RColorBrewer)

model<-"glmnet" # Specify the model you want to use

registerDoMC(cores = 20)
Group20_train <- matrix(ncol=19, nrow=20) # Row should be # of subjects
Group20_test <- matrix(ncol=19, nrow=20) # Row should be # of subjects

savevars<-c("subject","Group20_train","Group20_test","scounter","savevars", "ROI", "model", "Pred20_train", "Pred20_test", "i", "range_in", "range_out")

for (ROI in c("ROI_list_here")) # Specify the ROIs you want to loop through

{
  Pred20_train<-1:120
  Pred20_test<-1:120
  scounter<-0
  
  for (i in 1:4) # These are the iterations for cross-validation
    
  {
    for (subject in c("Subject_list_here")) # Specify the subjects you want to loop through
    
    {
      # This section specifies ranges of trials to train and test on
      if ( i==1 ) {
        range_in<-1:90
        range_out<-91:120
      } else if ( i==2 ) {
        range_in_1<-1:60
        range_in_2<-91:120
        range_in<-cbind(t(range_in_1),t(range_in_2))
        range_out<-61:90
      } else if ( i==3 ) {
        range_in_1<-1:30
        range_in_2<-61:120
        range_in<-cbind(t(range_in_1),t(range_in_2))
        range_out<-31:60
      } else if ( i==4 ) {
        range_in<-31:120
        range_out<-1:30
      }
      
      print(sprintf("Working on ROI %s for subject %s for iteration %s.", ROI, subject, i))
      rm(list=setdiff(ls(), savevars))
      if ( subject!="PA0118" ) {
        scounter<-scounter+1
      } else if ( subject=="PA0118" ) {
        scounter<-1
        rm(Group20_train)
        rm(Group20_test)
        Group20_train <- matrix(ncol=19, nrow=20)
        Group20_test <- matrix(ncol=19, nrow=20)
      }
        
      mrifile<-sprintf("/Directory/%s_TD_MVPA_stats_REML_nof.nii.gz", subject) # Load in your whole brain betas
      maskfile<-sprintf("/Directory/%s_%s_resample.nii.gz", subject, ROI) # Load in your mask, morphed to each individual participant
      classfile_test<-sprintf("/Directory/MVPA_both_online_choice_difference_%s",subject) # Load in your two-column trial label file
  
      
      # Read in and organize betas
      mridata<-readNifti(mrifile)
      justmridata_test<-mridata[,,,]
      reorg_mridata_test<-aperm(justmridata_test,c(4,1,2,3))
      dim(reorg_mridata_test)<-c(dim(reorg_mridata_test)[1],prod(dim(reorg_mridata_test)[2:4]))
      
      # Read in mask data
      maskdata<-readNifti(maskfile)
      # Eliminate non-mask brain data
      justmaskdata<-maskdata[,,]
      reorg_maskdata<-justmaskdata
      dim(reorg_maskdata)<-c(prod(dim(reorg_maskdata)[1:3]))
  
      mask_test<-reorg_mridata_test[,which(reorg_maskdata>0)]
      mask_test_dat<-as.data.frame(mask_test)
      
      # Organize labels
      classes_test<-read.table(classfile_test)
      names(classes_test)[names(classes_test)=="V1"] <- "Condition"
      names(classes_test)[names(classes_test)=="V2"] <- "Agent"
  
      dat_test<-cbind.data.frame(classes_test,mask_test_dat)
      
      dat_test$Condition[dat_test$Condition==0]<-"Low"
      dat_test$Condition[dat_test$Condition==1]<-"High"
      
      dat_test$Agent[dat_test$Agent==0]<-"Self"
      dat_test$Agent[dat_test$Agent==1]<-"Other"
      
      dat_test$Condition<-factor(dat_test$Condition)
      dat_test_scale<-scale(dat_test[,-(1:2)])
      dat_test_scale[is.nan(dat_test_scale)] <- 0
      
      train_index<-which(dat_test$Agent=="Self") # This should be the label of the data you want to train on
      test_index<-which(dat_test$Agent=="Other") # This should be the label of the data you want to test on
      test_index<-sort(test_index)
      
      # Set conditions and run glmnet model
      ctrl<-trainControl(method = "cv",
                         number=4,
                         classProbs = TRUE, 
                         summaryFunction = multiClassSummary,
                         savePredictions="final",
                         sampling="down")
  
      tmp<-glmnet(x = dat_test_scale[train_index[range_in],],
                  y = dat_test$Condition[train_index[range_in]],
                  family="binomial",
                  nlambda=100,
                  alpha=0)
      
      lambdas<-tmp$lambda
      
      glmnGrid<-expand.grid(.alpha=0,
                            .lambda=lambdas)
      
      set.seed(123)
      
      glmnTuned <- train(x = dat_test_scale[train_index[range_in],],
                         y = dat_test$Condition[train_index[range_in]],
                         preProc = c("center","scale"),
                         method = model,
                         tuneGrid = glmnGrid,
                         trControl = ctrl)
      
      # Test model on remaining trials from training trialset
      pred_model_train <- predict(glmnTuned,dat_test_scale[train_index[range_out],])
      predict_confuse_train <- confusionMatrix(pred_model_train, dat_test$Condition[train_index[range_out]])
      
      # Test model on trials from testing trialset
      pred_model_test <- predict(glmnTuned,dat_test_scale[test_index[range_out],])
      predict_confuse_test <- confusionMatrix(pred_model_test, dat_test$Condition[test_index[range_out]])
      
      # Now organize the output data measuring performance
      best_all_train<-cbind.data.frame(t(predict_confuse_train$overall),t(predict_confuse_train$byClass))
      performance_test<-cbind.data.frame(t(predict_confuse_test$overall),t(predict_confuse_test$byClass))
      
      Group20_train[scounter,1]<-subject
      Group20_train[scounter,2:19]<-as.matrix(best_all_train[1,1:18])
      Group_header_train<-colnames(best_all_train)
      
      Group20_test[scounter,1]<-subject
      Group20_test[scounter,2:19]<-as.matrix(performance_test[1,1:18])
      Group_header_test<-colnames(performance_test)
    }
    
    # Now write these data to files
    Group_header_train_r<-cbind(c(1),t(Group_header_train))
    Group20_train_all<-rbind(Group_header_train_r,Group20_train)
    write.csv(Group20_train_all,sprintf("/Directory/Group20_%s_%s_a0_train_both_model_perf_%s.csv", ROI, model, i))
    
    Group_header_test_r<-cbind(c(1),t(Group_header_test))
    Group20_test_all<-rbind(Group_header_test_r,Group20_test)
    write.csv(Group20_test_all,sprintf("/Directory/Group20_%s_%s_a0_test_both_model_perf_%s.csv", ROI, model, i))
  }
}
