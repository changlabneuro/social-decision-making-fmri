#Run this code after social_decision_MVPA.R and...
#social_decision_MVPA_permutations_pt1.R are completed
#It draws a null distribution from permutations and compares actual accuracy to that
#Adapted from code Kayla Velnoskey wrote in 2017
#Written by Matt Piva in 2018

model<-"glmnet"
savevars<-c("savevars", "ROI", "model")


for (ROI in c("ROI_list_here")) # Specify the ROIs you want to loop through

{
  
  print(sprintf("Working on ROI %s.", ROI))
  rm(list=setdiff(ls(), savevars))
  Null_dist<-matrix(ncol=20,nrow=10000) # Col should equal # participants
  permfile_1<-sprintf("/Directory/Group20_%s_%s_train_a0_permutations_1.csv",ROI, model) # Load in permutation data from all four cross-validation iterations
  permfile_2<-sprintf("/Directory/Group20_%s_%s_train_a0_permutations_2.csv",ROI, model)
  permfile_3<-sprintf("/Directory/Group20_%s_%s_train_a0_permutations_3.csv",ROI, model)
  permfile_4<-sprintf("/Directory/Group20_%s_%s_train_a0_permutations_4.csv",ROI, model)
  classfile_1<-sprintf("/Directory/Group20_%s_%s_a0_train_both_model_perf_1.csv",ROI, model) # Load in the actual accuracy data from all four cross-validation iterations
  classfile_2<-sprintf("/Directory/Group20_%s_%s_a0_train_both_model_perf_2.csv",ROI, model)
  classfile_3<-sprintf("/Directory/Group20_%s_%s_a0_train_both_model_perf_3.csv",ROI, model)
  classfile_4<-sprintf("/Directory/Group20_%s_%s_a0_train_both_model_perf_4.csv",ROI, model)
  
  # Read everything in
  permutations_1<-read.csv(permfile_1, skip=1)
  permutations_1<-permutations_1[,-1]
  permutations_2<-read.csv(permfile_2, skip=1)
  permutations_2<-permutations_2[,-1]
  permutations_3<-read.csv(permfile_3, skip=1)
  permutations_3<-permutations_3[,-1]
  permutations_4<-read.csv(permfile_4, skip=1)
  permutations_4<-permutations_4[,-1]
  
  classified_1<-read.csv(classfile_1,skip=1)
  classified_1<-classified_1[,-1]
  classified_2<-read.csv(classfile_2,skip=1)
  classified_2<-classified_2[,-1]
  classified_3<-read.csv(classfile_3,skip=1)
  classified_3<-classified_3[,-1]
  classified_4<-read.csv(classfile_4,skip=1)
  classified_4<-classified_4[,-1]
  
  # Get actual accuracies and average them
  Group_Acc_1<-mean(classified_1$Accuracy, na.rm=TRUE)
  Group_Acc_2<-mean(classified_2$Accuracy, na.rm=TRUE)
  Group_Acc_3<-mean(classified_3$Accuracy, na.rm=TRUE)
  Group_Acc_4<-mean(classified_4$Accuracy, na.rm=TRUE)
  
  Group_Acc_all<-c(Group_Acc_1, Group_Acc_2, Group_Acc_3, Group_Acc_4)
  
  Group_Acc<-mean(Group_Acc_all)
  
  # Draw 1/4th of 10000 permutations from each participant
  Null_dist[1:2500,1]<-sample(permutations_1[,1], 2500, replace=TRUE)
  Null_dist[2501:5000,1]<-sample(permutations_2[,1], 2500, replace=TRUE)
  Null_dist[5001:7500,1]<-sample(permutations_3[,1], 2500, replace=TRUE)
  Null_dist[7501:10000,1]<-sample(permutations_4[,1], 2500, replace=TRUE)
  Null_dist[1:2500,2]<-sample(permutations_1[,2], 2500, replace=TRUE)
  Null_dist[2501:5000,2]<-sample(permutations_2[,2], 2500, replace=TRUE)
  Null_dist[5001:7500,2]<-sample(permutations_3[,2], 2500, replace=TRUE)
  Null_dist[7501:10000,2]<-sample(permutations_4[,2], 2500, replace=TRUE)
  Null_dist[1:2500,3]<-sample(permutations_1[,3], 2500, replace=TRUE)
  Null_dist[2501:5000,3]<-sample(permutations_2[,3], 2500, replace=TRUE)
  Null_dist[5001:7500,3]<-sample(permutations_3[,3], 2500, replace=TRUE)
  Null_dist[7501:10000,3]<-sample(permutations_4[,3], 2500, replace=TRUE)
  Null_dist[1:2500,4]<-sample(permutations_1[,4], 2500, replace=TRUE)
  Null_dist[2501:5000,4]<-sample(permutations_2[,4], 2500, replace=TRUE)
  Null_dist[5001:7500,4]<-sample(permutations_3[,4], 2500, replace=TRUE)
  Null_dist[7501:10000,4]<-sample(permutations_4[,4], 2500, replace=TRUE)
  Null_dist[1:2500,5]<-sample(permutations_1[,5], 2500, replace=TRUE)
  Null_dist[2501:5000,5]<-sample(permutations_2[,5], 2500, replace=TRUE)
  Null_dist[5001:7500,5]<-sample(permutations_3[,5], 2500, replace=TRUE)
  Null_dist[7501:10000,5]<-sample(permutations_4[,5], 2500, replace=TRUE)
  Null_dist[1:2500,6]<-sample(permutations_1[,6], 2500, replace=TRUE)
  Null_dist[2501:5000,6]<-sample(permutations_2[,6], 2500, replace=TRUE)
  Null_dist[5001:7500,6]<-sample(permutations_3[,6], 2500, replace=TRUE)
  Null_dist[7501:10000,6]<-sample(permutations_4[,6], 2500, replace=TRUE)
  Null_dist[1:2500,7]<-sample(permutations_1[,7], 2500, replace=TRUE)
  Null_dist[2501:5000,7]<-sample(permutations_2[,7], 2500, replace=TRUE)
  Null_dist[5001:7500,7]<-sample(permutations_3[,7], 2500, replace=TRUE)
  Null_dist[7501:10000,7]<-sample(permutations_4[,7], 2500, replace=TRUE)
  Null_dist[1:2500,8]<-sample(permutations_1[,8], 2500, replace=TRUE)
  Null_dist[2501:5000,8]<-sample(permutations_2[,8], 2500, replace=TRUE)
  Null_dist[5001:7500,8]<-sample(permutations_3[,8], 2500, replace=TRUE)
  Null_dist[7501:10000,8]<-sample(permutations_4[,8], 2500, replace=TRUE)
  Null_dist[1:2500,9]<-sample(permutations_1[,9], 2500, replace=TRUE)
  Null_dist[2501:5000,9]<-sample(permutations_2[,9], 2500, replace=TRUE)
  Null_dist[5001:7500,9]<-sample(permutations_3[,9], 2500, replace=TRUE)
  Null_dist[7501:10000,9]<-sample(permutations_4[,9], 2500, replace=TRUE)
  Null_dist[1:2500,10]<-sample(permutations_1[,10], 2500, replace=TRUE)
  Null_dist[2501:5000,10]<-sample(permutations_2[,10], 2500, replace=TRUE)
  Null_dist[5001:7500,10]<-sample(permutations_3[,10], 2500, replace=TRUE)
  Null_dist[7501:10000,10]<-sample(permutations_4[,10], 2500, replace=TRUE)
  Null_dist[1:2500,11]<-sample(permutations_1[,11], 2500, replace=TRUE)
  Null_dist[2501:5000,11]<-sample(permutations_2[,11], 2500, replace=TRUE)
  Null_dist[5001:7500,11]<-sample(permutations_3[,11], 2500, replace=TRUE)
  Null_dist[7501:10000,11]<-sample(permutations_4[,11], 2500, replace=TRUE)
  Null_dist[1:2500,12]<-sample(permutations_1[,12], 2500, replace=TRUE)
  Null_dist[2501:5000,12]<-sample(permutations_2[,12], 2500, replace=TRUE)
  Null_dist[5001:7500,12]<-sample(permutations_3[,12], 2500, replace=TRUE)
  Null_dist[7501:10000,12]<-sample(permutations_4[,12], 2500, replace=TRUE)
  Null_dist[1:2500,13]<-sample(permutations_1[,13], 2500, replace=TRUE)
  Null_dist[2501:5000,13]<-sample(permutations_2[,13], 2500, replace=TRUE)
  Null_dist[5001:7500,13]<-sample(permutations_3[,13], 2500, replace=TRUE)
  Null_dist[7501:10000,13]<-sample(permutations_4[,13], 2500, replace=TRUE)
  Null_dist[1:2500,14]<-sample(permutations_1[,14], 2500, replace=TRUE)
  Null_dist[2501:5000,14]<-sample(permutations_2[,14], 2500, replace=TRUE)
  Null_dist[5001:7500,14]<-sample(permutations_3[,14], 2500, replace=TRUE)
  Null_dist[7501:10000,14]<-sample(permutations_4[,14], 2500, replace=TRUE)
  Null_dist[1:2500,15]<-sample(permutations_1[,15], 2500, replace=TRUE)
  Null_dist[2501:5000,15]<-sample(permutations_2[,15], 2500, replace=TRUE)
  Null_dist[5001:7500,15]<-sample(permutations_3[,15], 2500, replace=TRUE)
  Null_dist[7501:10000,15]<-sample(permutations_4[,15], 2500, replace=TRUE)
  Null_dist[1:2500,16]<-sample(permutations_1[,16], 2500, replace=TRUE)
  Null_dist[2501:5000,16]<-sample(permutations_2[,16], 2500, replace=TRUE)
  Null_dist[5001:7500,16]<-sample(permutations_3[,16], 2500, replace=TRUE)
  Null_dist[7501:10000,16]<-sample(permutations_4[,16], 2500, replace=TRUE)
  Null_dist[1:2500,17]<-sample(permutations_1[,17], 2500, replace=TRUE)
  Null_dist[2501:5000,17]<-sample(permutations_2[,17], 2500, replace=TRUE)
  Null_dist[5001:7500,17]<-sample(permutations_3[,17], 2500, replace=TRUE)
  Null_dist[7501:10000,17]<-sample(permutations_4[,17], 2500, replace=TRUE)
  Null_dist[1:2500,18]<-sample(permutations_1[,18], 2500, replace=TRUE)
  Null_dist[2501:5000,18]<-sample(permutations_2[,18], 2500, replace=TRUE)
  Null_dist[5001:7500,18]<-sample(permutations_3[,18], 2500, replace=TRUE)
  Null_dist[7501:10000,18]<-sample(permutations_4[,18], 2500, replace=TRUE)
  Null_dist[1:2500,19]<-sample(permutations_1[,19], 2500, replace=TRUE)
  Null_dist[2501:5000,19]<-sample(permutations_2[,19], 2500, replace=TRUE)
  Null_dist[5001:7500,19]<-sample(permutations_3[,19], 2500, replace=TRUE)
  Null_dist[7501:10000,19]<-sample(permutations_4[,19], 2500, replace=TRUE)
  Null_dist[1:2500,20]<-sample(permutations_1[,20], 2500, replace=TRUE)
  Null_dist[2501:5000,20]<-sample(permutations_2[,20], 2500, replace=TRUE)
  Null_dist[5001:7500,20]<-sample(permutations_3[,20], 2500, replace=TRUE)
  Null_dist[7501:10000,20]<-sample(permutations_4[,20], 2500, replace=TRUE)
  
  # Average permutation accuracies to creat null
  mean<-rowMeans(Null_dist)
  
  # Get p-value by comparing actual to null distribution
  Group_p<-length(which(mean>Group_Acc))/10000

  len <- length(mean)
  length(Group_Acc) <- len                      
  length(Group_p) <- len
  
  # Write results from each ROI to file
  All_perm <- cbind(mean, Group_Acc, Group_p)
  write.csv(All_perm,sprintf("/Directory/Group20_%s_%s_train_a0_pval.csv", ROI, model))

}
