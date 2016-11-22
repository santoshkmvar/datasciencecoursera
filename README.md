#This branch contains Data Science Project from John Hopkin's for Module3(Getting & Cleaning Data). It contains following Files:-

##CodeBook.txt
### This file contain info about dataset, project objective & execution strategy.
##run_analysis.R
### This file is R file which is source code. This file merges test,train,subject,activity data. Filter in only those column which have mean|std in name and apply mean on all the columns split by subject Id, activity Id. It also merge with activity description and give meaninful name to all column of data frame. At Last it sort the data frame by subjectId, Activity Id and write it into tidy data set as file.
##TidySet.txt
### This file is an output file generated as an execution of this R program.
