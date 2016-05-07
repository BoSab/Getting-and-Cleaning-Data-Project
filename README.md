# gcd-project
Getting and Cleaning Data Project

The first part of the code collects all the different datasets and stores them in one combined dataset. 
 - It is assumed that the script "run_analysis.R" is located in the R working directory, together with the "UCI HAR Dataset".  - The code first loads X,Y and subject datasets for both train and test data, and also loads "features" and "activity labels" datasets.
 - Column names for X datasets (for both train and test) are assigned using the values from "feaures" dataset.
 - Separate datasets are created for train and test, by combinig "X" dataset with "Subject" and "Y" data. This creates two datasets which incorporate data on the subjects that were tested (people who wore the devices), activities they performed (listed in Y datasets) and the actual recorded measurements (which were normalized, and are in [-1,1] range).
 - These two datasets are then combined to form one large dataset - "combdata"

The second part of the code retains only the variables (columns) which include means and standard deviations of the measurements.
 - Columns that should be retained were selected using the grepl() function.
 - Keywords were ".mean" and ".std". Dots were added in order to exclude the additional vectors (gravityMean,tBodyAccMean), which I did not consider as part of the data that should be retained (for these additional vectors, there are no measurements along the X,Y,Z axes - therefore they are inconsistent with the other variables).
 - Since it refers to mean frequency and not the mean, ".meanFreq" was excluded from the list of variables to be retained.
 - The "cleaned" dataset is created using the select() function from the dplyr package, and the result is stored in the data frame "data"
 
In the third part, the column "activity" was changed by replacing codes with appropriate descriptions of activities. This was done using the mutate() function.

In the fouth part of the assignment, I changed the variable names so that they become more appropriate.
 - Because of the easier text manipulation i created the names.txt file from column names and changed the variable names in the Notepad instead of in R.
 - There were not many changes: 
     * Variable names had 3 dots before the name of the axis (e.g. tBodyAcc.mean...X). These were reduced to one dot (tBodyAcc.mean.X).
     * Several variables had a typo - double the word "Body". This was corrected.
 - After the corrections were made, the text file was saved as "names1" and returned in R and the new column names for "data" data frame were assigned.

The fifth part of the code groups the data by subject and activity using the group_by() function. The new dataset, called "data2" is then created, and it sumarizes the data by subject and activity, while the arithmetic mean for each variable is presented. 
 - The first column lists the subjects which took part in the experiment.
 - The second column lists the activities performed by each of the subjects
 - The next columns contain the means of apppropriate variables, for appropriate subjects and activities.
 - This dataset is then writtend as the text file, called dataset.txt.
