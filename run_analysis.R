# You should create one R script called run_analysis.R that does the following.
#
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
#
# From the data set in step 4, creates a second, independent tidy data set with
# the average of each variable for each activity and each subject.
run_analysis <- function() {

        # Initialisation
        init()

        # Load test and train data in a single DF
        train <- load_data("train")
        test <- load_data("test")
        result <- rbind(train, test)

        # Load activity names
        base_dir <- paste("./UCI HAR Dataset/", sep = "")
        file <- paste(base_dir, "activity_labels.txt",  sep = "")
        message("loading ", file, "...")
        activities <- read.csv(file=file, header=FALSE, sep = " ")
        names(activities) <- c("Activity ID", "Activity")

        # Add the Activity name as a separate column
        result <- merge(x = activities, y = result)

        tidy <- cbind(result %>% group_by(Activity, Subjects) %>% summarise_each(funs(mean)) %>% select(contains("mean")),
                      result %>% group_by(Activity, Subjects) %>% summarise_each(funs(mean)) %>% select(contains("std" )))
        names(tidy) <- headers()
        tidy
}

# Load the training/test set
# param mode: either "training" or "test"
load_data <- function(mode) {
        base_dir <- paste("./UCI HAR Dataset/", mode, sep = "")

        # Load set activity values
        file <- paste(base_dir, "/y_", mode, ".txt", sep="")
        message("loading ", file, "...")
        activities <- read.csv(file = file, header = FALSE)
        header <- c("Activity ID")
        names(activities) <- c(header)

        # Load subjects
        base_dir <- paste("./UCI HAR Dataset/", mode, sep = "")
        file <- paste(base_dir, "/subject_",  mode, ".txt", sep = "")
        message("loading ", file, "...")
        subjects <- read.csv(file = file, header = FALSE)
        names(subjects) <- c("Subjects")

        #### Load features sets ####
        ## Loading the headers
        message("loading ", file, "...")
        file <- paste(base_dir, "/../features.txt", sep = "")
        feature_headers <- read.csv(file = file, header = FALSE, sep = " ")

        ##Load content
        file <- paste(base_dir, "/X_", mode, ".txt", sep = "")
        message("loading ", file, "...")
        features <- read_fwf(file = file,
                           col_positions = fwf_widths(rep(16, times=561)))
        names(features) <- feature_headers[,2]

        data_set <- rep(mode, times = nrow(features))
        cbind(subjects, activities, data_set, features)

}

# Load required libraries
init <- function() {
        library(readr)
        #library(plyr)
        library(dplyr)
        library(reshape2)
}

headers <- function() {
        c("Activity","Time Body Accelerometer Mean-X",
            "Time Body Accelerometer Mean-Y","Time Body Accelerometer Mean-Z",
            "Time Gravity Accelerometer Mean-X","Time Gravity Accelerometer Mean-Y",
            "Time Gravity Accelerometer Mean-Z","Time Body Accelerometer Jerk Signal Mean-X",
            "Time Body Accelerometer Jerk Signal Mean-Y","Time Body Accelerometer Jerk Signal Mean-Z",
            "Time Body Gyroscope Mean-X",  "Time Body Gyroscope Mean-Y",
            "Time Body Gyroscope Mean-Z",  "Time Body Gyroscope Jerk Signal Mean-X",
            "Time Body Gyroscope Jerk Signal Mean-Y","Time Body Gyroscope Jerk Signal Mean-Z",
            "Time Body Accelerometer Magnitude Mean",  "Time Gravity Accelerometer Magnitude Mean",
            "Time Body Accelerometer Jerk Signal Magnitude Mean","Time Body Gyroscope Magnitude Mean",
            "Time Body Gyroscope Jerk Signal Magnitude Mean","Frequency Body Acceleration Mean-X",
            "Frequency Body Acceleration Mean-Y","Frequency Body Acceleration Mean-Z",
            "Frequency Body Acceleration- Mean Frequency-X","Frequency Body Acceleration- Mean Frequency-Y",
            "Frequency Body Acceleration- Mean Frequency-Z","Frequency Body Acceleration Jerk Signals Mean-X",
            "Frequency Body Acceleration Jerk Signals Mean-Y","Frequency Body Acceleration Jerk Signals Mean-Z",
            "Frequency Body Acceleration Jerk Signals- Mean Frequency-X","Frequency Body Acceleration Jerk Signals- Mean Frequency-Y",
            "Frequency Body Acceleration Jerk Signals- Mean Frequency-Z","Frequency Body Gyroscope Mean-X",
            "Frequency Body Gyroscope Mean-Y",  "Frequency Body Gyroscope Mean-Z",
            "Frequency Body Gyroscope- Mean Frequency-X","Frequency Body Gyroscope- Mean Frequency-Y",
            "Frequency Body Gyroscope- Mean Frequency-Z","Frequency Body Acceleration Magnitude Mean",
            "Frequency Body Acceleration Magnitude- Mean Frequency","Frequency Body Body Accelerometer Jerk Signal Magnitude Mean",
            "Frequency Body Body Accelerometer Jerk Signal Magnitude- Mean Frequency","Frequency Body Body Gyroscope Magnitude Mean",
            "Frequency Body Body Gyroscope Magnitude- Mean Frequency",   "Frequency Body Body Gyroscope Jerk Signal Magnitude Mean",
            "Frequency Body Body Gyroscope Jerk Signal Magnitude- Mean Frequency","angle(Time Body Accelerometer Mean,gravity)",
            "angle(Time Body Accelerometer Jerk SignalMean),gravity Mean)","angle(Time Body Gyroscope Mean,gravity Mean)",
            "angle(Time Body Gyroscope Jerk SignalMean,gravity Mean)","angle(X,gravity Mean)",
            "angle(Y,gravity Mean)","angle(Z,gravity Mean)",
            "Activity","Time Body Accelerometer Standard Deviation-X",
            "Time Body Accelerometer Standard Deviation-Y", "Time Body Accelerometer Standard Deviation-Z",
            "Time Gravity Accelerometer Standard Deviation-X", "Time Gravity Accelerometer Standard Deviation-Y",
            "Time Gravity Accelerometer Standard Deviation-Z", "Time Body Accelerometer Jerk Signal Standard Deviation-X",
            "Time Body Accelerometer Jerk Signal Standard Deviation-Y","Time Body Accelerometer Jerk Signal Standard Deviation-Z",
            "Time Body Gyroscope Standard Deviation-X","Time Body Gyroscope Standard Deviation-Y",
            "Time Body Gyroscope Standard Deviation-Z","Time Body Gyroscope Jerk Signal Standard Deviation-X",
            "Time Body Gyroscope Jerk Signal Standard Deviation-Y","Time Body Gyroscope Jerk Signal Standard Deviation-Z",
            "Time Body Accelerometer Magnitude Standard Deviation","Time Gravity Accelerometer Magnitude Standard Deviation",
            "Time Body Accelerometer Jerk Signal Magnitude Standard Deviation","Time Body Gyroscope Magnitude Standard Deviation",
            "Time Body Gyroscope Jerk Signal Magnitude Standard Deviation","Frequency Body Acceleration Standard Deviation-X",
            "Frequency Body Acceleration Standard Deviation-Y", "Frequency Body Acceleration Standard Deviation-Z",
            "Frequency Body Acceleration Jerk Signals Standard Deviation-X","Frequency Body Acceleration Jerk Signals Standard Deviation-Y",
            "Frequency Body Acceleration Jerk Signals Standard Deviation-Z","Frequency Body Gyroscope Standard Deviation-X",
            "Frequency Body Gyroscope Standard Deviation-Y","Frequency Body Gyroscope Standard Deviation-Z",
            "Frequency Body Acceleration Magnitude Standard Deviation","Frequency Body Body Accelerometer Jerk Signal Magnitude Standard Deviation",
            "Frequency Body Body Gyroscope Magnitude Standard Deviation","Frequency Body Body Gyroscope Jerk Signal Magnitude Standard Deviation")
}