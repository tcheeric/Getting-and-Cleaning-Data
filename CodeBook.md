	- Transformations
		. Load the required libraries (readr, dplyr, reshape2)
		. Load the train data set:
			 Load the activity values from y_train.txt
			 Load the subjects from subject_train.txt
			 Load the features from X_train.xml
		. Load teh test data set:
			 Load the activity values from y_test.txt
			 Load the subjects from subject_test.txt
			 Load the features from X_test.xml
		. Merge both test and train data set in one single target data set
		. Drop all columns that don't have the words "mean" or "contain" in their headers
		. Compute the average of each variable for each activity and each subject.
		
	- Variables
		-- Activity
			Possible values are:
			WALKING
			WALKING_UPSTAIRS
			WALKING_DOWNSTAIRS
			SITTING
			STANDING
			LAYING
		-- Time Body Accelerometer Mean-X
		-- Time Body Accelerometer Mean-Y
		-- Time Body Accelerometer Mean-Z
        -- Time Gravity Accelerometer Mean-X
		-- Time Gravity Accelerometer Mean-Y
    	-- Time Gravity Accelerometer Mean-Z
		-- Time Body Accelerometer Jerk Signal Mean-X
		-- Time Body Accelerometer Jerk Signal Mean-Y
		-- Time Body Accelerometer Jerk Signal Mean-Z
		-- Time Body Gyroscope Mean-X
		-- Time Body Gyroscope Mean-Y
		-- Time Body Gyroscope Mean-Z
		-- Time Body Gyroscope Jerk Signal Mean-X
		-- Time Body Gyroscope Jerk Signal Mean-Y
		-- Time Body Gyroscope Jerk Signal Mean-Z
		-- Time Body Accelerometer Magnitude Mean
		-- Time Gravity Accelerometer Magnitude Mean
		-- Time Body Accelerometer Jerk Signal Magnitude Mean
		-- Time Body Gyroscope Magnitude Mean
		-- Time Body Gyroscope Jerk Signal Magnitude Mean
		-- Frequency Body Acceleration Mean-X
		-- Frequency Body Acceleration Mean-Y
		-- Frequency Body Acceleration Mean-Z
		-- Frequency Body Acceleration- Mean Frequency-X
		-- Frequency Body Acceleration- Mean Frequency-Y
		-- Frequency Body Acceleration- Mean Frequency-Z
		-- Frequency Body Acceleration Jerk Signals Mean-X
		-- Frequency Body Acceleration Jerk Signals Mean-Y
		-- Frequency Body Acceleration Jerk Signals Mean-Z
		-- Frequency Body Acceleration Jerk Signals- Mean Frequency-X
		-- Frequency Body Acceleration Jerk Signals- Mean Frequency-Y
		-- Frequency Body Acceleration Jerk Signals- Mean Frequency-Z
		-- Frequency Body Gyroscope Mean-X
		-- Frequency Body Gyroscope Mean-Y
		-- Frequency Body Gyroscope Mean-Z
		-- Frequency Body Gyroscope- Mean Frequency-X
		-- Frequency Body Gyroscope- Mean Frequency-Y
		-- Frequency Body Gyroscope- Mean Frequency-Z
		-- Frequency Body Acceleration Magnitude Mean
		-- Frequency Body Acceleration Magnitude- Mean Frequency
		-- Frequency Body Body Accelerometer Jerk Signal Magnitude Mean
		-- Frequency Body Body Accelerometer Jerk Signal Magnitude- Mean Frequency
		-- Frequency Body Body Gyroscope Magnitude Mean
		-- Frequency Body Body Gyroscope Magnitude- Mean Frequency
		-- Frequency Body Body Gyroscope Jerk Signal Magnitude Mean
		-- Frequency Body Body Gyroscope Jerk Signal Magnitude- Mean Frequency
		-- angle(Time Body Accelerometer Mean
		-- gravity)
		-- angle(Time Body Accelerometer Jerk SignalMean)
		-- gravity Mean)
		-- angle(Time Body Gyroscope Mean
		-- gravity Mean)
		-- angle(Time Body Gyroscope Jerk SignalMean
		-- gravity Mean)
		-- angle(X
		-- gravity Mean)
		-- angle(Y
		-- gravity Mean)
		-- angle(Z
		-- gravity Mean)
		-- Activity
		-- Time Body Accelerometer Standard Deviation-X
		-- Time Body Accelerometer Standard Deviation-Y
		-- Time Body Accelerometer Standard Deviation-Z
		-- Time Gravity Accelerometer Standard Deviation-X
		-- Time Gravity Accelerometer Standard Deviation-Y
		-- Time Gravity Accelerometer Standard Deviation-Z
		-- Time Body Accelerometer Jerk Signal Standard Deviation-X
		-- Time Body Accelerometer Jerk Signal Standard Deviation-Y
		-- Time Body Accelerometer Jerk Signal Standard Deviation-Z
		-- Time Body Gyroscope Standard Deviation-X
		-- Time Body Gyroscope Standard Deviation-Y
		-- Time Body Gyroscope Standard Deviation-Z
		-- Time Body Gyroscope Jerk Signal Standard Deviation-X
		-- Time Body Gyroscope Jerk Signal Standard Deviation-Y
		-- Time Body Gyroscope Jerk Signal Standard Deviation-Z
		-- Time Body Accelerometer Magnitude Standard Deviation
		-- Time Gravity Accelerometer Magnitude Standard Deviation
		-- Time Body Accelerometer Jerk Signal Magnitude Standard Deviation
		-- Time Body Gyroscope Magnitude Standard Deviation
		-- Time Body Gyroscope Jerk Signal Magnitude Standard Deviation
		-- Frequency Body Acceleration Standard Deviation-X
		-- Frequency Body Acceleration Standard Deviation-Y
		-- Frequency Body Acceleration Standard Deviation-Z
		-- Frequency Body Acceleration Jerk Signals Standard Deviation-X
		-- Frequency Body Acceleration Jerk Signals Standard Deviation-Y
		-- Frequency Body Acceleration Jerk Signals Standard Deviation-Z
		-- Frequency Body Gyroscope Standard Deviation-X
		-- Frequency Body Gyroscope Standard Deviation-Y
		-- Frequency Body Gyroscope Standard Deviation-Z
		-- Frequency Body Acceleration Magnitude Standard Deviation
		-- Frequency Body Body Accelerometer Jerk Signal Magnitude Standard Deviation
		-- Frequency Body Body Gyroscope Magnitude Standard Deviation
		-- Frequency Body Body Gyroscope Jerk Signal Magnitude Standard Deviation
