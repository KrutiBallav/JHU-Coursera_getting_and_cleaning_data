# 📖 Project Code Book

**Author**: Kruti Ballav  
This document provides a summary of the data, script, and variables used in the project.

---

## 📂 Data Access

1. **Download the Data**:
   - Get the dataset from this link: [Human Activity Recognition Using Smartphones Data Set](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones).
   - Extract the files into your working directory.

2. **Run the Script**:
   - Use R or RStudio to run the `run_analysis.R` script. This will process the data and generate the tidy dataset.

---

## 📊 About the Data

The data comes from the **Human Activity Recognition Using Smartphones Data Set**. It contains measurements from the accelerometers of Samsung Galaxy S smartphones.  
You can find the full dataset description here: [Human Activity Recognition Using Smartphones Data Set](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones).

---

## 🔧 About the Script (`run_analysis.R`)

The script performs the following key steps:

1. **Reading and Merging Data**:
   - Reads the training and testing datasets.
   - Merges them into one dataset.

2. **Extracting Mean and Standard Deviation**:
   - Selects only the columns related to the mean and standard deviation of each measurement.

3. **Adding Descriptive Activity Names**:
   - Assigns descriptive names to each activity (like "Walking" or "Sitting").

4. **Labeling with Descriptive Variable Names**:
   - Appropriately labels the columns with clear, descriptive names.

5. **Creating the Tidy Dataset**:
   - Creates a tidy dataset with the average of each variable for each activity and subject.
   - Saves the tidy dataset to a text file (`tidy_data.txt`).

---

## 📝 Variables Overview

- **`x_train`**, **`y_train`**, **`x_test`**, **`y_test`**, **`subject_train`**, **`subject_test`**: These variables hold the original data from the downloaded files.
- **`x_data`**, **`y_data`**, **`subject_data`**: Merged datasets used for analysis.
- **`features`**: Contains the names for the variables in `x_data`, which are applied to the dataset for better clarity.

---

This CodeBook provides an overview of the data processing steps and the variables used. It should help you understand how the raw data is transformed into the tidy dataset. 

Feel free to refer back to this document as needed!
