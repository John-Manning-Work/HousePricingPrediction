# IOWA HOUSE PREDICTION
 R Project - John Manning
 



# Kanban Board

Click here [Kanban Board](https://trello.com/b/75AZHajd/qa-project)
This link will send to my kanban board for this project. It was done through **Trello**
This will show you the **progress** made throughout the project

## How To Install

In order to get this project working you will need to do the following:

 - Download [R](https://cran.r-project.org/) as the project is built using the R language. This will allow you to run this application
 - Download [R studio](https://www.rstudio.com/products/rstudio/download/). This is an editing software and allows you to run R scripts directly from this program without need of a command prompt etc...
 - Download [MySQL](https://dev.mysql.com/downloads/windows/). This is the database program used in this project and will be needed to access the data stored inside the R file

## How to get application running

In order to use the R application you need to have downloaded all the items inside of the "**How to instal**l" area. Once this has been completed you will need to download the following packages
**install.packages("ggplot2")**
**install.packages("caret")**
**install.packages("e1071")**
**install.packages("statsr")**
**install.packages("dplyr")**
**install.packages("BAS")**
**install.packages("corrplot")**
**install.packages("GGally")**
**install.packages("MASS")**
Once packages has been installed the librarys will automatically run when the program starts
You can start the application by running the UI.R file. This will display a seperate window for you to interact with.


## How to use

Once all the previous steps have been done you should have a working program.
To use it you need to change the options in the selected boxes provided on the website to a desired range of rooms and quality that has been arranged.
This will affect the price of that house and will display a prediction sale value on the screen based on the items selected. This prediction comes from the machine learning model within the R file.

