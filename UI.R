#========================================================================(INSTALL PACKAGES)=================================================================#|
#Adding the shiny library for the UI                                                                                                                        #|
                                                                                                                                                            #|
#install.packages("shiny)                                                                                                                                   #|
                                                                                                                                                            #|
#=========================================================================(SETUP LIBRARYS)==================================================================#|
#Adding the library into the code for the UI                                                                                                                #|
                                                                                                                                                            #|
library(shiny)                                                                                                                                              #|
                                                                                                                                                            #|
#======================================================================(SETTING WD AND FILES)===============================================================#|
#Adding in options 9999 so all numerics display properly                                                                                                    #|
                                                                                                                                                            #|
options(scipen = 9999)                                                                                                                                      #|
                                                                                                                                                            #|
#Setting up workspace to Project location for files                                                                                                         #|
                                                                                                                                                            #|
ProjectAmend <- read.table("TrainNormal.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")                                                           #|
                                                                                                                                                            #|
#========================================================================(CREATING THE UI)==================================================================#|
#Creating the UI & Panel Tabs. Implementing the prediction into tab Main Prediction.                                                                        #|
#Adding the prediction output & input for area, overall cond, year, air con, kitchen qual and fireplaces                                                    #|
                                                                                                                                                            #|
ui <- fluidPage(                                                                                                                                            #|
  navbarPage("My Project",                                                                                                                                  #|
                              tabPanel("Main Prediction",                                                                                                   #|
                                                                                                                                                            #|
                                       textOutput(outputId = "Predict"),                                                                                    #|
                                                                                                                                                            #|
                                       numericInput(inputId = "area", label = "Lot Area", min = 0, value = 50000, step = 1, max = 500000),                  #|
                                       selectInput(inputId = "oacond", label = "OverAll Condition", choices = c(10,9,8,7,6,5,4,3,2,1)),                     #|
                                                                                                                                                            #|
                                       numericInput(inputId = "year", label = "Year Built", min = 1750, value = 1750, step = 1, max = 2009),                #|
                                       selectInput(inputId = "ac", label = "Air conditioning", choices = c("Yes","No")),                                    #|
                                       selectInput(inputId = "kitchenqual", label = "Kitchen Quality", choices = c("Excellent","Good","Typical/Average",    #|
                                                                                                                   "Fair","Poor")),                         #|
                                       sliderInput(inputId = "fireplace", label = "Number Of Fireplaces", min = 0, value = 0, step = 1, max = 3)),          #|
                                                                                                                                                            #|
                                                                                                                                                            #|
                              tabPanel("Graph"),                                                                                                            #|
                                                                                                                                                            #|
                              navbarMenu("More",                                                                                                            #|
                                         tabPanel("More1"),                                                                                                 #|
                                         tabPanel("More2"))                                                                                                 #|
))                                                                                                                                                          #|
                                                                                                                                                            #|
#======================================================================(ADDING LOGIC FUNCTION)==============================================================#|
#Creating a logic function for input. Sorting out data into correct values                                                                                  #|
                                                                                                                                                            #|
Logic <- function(input) {                                                                                                                                  #|
  SalePrice = 0                                                                                                                                             #| 
  LotArea <- (as.numeric(input$area))                                                                                                                       #|
  OverallCond <- (as.numeric(input$oacond))                                                                                                                 #|
  YearBuilt <- (as.numeric(input$year))                                                                                                                     #|
                                                                                                                                                            #|
  CentralAir <- input$ac                                                                                                                                    #|
  if (CentralAir == "Yes"){                                                                                                                                 #|
    CentralAir <- "Y"                                                                                                                                       #|
  }else if(CentralAir == "No"){                                                                                                                             #|
    CentralAir <- "N"                                                                                                                                       #|
  }                                                                                                                                                         #|
  KitchenAbvGr = 1                                                                                                                                          #|
  KitchenQual <- input$kitchenqual                                                                                                                          #|
  if (KitchenQual == "Excellent"){                                                                                                                          #|
    KitchenQual <- "Ex"                                                                                                                                     #|
  }else if(KitchenQual == "Good"){                                                                                                                          #|
    KitchenQual <- "Gd"                                                                                                                                     #|
  }else if (KitchenQual == "Typical/Average"){                                                                                                              #|
    KitchenQual <- "TA"                                                                                                                                     #|
  }else if (KitchenQual == "Fair"){                                                                                                                         #|
    KitchenQual <- "Fa"                                                                                                                                     #|
  }else if (KitchenQual == "Poor"){                                                                                                                         #|
    KitchenQual <- "Po"                                                                                                                                     #|
  }                                                                                                                                                         #|
                                                                                                                                                            #|
  Fireplaces <- (as.numeric(input$fireplace))                                                                                                               #|
                                                                                                                                                            #|
#=======================================================================(SAVING A DATAFRAME)================================================================#|
#Saving the inputs into a dataframe called TrainNormal2                                                                                                     #|
                                                                                                                                                            #|
TrainNormal2 <- data.frame(SalePrice,LotArea,OverallCond,YearBuilt,CentralAir,KitchenAbvGr, KitchenQual,Fireplaces)                                         #|
                                                                                                                                                            #|
#Writing the dataframe to a document                                                                                                                        #|
                                                                                                                                                            #|
write.csv(TrainNormal2, file = "TrainNormal2.csv")                                                                                                          #|
                                                                                                                                                            #|
#Sourcing the project file to create the prediction                                                                                                         #|
                                                                                                                                                            #|
source("App.R")                                                                                                                                             #|
                                                                                                                                                            #|
#Reading back in the amended file price                                                                                                                     #| 
result <- read.table(file = "resdata.csv", header = TRUE, sep = ",")                                                                                        #|
resdata <- round((as.numeric(result[1,2])), digits = 2)                                                                                                     #|
print(resdata)                                                                                                                                              #|
return(resdata)                                                                                                                                             #|
                                                                                                                                                            #|
}                                                                                                                                                           #|
                                                                                                                                                            #|
#========================================================================(OUTPUTTING PREDCIT)===============================================================#|
#Setting a function called server & outputting the prediction                                                                                               #|
                                                                                                                                                            #|
server <- function(input, output){                                                                                                                          #|
output$Predict <- renderText({                                                                                                                              #|
  paste("Predicted Value", Logic(input))                                                                                                                    #|
})}                                                                                                                                                         #|
                                                                                                                                                            #|
#=======================================================================(SETTING UP SHINYAPP)===============================================================#|
#Setting up shiny app                                                                                                                                       #|
                                                                                                                                                            #|
shinyApp(ui = ui, server = server)                                                                                                                          #|
                                                                                                                                                            #|
#=======================================================================(CURRENT NOT USED CODE)=============================================================#|
                                                                                                                                                            #|
# lotValue <- input$beds*100000                                                                                                                             #|
# conditionValue <- input$toilets*50000                                                                                                                     #|
# yearValue <- (as.numeric(input$stories)-1)*50000                                                                                                          #|
# value <- (bedsValue + toiletsValue + storiesValue)                                                                                                        #|
# return(value)                                                                                                                                             #|
# rm(list = ls())                                                                                                                                           #|
# library(shiny)                                                                                                                                            #|
# Logged = FALSE;                                                                                                                                           #|
# my_username <- "test"                                                                                                                                     #|
# my_password <- "test"                                                                                                                                     #|
# my_                                                                                                                                                       #|
# ui1 <- function(){                                                                                                                                        #|
#   tagList(                                                                                                                                                #|
#     div(id = "login",                                                                                                                                     #|
#         wellPanel(textInput("userName", "Username"),                                                                                                      #|
#                   passwordInput("passwd", "Password"),                                                                                                    #|
#                   br(),actionButton("Login", "Log in"))),                                                                                                 #|
#     tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}"#|
#   )}                                                                                                                                                      #|
# ui2 <- function(){tagList(tabPanel("Test"))}                                                                                                              #|
# ui = (htmlOutput("page"))                                                                                                                                 #|
# server = (function(input, output,session) {                                                                                                               #|
#   USER <- reactiveValues(Logged = Logged)                                                                                                                 #|
#   observe({                                                                                                                                               #|
#     if (USER$Logged == FALSE) {                                                                                                                           #|
#       if (!is.null(input$Login)) {                                                                                                                        #|
#         if (input$Login > 0) {                                                                                                                            #|
#           Username <- isolate(input$userName)                                                                                                             #|
#           Password <- isolate(input$passwd)                                                                                                               #|
#           Id.username <- which(my_username == Username)                                                                                                   #|
#           Id.password <- which(my_password == Password)                                                                                                   #|
#           if (length(Id.username) > 0 & length(Id.password) > 0) {                                                                                        #|
#             if (Id.username == Id.password) {                                                                                                             #|
#               USER$Logged <- TRUE                                                                                                                         #|
#             }                                                                                                                                             #|
#           }                                                                                                                                               #|
#         }                                                                                                                                                 #|
#       }                                                                                                                                                   #|
#     }                                                                                                                                                     #|
#   })                                                                                                                                                      #|
#   observe({                                                                                                                                               #|
#     if (USER$Logged == FALSE) {                                                                                                                           #|
#                                                                                                                                                           #|
#       output$page <- renderUI({                                                                                                                           #|
#         div(class="outer",do.call(bootstrapPage,c("",ui1())))                                                                                             #|
#       })                                                                                                                                                  #|
#     }                                                                                                                                                     #|
#     if (USER$Logged == TRUE)                                                                                                                              #|
#     {                                                                                                                                                     #|
#       output$page <- renderUI({                                                                                                                           #|
#         div(class="outer",do.call(navbarPage,c(inverse=TRUE,title = "Contratulations you got in!",ui2())))                                                #|
#       })                                                                                                                                                  #|
#       print(ui)                                                                                                                                           #|
#     }                                                                                                                                                     #|
#   })                                                                                                                                                      #|
# })                                                                                                                                                        #|
# runApp(list(ui = ui, server = server))                                                                                                                    #|
#if (interactive()) {                                                                                                                                       #|
#  ui <- fluidPage(                                                                                                                                         #|
#    passwordInput("password", "Password:"),                                                                                                                #|
#    actionButton("go", "Go"),                                                                                                                              #|
#    verbatimTextOutput("value")                                                                                                                            #|
#  )                                                                                                                                                        #|
#}                                                                                                                                                          #|
#ui <- navbarPage("My Project",                                                                                                                             #|
#                 tabPanel("Graph 1",                                                                                                                       #|
#                          titlePanel(print("QA Individual project")),                                                                                      #|
#                          passwordInput(pass, Password)),                                                                                                  #|
#                 tabPanel("Graph 2",                                                                                                                       #|
#                          titlePanel(print("QA"))),                                                                                                        #|
#                 navbarMenu("More EG",                                                                                                                     #|
#                            tabPanel("More1",                                                                                                              #|
#                                     titlePanel(print("Individual"))),                                                                                     #|
#                            tabPanel("More2",                                                                                                              #|
#                                     titlePanel(print("project")))                                                                                         #|
#                                                )                                                                                                          #|
#                                              )                                                                                                            #|
#server <- function(input, output){}                                                                                                                        #|
#shinyApp(ui = ui, server = server)                                                                                                                         #|
# #install.packages("shiny)                                                                                                                                 #|
# library(shiny)                                                                                                                                            #|
# options(scipen = 9999)                                                                                                                                    #|
# ui <- fluidPage(                                                                                                                                          #|
#   titlePanel(print("QA Individual project")),                                                                                                             #|
#   sidebarLayout(                                                                                                                                          #|
#   sidebarPanel(),                                                                                                                                         #|
#   mainPanel()                                                                                                                                             #|
#   )                                                                                                                                                       #|
# )                                                                                                                                                         #|
# server <- function(input, output){}                                                                                                                       #|
# shinyApp(ui = ui, server = server)                                                                                                                        #|
#sidebarLayout(                                                                                                                                             #|
#  sidebarPanel(),                                                                                                                                          #|
#  mainPanel( ))                                                                                                                                            #|
                                                                                                                                                            #|
#===========================================================================================================================================================#|