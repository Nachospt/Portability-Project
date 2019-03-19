################################### Portabilidad Shiny ###################################
#### INDEX #### 

## 1.Preliminar steps
  # 1.1 Data Load
  # 1.2 Library Load
## 2.Server Configuration
  # 2.1 Shinyapps.io login
  # 2.2 Shinyapp.io deployment
## 3.Shiny Application



#### 1.Preliminar steps
  # 1.1 Data Load
setwd("C:\\Users\\a1380\\Desktop\\Portability Project")
Porta.1 = read.csv("C:\\Users\\a1380\\Desktop\\data.csv")

  # 1.2 Library Load
library(ggplot2)
library(data.table)
library(dplyr)
library(lattice)
library(shiny)
library(shinythemes)
library(rsconnect)

#### 2.Server Configuration ####
  # 2.1 Shinyapps.io login
secret = readLines(con = "C:/Users/a1380/Documents/CredencialShiny.txt")
rsconnect::setAccountInfo(name='nspproject',
                          token='84DA9E67B476250E326ED7FAB04F4E9E',
                          secret=secret)
  # 2.2 Shinyapp.io deployment
rsconnect::deployApp("C:/Users/a1380/Desktop/Portability Project")

#### 3.Shiny Application ####

ui <- fluidPage(theme = shinytheme("cerulean"),
  
  # App title
  titlePanel("CdM Portability Spain", windowTitle = "Portabilidad"),
  
  fluidRow(
    # Select variable for y-axis 
    selectInput(inputId = "Operator", 
                label = "Operator:",
                choices = c("Vodafone" = "Vodafone", 
                            "Movistar" = "Movistar", 
                            "Orange" = "Orange", 
                            "Masmovil" = "Masmovil", 
                            "Resto" = "Resto"), 
                selected = "Vodafone")
    
  ),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      h3("Selection"),      # Third level header: Selection
      
      
      h3("Subsetting"),    # Third level header: Subsetting
      
      # Built with Shiny by RStudio
      br(),
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "by",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
         "."),
      
      img(src = "/image.jpg", height = "50px")
    ),
    
      # Output:
      mainPanel(
        HTML("lo que sea")
    )
  )
)

server <- function(input, output) {
  
}

## For build testing
shinyApp(ui=ui, server = server)
runApp()
runApp("C:\\Users\\a1380\\Desktop\\Portability Project")