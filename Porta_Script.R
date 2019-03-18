################################### Portabilidad Shiny ###################################

## INDEX

## 1.Preliminar steps
  # 1.1 Data Load
  # 1.2 Library Load
## 2.Server Configuration
  # 2.1 Shinyapps.io login
  # 2.2 Shinyapp.io deployment
## 3.Shiny Application

##########################################################################################

## 1.Preliminar steps
  # 1.1 Data Load
setwd()
Porta.1 = read.csv("Portabilidty_Set.csv")

  # 1.2 Library Load
library(ggplot2)
library(data.table)
library(dplyr)
library(lattice)
library(shiny)
library(rsconnect)

## 2.Shinyapps.io authorization
  # 2.1 Shinyapps.io login
secret = readLines(con = "C:/Users/a1380/Documents/CredencialShiny.txt")
rsconnect::setAccountInfo(name='nspproject',
                          token='84DA9E67B476250E326ED7FAB04F4E9E',
                          secret=secret)
  # 2.2 Shinyapp.io deployment
rsconnect::deployApp("C:/Users/a1380/Desktop/Portability Project")

## 3.Shiny Application

ui <- fluidPage()
server <- function(input, output)
shinyApp(ui=ui, server = server)


