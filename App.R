################################### Portabilidad Shiny ###################################
#### INDEX #### 

## 1.Preliminar steps
  # 1.1 Data Load
  # 1.2 Library Load
## 2.Server Configuration
  # 2.1 Shinyapps.io login
  # 2.2 Shinyapp.io deployment
## 3.Data Manipulation
  # 3.1 Creating Porta.2 table
  # 3.2 Removing self-portabilities
## 4.Shiny Application



#### 1.Preliminar steps
  # 1.1 Data Load
setwd("C:\\Users\\a1380\\Desktop\\Portability Project")
Porta.1 = read.csv("C:\\Users\\a1380\\Desktop\\data.csv")

  # 1.2 Library Load
library(ggplot2)
library(data.table)
library(dplyr)
library(readr)
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


#### 3.Data Manipulation ####

names(Porta.1)[1] = "ano.mes"


  ## 3.1 Creating Porta.2 table

Porta.2 = Porta.1

Porta.2$Total.Receptor = apply(Porta.1, 1, FUN = function(x) {
  TargetRow = intersect(which(Porta.1$Donante.Grupo == x["Operador.Grupo"]), intersect(which(Porta.1$Operador.Grupo == x["Donante.Grupo"]), which(Porta.1$ano.mes == x["ano.mes"])))
  Porta.1$Importaciones[TargetRow]
})

  ## 3.2 Removing self-portabilities
Porta.2 = Porta.2[-which(Porta.2$Donante.Grupo == Porta.2$Operador.Grupo),]

#### 4.Shiny Application ####

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
      
      # Create scatterplot object the plotOutput function is expecting 
      output$scatterplot <- renderPlot({
        ggplot(data = Porta.1,
               aes(y = Structure.Cost, x = Land.Value)) +
          geom_point()
      }),
      
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
        HTML("Here goes the content"),
        
        downloadButton(outputId = "download_data", label = "Download data")
    )
  )
)

server <- function(input, output) {
  filetype = "csv"
  output$download_data <- downloadHandler(
    filename = function() {
      paste("test.csv")
    },
    content = function(file) {
      if(filetype == "csv"){
        write_csv(Porta.1, path = file)
      }
    }
  )
}

## For build testing
shinyApp(ui=ui, server = server)
runApp()
runApp("C:\\Users\\a1380\\Desktop\\Portability Project")