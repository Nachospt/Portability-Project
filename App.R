################################### Portabilidad Shiny ###################################
#### INDEX #### 

## 1.Preliminar steps
# 1.1 Library Load
# 1.2 Data Load
## 2.Server Configuration
# 2.1 Shinyapps.io login
# 2.2 Shinyapp.io deployment
## 3.Data Manipulation
# 3.1 Creating Pr_2 table
# 3.2 Removing self-portabilities
## 4.Shiny Application
# 4.1 Adjustements
# 4.2 ui and server
# 4.3 Running the app


#### 1.Preliminar steps

# 1.1 Library Load
library(ggplot2)
library(data.table)
library(dplyr)
library(reshape)
library(readr)
library(lattice)
library(shiny)
library(shinythemes)
library(rsconnect)
library(DT)
library(scales)

# 1.2 Data Load
setwd("C:\\Users\\a1380\\Desktop\\Portability Project")
Pr_1 = fread("Portability_sim_data.csv")

#### 2.Server Configuration ####
# 2.1 Shinyapps.io login
secret = readLines(con = "C:/Users/a1380/Documents/CredencialShiny.txt")
rsconnect::setAccountInfo(name='nspproject',
                          token='84DA9E67B476250E326ED7FAB04F4E9E',
                          secret=secret)
# 2.2 Shinyapp.io deployment
rsconnect::deployApp("C:/Users/a1380/Desktop/Portability Project")

#### 3.Data Manipulation ####

## 3.1 Creating Pr_2 table, to be used in the visualizations.

Pr_2 = Pr_1

Pr_2$Exportaciones = apply(Pr_1, 1, FUN = function(x) {
  TargetRow = intersect(which(Pr_1$Donante.Grupo == x["Operador.Grupo"]),
                        intersect(which(Pr_1$Operador.Grupo == x["Donante.Grupo"]),
                                  which(Pr_1$ano.mes == x["ano.mes"])))
  Pr_1$Importaciones[TargetRow]
})

## 3.2 Removing self-portabilities
Pr_2 = Pr_2[!which(Pr_2$Donante.Grupo == Pr_2$Operador.Grupo),]

#### 4.Shiny Application ####

## 4.1 Adjustments

options(scipen=999)

zones = data.frame(c(1,1,1,1,2,2,2,2), c(0, 0, 300000, 0, 300000, 300000, 0, 0),
                   c(0, 300000, 300000, 0, 0, 300000, 0, 0),
                   c("#FD625E", "#FD625E", "#FD625E", "#FD625E", "#66CC00", "#66CC00", "#66CC00", "#66CC00"))
colnames(zones) = c("group", "Importaciones", "Exportaciones", "color")

## 4.2 ui and server

ui <- fluidPage(theme = shinytheme("cerulean"),
                
                # App title
                column(width = 8,titlePanel("CdM Portability Spain", windowTitle = "Portabilidad"), offset = 3),
                
                # Sidebar layout with a input and output definitions
                sidebarLayout(
                  
                  # Inputs
                  sidebarPanel( width = 3,
                                h5(imageOutput("Op1", inline = TRUE),
                                   imageOutput("Op2", inline = TRUE),
                                   imageOutput("Op3", inline = TRUE),
                                   imageOutput("Op4", inline = TRUE),
                                   imageOutput("Op5", inline = TRUE)),
                                
                                h3("Selection"),      # Third level header: Selection
                                
                                # Select Operator
                                selectInput(inputId = "Operator", 
                                            label = "Operator:",
                                            choices = c("Vodafone", "Movistar", "Orange", "Masmovil", "Resto"),
                                            selected = "Vodafone"
                                ),
                                
                                # Select Years
                                selectInput(inputId = "MinYear", 
                                            label = "MinimumYear:",
                                            choices = levels(as.factor(Pr_2$ano.mes)),
                                            selected = "1712"
                                ),
                                
                                # Built with Shiny by RStudio
                                br(),
                                h5("Built with",
                                   img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                   "by"),
                                h5("Ignacio Sánchez Puente"),
                                img(src = "image.png", height = "50px")
                  ),
                  
                  # Output:
                  mainPanel(tabsetPanel(type = "tabs",
                                        tabPanel(title = "Overview",
                                                 HTML("In this dashboard we show an advanced visualization on portability operations data.
                                                      Portability data is still being analyzed with tables like this:"),
                                                 tableOutput(outputId = "ClassicTable")
                                        ),
                                        tabPanel(title = "Portability Analysis",
                                                 h5("Waterfall graph", inline = TRUE),
                                                 h5(plotOutput(outputId = "waterfall", height = 180, width = 500)),
                                                 h5("Portability Evolution Map"),
                                                 plotOutput(outputId = "scatterplot")),
                                        tabPanel(title = "Raw table",
                                                 h3("Raw Table"),
                                                 br(),
                                                 DT::dataTableOutput(outputId = "rawtable"), #tableOutput(outputId = "rawtable"),
                                                 downloadButton(outputId = "download_data", label = "Download data"),
                                                 br(),
                                                 br())
                  )
                  )
                )
)

server <- function(input, output) {
  Upd.Pr_1 <- reactive({filter(Pr_1, Operador.Grupo == input$Operator & ano.mes >= input$MinYear)})
  Upd.Pr_2 <- reactive({filter(Pr_2, Operador.Grupo == input$Operator & ano.mes >= input$MinYear)})
  Upd.Pr_Waterfall <- reactive({Pr_Waterfall = as.data.table(Upd.Pr_2())
  Pr_Waterfall = Pr_Waterfall[, list(Import = sum(Importaciones), Export = sum(Exportaciones)), by = list(ano.mes, Operador.Grupo)][,"end":= cumsum(Import - Export)]
  Pr_Waterfall[,"start" := c(0, Pr_Waterfall[1:(.N-2), end], 0)][, "id" := 1:.N][,"sign" := {if(Import - Export >= 0) {"Positive"} else {"Negative"}}, by = id][.N,"sign" := "Net"]
  })
  
  # Classic table
  output$ClassicTable <- renderTable({Pr_2[ano.mes == "1804"]})
  
  output$description <- renderText({
    paste0(nrow(Upd.Pr_1()))})
  
  # Create waterfall object the plotOutput function is expecting
  output$waterfall <- renderPlot({x_breaks = seq(from = 1, to = 1 + (3 * floor(max(Upd.Pr_Waterfall()[,id]) / 3)), by = 3)
  ggplot(Upd.Pr_Waterfall(), aes(id, fill = sign)) + 
    geom_rect(aes(xmin = id - 0.45, xmax = id + 0.45, ymin = end, ymax = start)) +
    scale_x_continuous(name = "Date (YYMM format)", breaks = x_breaks, labels = unique(Upd.Pr_Waterfall()[, ano.mes])[x_breaks]) +
    theme_minimal() +
    scale_fill_manual(values=sapply(levels(as.factor(Upd.Pr_Waterfall()[,sign])), FUN = function(x) switch(x, "Positive" = "#66CC00", "Negative" = "#FD625E", "Net" = "#56B4E9"))) +
    theme(legend.title = element_blank(), title = element_text("Waterfall Evolution Graph"))
  },
  height = 180)
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = Upd.Pr_2(),
           aes(y = Importaciones, x = Exportaciones)) +
      geom_polygon(data = zones, aes(y = Importaciones, x = Exportaciones, group = as.factor(group)), alpha = 0.5, fill = zones$color, color = zones$color, linetype = 0, inherit.aes = FALSE) +
      geom_point(size = 2, aes(colour = Donante.Grupo)) +
      geom_path(aes(color = Donante.Grupo), arrow = arrow(length=unit(0.30,"cm"), type = "closed")) +
      geom_text(aes(label = Upd.Pr_2()[,"ano.mes"], y = Importaciones + 5000), size = 3, color = "grey29") +
      scale_color_manual(values = sapply(levels(as.factor(Upd.Pr_2()[, "Donante.Grupo"])),
                                         function(x) switch(x, "Vodafone" = "#E60000", "Movistar" = "#00B6E8", "Masmovil" = "#FFE500", "Orange" = "#FF9800", "Resto" = "#01B8AA"))) +
      coord_cartesian(xlim = c(0, max(Upd.Pr_2()[,c("Importaciones", "Exportaciones")]) * 1.04), ylim = c(0, max(Upd.Pr_2()[,c("Importaciones", "Exportaciones")]) * 1.04)) +
      theme_minimal()
  })
  
  # Operator images: Selected / Not selected (light version)
  output$Op1 <- renderImage({list(src = if(input$Operator == "Vodafone") {"www/VODAFONE.png"} else {"www/VODAFONE_LIGHT.png"}, contentType = 'image/png', width = 80)}, deleteFile = FALSE)
  output$Op2 <- renderImage({list(src = if(input$Operator == "Movistar") {"www/MOVISTAR.png"} else {"www/MOVISTAR_LIGHT.png"}, contentType = 'image/png', width = 80)}, deleteFile = FALSE)
  output$Op3 <- renderImage({list(src = if(input$Operator == "Orange") {"www/ORANGE.png"} else {"www/ORANGE_LIGHT.png"}, contentType = 'image/png', width = 80)}, deleteFile = FALSE)
  output$Op4 <- renderImage({list(src = if(input$Operator == "Masmovil") {"www/MASMOVIL.png"} else {"www/MASMOVIL_LIGHT.png"}, contentType = 'image/png', width = 80)}, deleteFile = FALSE)
  output$Op5 <- renderImage({list(src = if(input$Operator == "Resto") {"www/RESTO.png"} else {"www/RESTO_LIGHT.png"}, contentType = 'image/png', width = 80)}, deleteFile = FALSE)
  
  # Create rawtable object the plotOutput function is expecting
  output$rawtable <- DT::renderDataTable({DT::datatable(Upd.Pr_1())})
  
  # Download Button
  filetype = "csv"
  output$download_data <- downloadHandler(
    filename = function() {
      paste("test.csv")
    },
    content = function(file) {
      if(filetype == "csv"){
        write_csv(Upd.Pr_1(), path = file)
      }
    }
  )
}

## 4.3 Running the app

shinyApp(ui = ui, server = server)
runApp()
runApp("C:\\Users\\a1380\\Desktop\\Portability Project")