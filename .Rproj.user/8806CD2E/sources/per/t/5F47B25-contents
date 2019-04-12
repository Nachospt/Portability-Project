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
                                                 br(),
                                                 HTML("In this dashboard we show an advanced visualization on portability operations data.
                                                      Portability data is still being analyzed with tables like this:"),
                                                 br(),
                                                 tableOutput(outputId = "ClassicTable")
                                                 ),
                                        tabPanel(title = "General CdM",
                                                 h5("Waterfall graph", inline = TRUE),
                                                 h5(plotOutput(outputId = "waterfall", height = 180, width = 500))),
                                        tabPanel(title = "Portability Analysis",
                                                 h5("Portability Evolution Map"),
                                                 plotOutput(outputId = "scatterplot", height = 500)),
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