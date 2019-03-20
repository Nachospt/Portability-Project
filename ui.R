ui <- fluidPage(theme = shinytheme("cerulean"),
                
                # App title
                column(width = 8,titlePanel("CdM Portability Spain", windowTitle = "Portabilidad"), offset = 3),
                
                # Sidebar layout with a input and output definitions
                sidebarLayout(
                  
                  # Inputs
                  sidebarPanel( width = 3,
                                
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
                                            choices = levels(as.factor(Porta.2$ano.mes)),
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
                                                 h3("Overview"),
                                                 br(),
                                                 HTML("Here goes the content")),
                                        tabPanel(title = "Portability Graph",
                                                 h3("Portability graph"),
                                                 br(),
                                                 plotOutput(outputId = "scatterplot"),
                                                 textOutput(outputId = "description")),
                                        tabPanel(title = "Raw table",
                                                 h3("Raw Table"),
                                                 br(),
                                                 DT::dataTableOutput(outputId = "rawtable"), #tableOutput(outputId = "rawtable"),
                                                 downloadButton(outputId = "download_data", label = "Download data"))
                  )
                  )
                )
)