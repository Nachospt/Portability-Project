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