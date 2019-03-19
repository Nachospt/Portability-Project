server <- function(input, output) {
  filetype = "csv"
  output$download_data <- downloadHandler(
    filename = function() {
      paste("test.csv")
    },
    content = function(file) { 
      if(input$filetype == "csv"){ 
        write_csv(Porta.1, path = file) 
      }
    }
  )
}