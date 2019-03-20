server <- function(input, output) {
  Upd.Porta.1 <- reactive({filter(Porta.1, Operador.Grupo == input$Operator & ano.mes >= input$MinYear)}) 
  Upd.Porta.2 <- reactive({filter(Porta.2, Operador.Grupo == input$Operator & ano.mes >= input$MinYear)}) 
  
  filetype = "csv"
  output$download_data <- downloadHandler(
    filename = function() {
      paste("test.csv")
    },
    content = function(file) {
      if(filetype == "csv"){
        write_csv(Upd.Porta.1(), path = file)
      }
    }
  )
  
  output$description <- renderText({
    paste0(nrow(Upd.Porta.1()))})
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = Upd.Porta.2(),
           aes(y = Importaciones, x = Exportaciones)) +
      geom_polygon(data = zones, aes(y = Importaciones, x = Exportaciones, group = as.factor(group)), alpha = 0.5, fill = zones$color, color = zones$color, linetype = 0, inherit.aes = FALSE) +
      geom_point(size = 2, aes(colour = Donante.Grupo)) +
      geom_line(aes(color = Donante.Grupo), arrow = arrow(length=unit(0.30,"cm"), type = "closed")) +
      geom_text(aes(label = Upd.Porta.2()[,"ano.mes"], y = Importaciones + 5000), size = 3, color = "grey29") +
      scale_color_manual(values = sapply(levels(as.factor(Upd.Porta.2()[, "Donante.Grupo"])), function(x) switch(x, "Vodafone" = "#E60000", "Movistar" = "#00B6E8", "Masmovil" = "#FFE500", "Orange" = "#FF9800", "Resto" = "#01B8AA")))
  })
  
  # Create rawtable object the plotOutput function is expecting
  output$rawtable <- DT::renderDataTable({DT::datatable(Upd.Porta.1(), options = list(pageLength = 10))})#renderTable({Porta.1})
}