server <- function(input, output) {
  Upd.Pr_1 <- reactive({filter(Pr_1, Operador.Grupo == input$Operator & ano.mes >= input$MinYear)})
  Upd.Pr_2 <- reactive({filter(Pr_2, Operador.Grupo == input$Operator & ano.mes >= input$MinYear)})
  Upd.Pr_Waterfall <- reactive({Pr_Waterfall = as.data.table(Upd.Pr_2())
  Pr_Waterfall = Pr_Waterfall[, list(Import = sum(Importaciones), Export = sum(Exportaciones)), by = list(ano.mes, Operador.Grupo)][,"end":= cumsum(Import - Export)]
  Pr_Waterfall[,"start" := c(0, Pr_Waterfall[1:(.N-2), end], 0)][, "id" := 1:.N][,"sign" := {if(Import - Export >= 0) {"b"} else {"a"}}, by = id][c(1, .N),"sign" := "c"]
    })
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
  
  output$description <- renderText({
    paste0(nrow(Upd.Pr_1()))})
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = Upd.Pr_2(),
           aes(y = Importaciones, x = Exportaciones)) +
      geom_polygon(data = zones, aes(y = Importaciones, x = Exportaciones, group = as.factor(group)), alpha = 0.5, fill = zones$color, color = zones$color, linetype = 0, inherit.aes = FALSE) +
      geom_point(size = 2, aes(colour = Donante.Grupo)) +
      geom_line(aes(color = Donante.Grupo), arrow = arrow(length=unit(0.30,"cm"), type = "closed")) +
      geom_text(aes(label = Upd.Pr_2()[,"ano.mes"], y = Importaciones + 5000), size = 3, color = "grey29") +
      scale_color_manual(values = sapply(levels(as.factor(Upd.Pr_2()[, "Donante.Grupo"])),
                                         function(x) switch(x, "Vodafone" = "#E60000", "Movistar" = "#00B6E8", "Masmovil" = "#FFE500", "Orange" = "#FF9800", "Resto" = "#01B8AA"))) +
      coord_cartesian(xlim = c(0, max(Upd.Pr_2()[,c("Importaciones", "Exportaciones")]) * 1.04), ylim = c(0, max(Upd.Pr_2()[,c("Importaciones", "Exportaciones")]) * 1.04))
  })
  
  # Create waterfall object the plotOutput function is expecting
  output$waterfall <- renderPlot({ggplot(Upd.Pr_Waterfall(), aes(id, fill = sign)) + 
      geom_rect(aes(xmin = id - 0.45, xmax = id + 0.45, ymin = end, ymax = start))
  })
  
  #Operator images
  output$Op1 <- renderImage({list(src = if(input$Operator == "Vodafone") {"www/VODAFONE.png"} else {"www/VODAFONE_LIGHT.png"}, contentType = 'image/png', width = 100)}, deleteFile = FALSE)
  output$Op2 <- renderImage({list(src = if(input$Operator == "Movistar") {"www/MOVISTAR.png"} else {"www/MOVISTAR_LIGHT.png"}, contentType = 'image/png', width = 100)}, deleteFile = FALSE)
  output$Op3 <- renderImage({list(src = if(input$Operator == "Orange") {"www/ORANGE.png"} else {"www/ORANGE_LIGHT.png"}, contentType = 'image/png', width = 100)}, deleteFile = FALSE)
  output$Op4 <- renderImage({list(src = if(input$Operator == "Masmovil") {"www/MASMOVIL.png"} else {"www/MASMOVIL_LIGHT.png"}, contentType = 'image/png', width = 100)}, deleteFile = FALSE)
  output$Op5 <- renderImage({list(src = if(input$Operator == "Resto") {"www/RESTO.png"} else {"www/RESTO_LIGHT.png"}, contentType = 'image/png', width = 100)}, deleteFile = FALSE)
  
  #Create rawtable object the plotOutput function is expecting
  output$rawtable <- DT::renderDataTable({DT::datatable(Upd.Pr_1(), options = list(pageLength = 10))}) #renderTable({Pr_1})
}