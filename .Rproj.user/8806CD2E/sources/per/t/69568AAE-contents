server <- function(input, output) {
  Upd.Pr_1 <- reactive({filter(Pr_1, Operador.Grupo == input$Operator & ano.mes >= input$MinYear)})
  Upd.Pr_2 <- reactive({filter(Pr_2, Operador.Grupo == input$Operator & ano.mes >= input$MinYear)})
  
  Upd.Pr_ClassicTable <- reactive({Pr_TempTable = as.data.table(filter(Pr_1, ano.mes == input$MinYear))
  Pr_ClassicTable = as.data.table(matrix(rep(0,25),5,5))
  colnames(Pr_ClassicTable) = c("Movistar", "Vodafone", "Orange", "Masmovil", "Resto")
  rownames(Pr_ClassicTable) = c("Movistar", "Vodafone", "Orange", "Masmovil", "Resto")
  for (i in 1:ncol(Pr_ClassicTable)) {
    for (j in 1:ncol(Pr_ClassicTable)) {
      Pr_ClassicTable[i,j] = if (i > j) { Pr_TempTable[Operador.Grupo == rownames(Pr_ClassicTable)[i] & Donante.Grupo == rownames(Pr_ClassicTable)[j], Importaciones] }
      else if (i < j) { Pr_TempTable[Operador.Grupo == rownames(Pr_ClassicTable)[j] & Donante.Grupo == rownames(Pr_ClassicTable)[i], Importaciones] }
      else { Pr_TempTable[Operador.Grupo == rownames(Pr_ClassicTable)[i] & Donante.Grupo == rownames(Pr_ClassicTable)[i], Importaciones]
        }
    }
  }
  Pr_ClassicTable
  })
  
  Upd.Pr_Waterfall <- reactive({Pr_Waterfall = as.data.table(Upd.Pr_2())
  Pr_Waterfall = Pr_Waterfall[, list(Import = sum(Importaciones), Export = sum(Exportaciones)), by = list(ano.mes, Operador.Grupo)][,"end":= cumsum(Import - Export)]
  Pr_Waterfall[,"start" := c(0, Pr_Waterfall[1:(.N-2), end], 0)][, "id" := 1:.N][,"sign" := {if(Import - Export >= 0) {"Positive"} else {"Negative"}}, by = id][.N,"sign" := "Net"]
    })
  
  # Classic table
  output$ClassicTable <- renderTable({Upd.Pr_ClassicTable()})
  
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
      geom_text(aes(label = Upd.Pr_2()[,"ano.mes"], y = Importaciones + 2500), size = 3, color = "grey29") +
      scale_color_manual(values = sapply(levels(as.factor(Upd.Pr_2()[, "Donante.Grupo"])),
                                         function(x) switch(x, "Vodafone" = "#E60000", "Movistar" = "#00B6E8", "Masmovil" = "#FFE500", "Orange" = "#FF9800", "Resto" = "#01B8AA"))) +
      coord_fixed(ratio = 1, xlim = c(0, max(Upd.Pr_2()[,c("Importaciones", "Exportaciones")]) * 1.04), ylim = c(0, max(Upd.Pr_2()[,c("Importaciones", "Exportaciones")]) * 1.04)) +
      theme_minimal() +
      theme(plot.margin=unit(c(0,0,0,0),"cm"), plot.subtitle = element_text(family  =  'serif'), plot.caption = element_text(family  =  'serif'), panel.grid.major = element_line(linetype  =  'blank'), panel.grid.minor = element_line(size  =  0, linetype  =  'blank'), axis.text = element_text(family  =  'serif', size  =  10), axis.text.x = element_text(family  =  'serif'), axis.text.y = element_text(family  =  'serif'), legend.title = element_text(family  =  'serif'), panel.background = element_rect(size  =  0), plot.background = element_rect(size  =  0))
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