
## Option2 of New ggplot2 graph
itemstoplot = intersect(which(Porta.2$Operador.Grupo == "Vodafone"),which(Porta.2$ano.mes >= 1805))

ggplot(data = Porta.2[itemstoplot,],
       aes(y = Importaciones, x = Exportaciones)) +
  geom_polygon(data = zones, aes(y = Importaciones, x = Exportaciones, group = as.factor(group)), alpha = 0.5, fill = zones$color, color = zones$color, linetype = 0, inherit.aes = FALSE) +
  geom_point(size=2, aes(colour=Donante.Grupo)) +
  geom_line(aes(color = Donante.Grupo), arrow = arrow(length=unit(0.30,"cm"), type = "closed")) +
  geom_text(aes(label = Porta.2[itemstoplot,"ano.mes"], y = Importaciones + 5000), size = 3) +
  scale_color_manual(values = sapply(levels(as.factor(Porta.2[itemstoplot, "Donante.Grupo"])), function(x) switch(x, "Vodafone" = "#E60000", "Movistar" = "#00B6E8", "Masmovil" = "#FFE500", "Orange" = "#FF9800", "Resto" = "#01B8AA")))

## Option3 of New ggplot2 graph
ggplot(data = Porta.2[itemstoplot,],
       aes(y = Importaciones, x = Total.Receptor, color = Donante.Grupo)) +
  geom_segment(data = reshape(DATA, v.names="VALUE", idvar = "NAME", timevar = "YEAR", direction = "wide"),
               aes(x=VALUE.2011, xend=VALUE.2016, y=NAME, yend=NAME), size = 2,
               arrow = arrow(length = unit(0.5, "cm")))

#### cambio dataset

Porta.3 = Porta.1

Porta.3$Importaciones = sapply(Porta.1$Importaciones, function(x) {
  x = x + runif(1,0,2) * x
  x
})

Porta.4 = as.data.table(Porta.3[which(Porta.3$ano.mes == 1804),])
Porta.4$Referencia = Porta.1[which(Porta.1$ano.mes == 1804),"Importaciones"]

Porta.4[, .( resta = sum(Importaciones)-sum(Referencia), Import = sum(Importaciones), Refer = sum(Referencia)), by = Donante.Grupo]

