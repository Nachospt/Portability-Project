
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



## Simulation starting data
## Starting pattern
Pr_0A = data.table(
  c(rep(1804,25)),
  c(rep("Movistar",5),
    rep("Vodafone",5),
    rep("Orange",5),
    rep("Masmovil",5),
    rep("Resto",5)),
  c(rep(c("Movistar", "Vodafone", "Orange", "Masmovil", "Resto"), 5)),
  c(1, 6, 5, 7, 2,
    4, 2, 7, 2, 3,
    6, 4, 2, 4, 3,
    8, 8, 7, 1, 2,
    5, 4, 5, 4, 5
  )
)

## Stable random series to transform starting pattern
a = sapply(1:25, FUN = function(x) runif(1,-1,1)) # Serie of 25 random numbers

for (i in c(1701, 1702, 1703, 1704, 1705, 1706, 1707, 1708, 1709, 1710, 1711, 1712, 1801, 1802, 1803, 1804, 1805, 1806)) {
  print(i)
  a = sapply(a, FUN = function(x) (x + runif(1,-1,1) / 1.1 )) # Modification of the random numbers
  # Another random number is summed and it is divided but some number to make some decay.
  
  Pr_0A = rbind(Pr_0A, data.table(
    c(rep(i,25)),
    c(rep("Movistar",5),
      rep("Vodafone",5),
      rep("Orange",5),
      rep("Masmovil",5),
      rep("Resto",5)),
    c(rep(c("Movistar", "Vodafone", "Orange", "Masmovil", "Resto"), 5)),
    abs(c(1, 6, 5, 7, 2,
          4, 2, 7, 2, 3,
          6, 4, 2, 4, 3,
          8, 8, 7, 1, 2,
          5, 4, 5, 4, 5
    ) + a)
  )) 
}
colnames(Pr_0A) <- c("ano.mes","Operador.Grupo", "Donante.Grupo", "Importaciones")
Pr_0A[,ano.mes:= as.integer(ano.mes)]
Pr_0A = Pr_0A[Pr_1, .(ano.mes, Operador.Grupo, Donante.Grupo, Importaciones, Referencia), on = .(ano.mes, Operador.Grupo, Donante.Grupo)]

# Reference adjust

Pr_1 = Pr0A
Pr_2 = Pr_1

Pr_2$Exportaciones = apply(Pr_1, 1, FUN = function(x) {
  TargetRow = intersect(which(Pr_1$Donante.Grupo == x["Operador.Grupo"]),
                        intersect(which(Pr_1$Operador.Grupo == x["Donante.Grupo"]),
                                  which(Pr_1$ano.mes == x["ano.mes"])))
  Pr_1$Importaciones[TargetRow]
})

#### cambio dataset
Porta.1 = Pr_2
Porta.3 = Pr_2
# 
# Porta.3$Importaciones = sapply(Porta.1$Importaciones, function(x) {
#   x = x + x * runif(1,0,2)
#   x
# })
Porta.4 = as.data.table(Porta.3)
Porta.4$Referencia = Porta.1[,"Importaciones"]

Porta.5 = Porta.4
Porta_F = function(x) {x %>%
  .[, .( resta = sum(Importaciones)-sum(Referencia), Import = sum(Importaciones), Refer = sum(Referencia), Export = sum(Exportaciones)), by = list(ano.mes, Operador.Grupo)]}

for  (z in levels(as.factor(Porta.5$ano.mes))) {
  for (i in levels(as.factor(Porta.5$Operador.Grupo))) {
    print(i)
    print(z)
    if (abs(Porta_F(Porta.5[ano.mes == z,])[Operador.Grupo == i,resta]) > 2)
    { Porta.5[ano.mes == z,][Operador.Grupo == i, "Importaciones" ] = Porta.5[ano.mes == z,][Operador.Grupo == i, "Importaciones"] * Porta_F(Porta.5[ano.mes == z,])[Operador.Grupo == i, Refer/Import]}
  }
}

View(cbind(Porta.3, Porta.5))

## CNMC data

Pr_1 = fread("C:\\Users\\a1380\\Desktop\\Portability Project\\9. Portabilidades de numeración móvil.csv",skip = 4)
colnames(Pr_1)[2] = "Grupo.Operador"
Pr_1[, V1 := NULL]
Pr_1 = Pr_1[9:14,][Grupo.Operador %in% c("Movistar", "Vodafone", "Orange", "Grupo MASMOV!L", "OMV"),]

##  
colnames(Pr_1)[2:length(colnames(Pr_1))] = colnames(Pr_1)[2:length(colnames(Pr_1))] %>%
  substring(1,3) %>%
  sapply(.,function(x) do.call("switch", as.list(c(x,"Ene" = "01","Feb" = "02", "Mar" = "03", "Abr" = "04", "May" = "05", "Jun" = "06", "Jul" = "07", "Ago" = "08", "Sep" = "09", "Oct" = "10", "Nov" = "11", "Dic" = "12")))) %>%
  paste(substring(colnames(Pr_1)[2:length(colnames(Pr_1))], 7, 8), sep = "",.)

## Pivoting year columns
Pr_1 <- melt(Pr_1, id=c("Grupo.Operador"))
colnames(Pr_1)[2] = "ano.mes"

## Selecting recent data
Pr_1 = Pr_1[as.numeric(as.character(ano.mes)) > 1609,]

write.csv(Pr_1, file = "Sim_data.csv",row.names=FALSE, na="")