
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
set.seed(0)
## Starting pattern
Pr_0A = data.table()

## Stable random series to transform starting pattern
a = sapply(1:25, FUN = function(x) runif(1,-1,1)) # Serie of 25 random numbers

for (i in c(1610, 1611, 1612, 1701, 1702, 1703, 1704, 1705, 1706, 1707, 1708, 1709, 1710, 1711, 1712, 1801, 1802, 1803, 1804, 1805, 1806, 1807, 1808, 1809, 1810, 1811, 1812)) {
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
    ) + a )
  )) 
}
colnames(Pr_0A) <- c("ano.mes","Operador.Grupo", "Donante.Grupo", "Importaciones")
Pr_0A[,ano.mes:= as.integer(ano.mes)]

# Reference adjust
Pr_1 = Pr_0A

## 3.2 Removing self-portabilities
Pr_1 = Pr_1[!which(Pr_1$Donante.Grupo == Pr_1$Operador.Grupo),]
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
Porta.4 = Porta.4[Pr_Ref, .(ano.mes, Operador.Grupo, Donante.Grupo, Importaciones, Exportaciones, Ref.Import, Ref.Export), on = .(ano.mes, Operador.Grupo), nomatch = 0]

Porta.5 = Porta.4
Porta_F = function(x) {x %>%
  .[, .( resta = sum(Importaciones)-mean(Ref.Import), Import = sum(Importaciones), Ref.Import = mean(Ref.Import), Export = sum(Exportaciones), Ref.Export = mean(Ref.Export)), by = list(ano.mes, Operador.Grupo)]}

while (Porta.5[abs(Exportaciones - Ref.Export) > 100, .N)]
for (z in levels(as.factor(Porta.5$ano.mes))) {
  for (i in levels(as.factor(Porta.5$Operador.Grupo))) {
    print(i)
    print(z)
    if (abs(Porta_F(Porta.5[ano.mes == z,])[Operador.Grupo == i,resta]) > 2)
    { Porta.5[ano.mes == z,][Operador.Grupo == i, "Importaciones" ] = Porta.5[ano.mes == z,][Operador.Grupo == i, "Importaciones"] * Porta_F(Porta.5[ano.mes == z,])[Operador.Grupo == i, Ref.Import/Import]}
  }
}

View(cbind(Porta.3, Porta.5))

## CNMC data
## Import data
Pr_X = fread("C:\\Users\\a1380\\Desktop\\Portability Project\\9. Portabilidades de numeración móvil.csv",skip = 4)
colnames(Pr_X)[2] = "Operador.Grupo"
Pr_X[, V1 := NULL]
Pr_X = Pr_X[c(2:7, 9:14)][, Operador.Grupo := c("Movistar", "Vodafone", "Orange", "Yoigo", "Masmovil", "Resto","Movistar", "Vodafone", "Orange", "Yoigo", "Masmovil", "Resto")]
Pr_X = Pr_X[Operador.Grupo %in% c("Movistar", "Vodafone", "Orange", "Masmovil", "Resto"),]

##
colnames(Pr_X)[2:length(colnames(Pr_X))] = colnames(Pr_X)[2:length(colnames(Pr_X))] %>%
  substring(1,3) %>%
  sapply(.,function(x) do.call("switch", as.list(c(x,"Ene" = "01","Feb" = "02", "Mar" = "03", "Abr" = "04", "May" = "05", "Jun" = "06", "Jul" = "07", "Ago" = "08", "Sep" = "09", "Oct" = "10", "Nov" = "11", "Dic" = "12")))) %>%
  paste(substring(colnames(Pr_X)[2:length(colnames(Pr_X))], 7, 8), sep = "",.)

## Pivoting year columns
## Export elements
Pr_Exp <- melt(Pr_X[6:10], id=c("Operador.Grupo"))
colnames(Pr_Exp)[2:3] = c("ano.mes", "Ref.Export")

## Import elements
Pr_Imp <- melt(Pr_X[1:5], id=c("Operador.Grupo"))
colnames(Pr_Imp)[2:3] = c("ano.mes", "Ref.Import")

## Joining both elements
Pr_Ref = Pr_Imp[Pr_Exp, .(ano.mes, Operador.Grupo, Ref.Import, Ref.Export), on = .(ano.mes, Operador.Grupo)]

## Selecting recent data (after transformingobject classes)
Pr_Ref[,ano.mes := sapply(ano.mes, as.character)][,ano.mes := sapply(ano.mes, as.integer)][ano.mes > 1609]

write.csv(Pr_X, file = "Sim_data.csv",row.names=FALSE, na="")