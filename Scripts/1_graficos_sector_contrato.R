
#### GRAFICOS EMPLEO PUBLICO #### 

library(ggplot2)
library(tidyr)
library(scales)
library(readxl)
library(lubridate)
library(ggplot2)
library(ggtext)
library(dplyr)
library(scales)
library(showtext)
library(writexl)

font_add_google("Montserrat", "montserrat")
showtext_auto()

# Crear el dataframe con los datos proporcionados
datos <- data.frame(
  Periodo = c("Dic.23", "Ene.24", "Feb.24", "Mar.24", "Abr.24", "May.24", "Jun.24", "Jul.24"),
  "Pnal.Civil" = c(205550, 200778, 199837, 197815, 192091, 191149, 189600, 187749),
  "Pnal.Militar" = c(204581, 203012, 203332, 205644, 204934, 203948, 204117, 203722),
  "Pnal.Empresas" = c(91474, 90200, 89816, 89133, 88487, 86779, 83290, 81636),
  Total = c(501605, 493990, 492985, 492592, 485512, 481876, 477007, 473107)
)

# Convertir el dataframe a formato largo
datos_largo <- pivot_longer(datos, cols = c("Pnal.Civil", "Pnal.Militar", "Pnal.Empresas", "Total"),
                            names_to = "Categoria", values_to = "Cantidad")

# Crear un vector de fechas en formato "año-mes"
fechas_ordenadas <- rep(c("2023-12", "2024-01", "2024-02", "2024-03", "2024-04", "2024-05", "2024-06", "2024-07"),
                        each = 4)

# Asignar este formato al Periodo
datos_largo$Periodo <- factor(fechas_ordenadas, 
                              levels = unique(fechas_ordenadas))


write_xlsx(datos_largo, "/Users/ariquelme/Desktop/base_1.xlsx")


# Colores personalizados para las líneas
colores_personalizados <- c("#46658B", "#F3BA4D", "#4D4D4D", "#999999")



### TOTAL ####

ggplot(datos_largo[datos_largo$Categoria=="Total",], aes(x = Periodo, y = Cantidad, group = Categoria)) + 
  geom_line(size = 1.5, color="#242C4F") + 
  geom_point(size = 3, color="#242C4F") + 
  theme_minimal() + 
  scale_color_manual(values = colores_personalizados) +  # Colores personalizados
  labs(title = "Personal Civil, Militar y de Seguridad de la\n Administración Pública Nacional Centralizada, Descentralizada, \nOtros Entes y Empresa del Estado",
       x = NULL,
       y = "Cantidad de puestos de trabajo",
       color = NULL, caption="Fuente: Elaboración propia en base a datos del SIRHU") + 
  scale_y_continuous(labels = scales::comma, limits = c(470000, 505000)) + # Ajustes para los límites del eje Y
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    text = element_text(size = 14, family = "montserrat"),
    legend.position = "none",
    legend.text = element_text(size = 12),
    panel.grid = element_blank()  # Eliminar las líneas de cuadrícula
  ) + 
  geom_label(aes(x = "2023-12", y = 501605+3000, label = "501.605"), fill = "white", color = "#999999", size = 5, label.size = 0.5) + 
  geom_label(aes(x = "2024-07", y = 473107+3000, label = "473.107"), fill = "white", color = "#999999", size = 5, label.size = 0.5) + 
  annotate("text", x = "2024-07", y = 476386+2900, label = "2024-07 vs. 2023-12\n -5.7 %", color = "#999999", size = 5, hjust = 0.5, fontface = "bold")


### MILITAR #### 

g1 <- ggplot(datos_largo[datos_largo$Categoria=="Pnal.Militar",], aes(x = Periodo, y = Cantidad, group = Categoria, color = Categoria)) +
  geom_line(size = 1.5, color="#242C4F") +
  geom_point(size = 3, color="#242C4F") +
  theme_minimal() +
  scale_color_manual(values = colores_personalizados) +  # Colores personalizados
  labs(title = "Militar",
       x = NULL,
       y = "",
       color = NULL) +
  scale_y_continuous(labels = scales::comma, limits = c(202700, 205900)) + # Ajustes para los límites del eje Y
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12),
    text = element_text(size = 14, family = "montserrat"),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 12), 
    panel.grid = element_blank() 
  ) +
  geom_label(aes(x = "2023-12", y = 204581+200, label = "204581"), fill = "white", color = "#999999", size = 4, label.size = 0.5) +
  geom_label(aes(x = "2024-07", y = 203722+200, label = "203722"), fill = "white", color = "#999999", size = 4, label.size = 0.5) +
  annotate("text", x = "2024-07", y = 203722+470, label = "2024-07 vs.\n2023-12:\n-0.4 %", color = "#999999", size = 4, hjust = 0.7,fontface = "bold")

g1
### EMPRESAS #### 


g2 <- ggplot(datos_largo[datos_largo$Categoria=="Pnal.Empresas",], aes(x = Periodo, y = Cantidad, group = Categoria, color = Categoria)) +
  geom_line(size = 1.5, color="#242C4F") +
  geom_point(size = 3, color="#242C4F") +
  theme_minimal() +
  scale_color_manual(values = colores_personalizados) +  # Colores personalizados
  labs(title = "Empresas",
       x = NULL,
       y = "",
       color = NULL) +
  scale_y_continuous(labels = scales::comma, limits = c(80000, 93000)) + # Ajustes para los límites del eje Y
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    text = element_text(size = 14, family = "montserrat"),
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 12), 
    panel.grid = element_blank() 
  ) +
  geom_label(aes(x = "2023-12", y = 91474+700, label = "91474"), fill = "white", color = "#999999", size = 4, label.size = 0.5) +
  geom_label(aes(x = "2024-07", y = 81636+700, label = "81636"), fill = "white", color = "#999999", size = 4, label.size = 0.5) +
  annotate("text", x = "2024-07", y = 81636+1750, label = "2024-07 vs.\n2023-12:\n-10.8 %", color = "#999999", size = 4, hjust = 0.7,fontface = "bold")

g2


#### CIVIL #### 

g3 <- ggplot(datos_largo[datos_largo$Categoria=="Pnal.Civil",], aes(x = Periodo, y = Cantidad, group = Categoria, color = Categoria)) +
  geom_line(size = 1.5, color="#242C4F") +
  geom_point(size = 3, color="#242C4F") +
  theme_minimal() +
  scale_color_manual(values = colores_personalizados) +  # Colores personalizados
  labs(title = "Administración Pública",
       x = NULL,
       y = "Cantidad de puestos de trabajo",
       color = NULL) +
  scale_y_continuous(labels = scales::comma, limits = c(180000, 209000)) + # Ajustes para los límites del eje Y
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12),
    text = element_text(size = 14, family = "montserrat"),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 12), 
    panel.grid = element_blank() 
  ) +
  geom_label(aes(x = "2023-12", y = 205550+1150, label = "205550"), fill = "white", color = "#999999", size = 4, label.size = 0.5) +
  geom_label(aes(x = "2024-07", y = 187749+1150, label = "187749"), fill = "white", color = "#999999", size = 4, label.size = 0.5) +
  annotate("text", x = "2024-07", y = 187749+3500, label = "2024-07 vs.\n2023-12:\n-8.7 %", color = "#999999", size = 4, hjust = 0.7,fontface = "bold")

library(gridExtra)
library(grid)
titulo <- textGrob("Empleo público por sectores", 
                   gp = gpar(fontsize = 24, fontfamily = "montserrat", fontface = "bold"))


caption <- textGrob("Fuente: Elaboración propia en base a datos del SIRHU", 
                    gp = gpar(fontsize = 12, fontfamily = "montserrat"), 
                    hjust = 1, x = 1)

grafico_completo <- arrangeGrob(
  g3, g2, g1, 
  ncol = 3, 
  top = titulo
)

grafico_con_caption <- arrangeGrob(
  grafico_completo, 
  bottom = caption
)

grid.newpage()

grid.draw(grafico_con_caption)


ggsave("grafico_publico_por_sectores.svg", plot = grafico_con_caption, width = 10, height = 6)



#### CANTIDAD DE PUESTOS DE TRABAJO POR TIPO DE VINCULACION #### 


library(ggplot2)
library(tidyr)
options(scipen = 999)


datos_3 <- data.frame(
  Periodo = c("Dic.23", "Ene.24", "Feb.24", "Mar.24", "Abr.24", "May.24", "Jun.24", "Jul.24"),
  Planta_Permanente_Transitoria = c(128760, 127648, 128167, 127492, 126875, 126679, 126241, 125620),
  Personal_Contratado = c(69982, 67860, 66457, 65160, 61440, 60723, 59671, 58924),
  Contratos_LOYS = c(6808, 5270, 5213, 5163, 3776, 3747, 3688, 3205),
  Total_General = c(205550, 200778, 199837, 197815, 192091, 191149, 189600, 187749)
)

datos_3_largo <- pivot_longer(datos_3, cols = -Periodo,
                              names_to = "Categoria", values_to = "Cantidad")

fechas_ordenadas <- c(
  "Dic.23" = "2023-12",
  "Ene.24" = "2024-01",
  "Feb.24" = "2024-02",
  "Mar.24" = "2024-03",
  "Abr.24" = "2024-04",
  "May.24" = "2024-05",
  "Jun.24" = "2024-06",
  "Jul.24" = "2024-07"
)

datos_3_largo <- datos_3_largo %>%
  mutate(Periodo = fechas_ordenadas[Periodo])

fechas_ordenadas <- rep(c("2023-12", "2024-01", "2024-02", "2024-03", "2024-04", "2024-05", "2024-06", "2024-07"),
                        each = 4)

# Asignar este formato al Periodo
datos_3_largo$Periodo <- factor(fechas_ordenadas, 
                                levels = unique(fechas_ordenadas))


write_xlsx(datos_3_largo, "/Users/ariquelme/Desktop/base_2.xlsx")


datos_largo <- datos_3_largo

#### PERSONAL CONTRATADO #### 

# Evolución Puestos de Trabajo Personal Civil de la Administración Pública Nacional 
# Centralizada, Descentralizada y Otros Entes por modalidad de vinculación

g4 <- ggplot(datos_largo[datos_largo$Categoria=="Personal_Contratado",], aes(x = Periodo, y = Cantidad, group = Categoria)) + 
  geom_line(size = 1.5, color="#242C4F") + 
  geom_point(size = 3, color="#242C4F") + 
  theme_minimal() + 
  scale_color_manual(values = colores_personalizados) +  # Colores personalizados
  labs(title = "Ley Marco",
       x = NULL,
       y = "Cantidad de puestos de trabajo",
       color = NULL) + 
  scale_y_continuous(labels = scales::comma, limits = c(58000, 71000)) + # Ajustes para los límites del eje Y
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    text = element_text(size = 14, family = "montserrat"),
    legend.position = "none",
    legend.text = element_text(size = 12),
    panel.grid = element_blank()  # Eliminar las líneas de cuadrícula
  ) + 
  geom_label(aes(x = "2023-12", y = 69982+1000, label = "69982"), fill = "white", color = "#999999", size = 4, label.size = 0.5) + 
  geom_label(aes(x = "2024-07", y = 58924+1000, label = "58924"), fill = "white", color = "#999999", size = 4, label.size = 0.5) + 
  annotate("text", x = "2024-07", y = 58924+2000, label = "2024-07 vs.\n 2023-12\n -18.8 %", color = "#999999", size = 4, hjust = 0.8, fontface = "bold")

g4
#### LOYS #### 

g5 <- ggplot(datos_largo[datos_largo$Categoria=="Contratos_LOYS",], aes(x = Periodo, y = Cantidad, group = Categoria)) + 
  geom_line(size = 1.5, color="#242C4F") + 
  geom_point(size = 3, color="#242C4F") + 
  theme_minimal() + 
  scale_color_manual(values = colores_personalizados) +  # Colores personalizados
  labs(title = "Locacion de Obras y Servicios (LOYS)",
       x = NULL,
       y = "Cantidad de puestos de trabajo",
       color = NULL) + 
  scale_y_continuous(labels = scales::comma, limits = c(3200, 7200)) + 
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    text = element_text(size = 14, family = "montserrat"),
    legend.position = "none",
    legend.text = element_text(size = 12),
    panel.grid = element_blank()  
  ) + 
  geom_label(aes(x = "2023-12", y = 6808+300, label = "6808"), fill = "white", color = "#999999", size = 4, label.size = 0.5) + 
  geom_label(aes(x = "2024-07", y = 3205+300, label = "3205"), fill = "white", color = "#999999", size = 4, label.size = 0.5) + 
  annotate("text", x = "2024-07", y = 3205+600, label = "2024-07 vs.\n 2023-12\n -5.7 %", color = "#999999", size = 4, hjust = 0.8, fontface = "bold")
g5

#### PLANTA PERMANENTE #### 

g6 <- ggplot(datos_largo[datos_largo$Categoria=="Planta_Permanente_Transitoria",], aes(x = Periodo, y = Cantidad, group = Categoria)) + 
  geom_line(size = 1.5, color="#242C4F") + 
  geom_point(size = 3, color="#242C4F") + 
  theme_minimal() + 
  scale_color_manual(values = colores_personalizados) +  # Colores personalizados
  labs(title = "Planta permanente y transitoria",
       x = NULL,
       y = "Cantidad de puestos de trabajo",
       color = NULL) + 
  scale_y_continuous(labels = scales::comma, limits = c(125500, 129200)) + 
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    text = element_text(size = 14, family = "montserrat"),
    legend.position = "none",
    legend.text = element_text(size = 12),
    panel.grid = element_blank()  
  ) + 
  geom_label(aes(x = "2023-12", y = 128760+300, label = "128760"), fill = "white", color = "#999999", size = 4, label.size = 0.5) + 
  geom_label(aes(x = "2024-07", y = 125620+300, label = "125620"), fill = "white", color = "#999999", size = 4, label.size = 0.5) + 
  annotate("text", x = "2024-07", y = 125620+600, label = "2024-07 vs.\n 2023-12\n -2.4 %", color = "#999999", size = 4, hjust = 0.8, fontface = "bold")

g6


library(gridExtra)
library(grid)
titulo <- textGrob("Empleo público por tipo de contrato", 
                   gp = gpar(fontsize = 24, fontfamily = "montserrat", fontface = "bold"))

grafico_completo <- arrangeGrob(
  g4, g5, g6,
  ncol = 3,         
  top = titulo    
)


caption <- textGrob("Fuente: Elaboración propia en base a datos del SIRHU", 
                    gp = gpar(fontsize = 12, fontfamily = "montserrat"), 
                    hjust = 1, x = 1)

grafico_con_caption <- arrangeGrob(
  grafico_completo, 
  bottom = caption
)

grid.newpage()

grid.draw(grafico_con_caption)





