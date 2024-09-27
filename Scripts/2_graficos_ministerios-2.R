#---------------------------------------------------------------#
#           Trabajo en la Administración Pública Nacional
#     Ministerio de Desregulación y Transformación del Estado
#           Secretaria de Simplificación del Estado
#                       Martín Rossi
#     Autores: Abigail Riquelme y Facundo Gómez García
#---------------------------------------------------------------#

#install.packages("showtext")
#install.packages("tidyr")
library(tidyr)
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

setwd("C:/Users/Usuario/Desktop/Trabajo - UdeSA/Trabajo - Ministerio/Trabajo Nación")

empleo_nacion <- read_excel("evolucion_por_puestos.xlsx", sheet = "Hoja1")
names(empleo_nacion) <- gsub(" ", "_", names(empleo_nacion))
names(empleo_nacion) <- gsub("-", "", names(empleo_nacion))
names(empleo_nacion) <- gsub("__", "_", names(empleo_nacion))
names(empleo_nacion) <- gsub(",", "", names(empleo_nacion))

empleo_nacion <- empleo_nacion %>%
  rename(Periodo = "Jurisdicción_/_Modalidad_de\r\nvinculación")
empleo_nacion$Periodo <- as.Date(empleo_nacion$Periodo, format = "%Y-%m-%d")

datos_filtrados <- empleo_nacion %>%
  select(Periodo,
         "Jefatura_de_Gabinete_de_Ministros_(JGM)",
         "Ministerio_de_Capital_Humano_(MCH)",
         "Ministerio_de_Defensa",
         "Ministerio_de_Economía",
         "Ministerio_de_Justicia",
         "Ministerio_de_Relaciones_Exteriores_Comercio_Internacional_y_culto_(MRECIC)",
         "Ministerio_de_Salud",
         "Ministerio_de_Seguridad",
         "Presidencia_de_la_Nación") %>%
  mutate(
    var_JGM = (`Jefatura_de_Gabinete_de_Ministros_(JGM)` - `Jefatura_de_Gabinete_de_Ministros_(JGM)`[1]) / `Jefatura_de_Gabinete_de_Ministros_(JGM)`[1] * 100,
    var_MCH = (`Ministerio_de_Capital_Humano_(MCH)` - `Ministerio_de_Capital_Humano_(MCH)`[1]) / `Ministerio_de_Capital_Humano_(MCH)`[1] * 100,
    var_Defensa = (`Ministerio_de_Defensa` - `Ministerio_de_Defensa`[1]) / `Ministerio_de_Defensa`[1] * 100,
    var_Economía = (`Ministerio_de_Economía` - `Ministerio_de_Economía`[1]) / `Ministerio_de_Economía`[1] * 100,
    var_Justicia = (`Ministerio_de_Justicia` - `Ministerio_de_Justicia`[1]) / `Ministerio_de_Justicia`[1] * 100,
    var_MRECIC = (`Ministerio_de_Relaciones_Exteriores_Comercio_Internacional_y_culto_(MRECIC)` - `Ministerio_de_Relaciones_Exteriores_Comercio_Internacional_y_culto_(MRECIC)`[1]) / `Ministerio_de_Relaciones_Exteriores_Comercio_Internacional_y_culto_(MRECIC)`[1] * 100,
    var_Salud = (`Ministerio_de_Salud` - `Ministerio_de_Salud`[1]) / `Ministerio_de_Salud`[1] * 100,
    var_Seguridad = (`Ministerio_de_Seguridad` - `Ministerio_de_Seguridad`[1]) / `Ministerio_de_Seguridad`[1] * 100,
    var_Presidencia = (`Presidencia_de_la_Nación` - `Presidencia_de_la_Nación`[1]) / `Presidencia_de_la_Nación`[1] * 100
  )

# Filtramos solo las columnas de cantidad que nos interesan
datos_cantidad <- empleo_nacion %>%
  select(Periodo,
         "Jefatura_de_Gabinete_de_Ministros_(JGM)",
         "Ministerio_de_Capital_Humano_(MCH)",
         "Ministerio_de_Salud") %>%
  pivot_longer(cols = -Periodo, names_to = "Ministerio", values_to = "Cantidad")


# Crear gráfico de líneas con ggplot
colores_personalizados <- c("Jefatura_de_Gabinete_de_Ministros_(JGM)" = "#46658B", 
                            "Ministerio_de_Salud" = "#4D4D4D",
                            "Ministerio_de_Capital_Humano_(MCH)" = "#999999")

nombres_ministerios <- c("Jefatura_de_Gabinete_de_Ministros_(JGM)" = "Jefatura de Gabinete de Ministros",
                         "Ministerio_de_Capital_Humano_(MCH)" = "Ministerio de Capital Humano",
                         "Ministerio_de_Defensa" = "Ministerio de Defensa",
                         "Ministerio_de_Economía" = "Ministerio de Economía",
                         "Ministerio_de_Justicia" = "Ministerio de Justicia",
                         "Ministerio_de_Relaciones_Exteriores_Comercio_Internacional_y_culto_(MRECIC)" = "Ministerio de Relaciones Exteriores",
                         "Ministerio_de_Salud" = "Ministerio de Salud",
                         "Ministerio_de_Seguridad" = "Ministerio de Seguridad",
                         "Presidencia_de_la_Nación" = "Presidencia de la Nación")

ggplot(datos_cantidad, aes(x = Periodo, y = Cantidad, group = Ministerio, color = Ministerio)) +
  geom_line(size = 1.5) + 
  geom_point(size = 3) + 
  theme_minimal() +
  scale_color_manual(values = colores_personalizados, labels = nombres_ministerios) + # Nombres en la leyenda
  labs(title = "Evolución de puestos de trabajo por ministerio",
       x = NULL,
       y = "Cantidad de puestos de trabajo",
       color = NULL) +  # Cambiar el título de la leyenda a "Jurisdicción"
  scale_y_continuous(labels = scales::comma_format()) +  # Mostrar los valores de cantidad con formato de miles
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    text = element_text(size = 14, family = "montserrat"),
    axis.title.x = element_text(margin = margin(t = 10)), 
    axis.title.y.left = element_text(margin = margin(r = 10)),
    axis.title.y.right = element_text(margin = margin(l = 10)), 
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.box = "horizontal", # Asegura que la leyenda esté en línea horizontal
    panel.grid = element_blank()  # Eliminar las líneas de cuadrícula
  ) + 
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 months") +
  guides(color = guide_legend(nrow = 2))  # Divide la leyenda en 2 filas


##### Gráfico de barras variación dic.2023 vs jul.2024:
variaciones_julio <- datos_filtrados %>%
  filter(Periodo == as.Date("2024-07-01")) %>%
  select(var_JGM, var_MCH, var_Defensa, var_Economía, var_Justicia, var_MRECIC, var_Salud, var_Seguridad, var_Presidencia) %>%
  pivot_longer(cols = everything(), names_to = "Ministerio", values_to = "Variación") %>%
  mutate(Ministerio = recode(Ministerio,
                             var_JGM = "Jefatura de Gabinete de Ministros",
                             var_MCH = "Ministerio de Capital Humano",
                             var_Defensa = "Ministerio de Defensa",
                             var_Economía = "Ministerio de Economía",
                             var_Justicia = "Ministerio de Justicia",
                             var_MRECIC = "Ministerio de Relaciones Exteriores",
                             var_Salud = "Ministerio de Salud",
                             var_Seguridad = "Ministerio de Seguridad",
                             var_Presidencia = "Presidencia de la Nación"))

# Crear histograma ordenado
ggplot(variaciones_julio, aes(x = reorder(Ministerio, Variación), y = Variación, fill = Ministerio)) +
  geom_bar(stat = "identity", fill = "#46658B", size = 0.5) +  # Agregamos un borde negro y ajustamos el tamaño
  geom_text(aes(label = round(Variación, 1)),  
            hjust = ifelse(variaciones_julio$Variación < 0, 1.3, -0.2),  # Mayor ajuste para los negativos
            vjust = 0.5,  # Centra verticalmente las etiquetas
            color = "black", size = 4) +  
  coord_flip() +  # Voltear los ejes para que el histograma sea horizontal
  theme_minimal() +
  labs(title = "Variación de julio 2024 respecto a diciembre 2023",
       x = NULL,
       y = "Variación porcentual (%)",
       fill = "Jurisdicción",
       caption="\nFuente: Elaboración propia en base a datos del SIRHU") +  # Cambiar el título de la leyenda a "Jurisdicción"
  scale_y_continuous(labels = scales::comma_format(), 
                     breaks = scales::pretty_breaks(n = 10),
                     limits = c(-18.5, 2)) +  # Cambia los límites para que los valores negativos se vean mejor
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12, margin = margin(r = 10)), 
    text = element_text(size = 14, family = "montserrat"),
    axis.title.x = element_text(margin = margin(t = 10)), 
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.box = "horizontal",  # Asegura que la leyenda esté en línea horizontal
    panel.grid = element_blank()  # Eliminar las líneas de cuadrícula
  ) +
  guides(fill = guide_legend(nrow = 2))  # Divide la leyenda en 2 filas


