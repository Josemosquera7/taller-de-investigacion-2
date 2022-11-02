rm(list=ls())
require(pacman)
p_load(dplyr, tidyverse, ggplot2, stargazer, tidyr, rio, skimr, janitor)
base_trabajo <-  import("base_investigacion.xlsx")
base_trabajo_filtro <- import("base_investigación_filtro_gastostransporte.xlsx")

#Ahora estimaremos el modelo grande de nuestro proyecto
reg_1 <- lm(valor_agregado ~  gastostransporte_insumos + gastostransp_productos + vias_primarias +  tamaño_mercado_interno + IDC + Cobertura_energia, data = base_trabajo )
reg_1
reg_2 <- lm(valor_agregado ~  gastostransporte_insumos + gastostransp_productos + vias_primarias +  tamaño_mercado_interno + IDC + Cobertura_energia, data = base_trabajo_filtro )
reg_3 <- lm(log_valagri ~ log_gastostrans_insumos + log_gastostrans_productos + vias_primarias +  tamaño_mercado_interno + IDC + Cobertura_energia, data = base_trabajo_filtro)
stargazer(reg_1, reg_2, reg_3,
          type= "html",
          dep.var.labels = c("productividad de la empresa", "logaritmo valor agregado"),
          covariate.labels = c("gastos transporte insumos", "gastos transporte producto", "log gastos transporte insumos", "log gastos transporte producto", "vias primarias por cada 100 mil habitantes", "indice tamaño mercado interno", "Indice Departamental de Competitividad", "Cobertura de energia"),
          out= "resultados del trabajo.doc")

