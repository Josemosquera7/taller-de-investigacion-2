rm(list=ls())
require(pacman)
p_load(dplyr, tidyverse, ggplot2, stargazer, tidyr, rio, skimr, janitor)
base_trabajo <-  import("base_investigacion.xlsx")
view(base_trabajo)
#Ahora estimaremos el modelo grande de nuestro proyecto
reg_1 <- lm(valor_agregado ~  gastostransporte_insumos + gastostransp_productos + vias_primarias +  tamaño_mercado_interno + IDC + Cobertura_energia, data = base_trabajo )
reg_1
reg_2 <- lm(log_valagri ~ gastostransporte_insumos + gastostransp_productos + vias_primarias +  tamaño_mercado_interno + IDC + Cobertura_energia, data = base_trabajo)
stargazer(reg_1, reg_2,
          type= "html",
          dep.var.labels = c("productividad de la empresa", "logaritmo valor agregado"),
          covariate.labels = c( "gastos transporte de insumos", "gastos transporte productos", "vias primarias por cada 100 mil habitantes", "indice tamaño mercado interno", "Indice Departamental de Competitividad", "Cobertura de energia"),
          out= "resultados del trabajo.doc")
##modelo secundario que nos pordria ayudar a responder nuestra pregunta
reg_3 <- lm(densidad_empresarial ~ vias_primarias +  tamaño_mercado_interno + IDC + Cobertura_energia, data = base_trabajo )
stargazer(reg_2,
          type= "html",
          dep.var.labels = c("densidad empresarial"),
          covariate.labels = c( "vias primarias por cada 100 mil habitantes", "indice tamaño mercado interno", "Indice Departamental de Competitividad", "Cobertura de energia"),
          out= "resultados del trabajo 2.doc")