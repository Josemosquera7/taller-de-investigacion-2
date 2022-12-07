rm(list=ls())
require(pacman)
p_load(dplyr, tidyverse, ggplot2, stargazer, tidyr, rio, skimr, janitor, tmaptools,
       sf, 
       leaflet,
       ggsn, 
       osmdata,
       ggplot2, 
       ggmap,
       viridis)

base_trabajo <-  import("Build/Outputs/base_investigacion.xlsx")
base_trabajo_filtro <- import("Build/Outputs/base_investigación_filtro_gastostransporte.xlsx")

estadisticas_d1 <- base_trabajo[c("valor_agregado", "gastostransporte_insumos", "gastostransp_productos", "vias_primarias", "tamaño_mercado_interno" , "IDC" , "Cobertura_energia")]
stargazer(estadisticas_d1, type="html", title="Tabla 1:Estadisiticas descriptivas", out="Analysis/Tablas/descriptivas1.doc")

#Graficas
png("Analysis/Figuras/Grafico de dispersion Gastos transporte insumos vs Valor agregado")
ggplot(base_trabajo, aes(x=log_gastostrans_insumos, y=log_valagri)) +
  geom_point() +
  labs (x="Gastos trasnporte insumos", y="Valor agregado") +
  geom_smooth(method="lm")+ggtitle("Grafico 1: Log gastos trasnporte insumos vs log Valor agregado")
dev.off()

png("Analysis/Figuras/Histograma del tamaño del mercado interno")
ggplot(base_trabajo, aes(x=tamaño_mercado_interno))+
  geom_histogram(color="blue", fill="blue", alpha=0.75,bins=25, position="identity")+
  scale_x_continuous(name="Indice Tamaño de mercado Interno")+
  scale_y_continuous(name="Frecuencua")+
  ggtitle("Grafico 2: Histograma Tamaño de mercado Interno")
dev.off()

png("Analysis/Figuras/Histograma indice de vias primarias")
ggplot(base_trabajo, aes(x=vias_primarias))+
  geom_histogram(color="red", fill="red", alpha=0.75,bins=25, position="identity")+
  scale_x_continuous(name="Vias primarias por cada 100 mil habitantes")+
  scale_y_continuous(name="Frecuencua")+
  ggtitle("Grafico 3: Histograma Vias primarias")
dev.off()

png("Analysis/Figuras/Grafico de dispersion gastos de transporte de products vs valor agregado")
ggplot(base_trabajo, aes(x=log_gastostrans_productos, y=log_valagri)) +
  geom_point() +
  labs (x="Gastos trasnporte productos", y="Valor agregado") +
  geom_smooth(method="lm")+ggtitle("Grafico 4: Log gastos trasnporte productos vs log Valor agregado")
dev.off()

grafica3 <- ggplot(base_trabajo_filtro, aes(x=IDC, y=valor_agregado))+
            geom_point()+
            labs(x="Indice departamental de competitividad", y="Valor Agregado")+
           geom_smooth(method="lm")+
           ggtitle("Grafico 5: Valor agregado vs Indice departamental de competitividad")
#Ahora estimaremos el modelo grande de nuestro proyecto
reg_1 <- lm(valor_agregado ~  gastostransporte_insumos + gastostransp_productos + vias_primarias +  tamaño_mercado_interno + IDC + Cobertura_energia, data = base_trabajo )
reg_1
reg_2 <- lm(valor_agregado ~  gastostransporte_insumos + gastostransp_productos + vias_primarias +  tamaño_mercado_interno + IDC + Cobertura_energia, data = base_trabajo_filtro )
reg_3 <- lm(log_valagri ~ log_gastostrans_insumos + log_gastostrans_productos + vias_primarias +  tamaño_mercado_interno + IDC + Cobertura_energia, data = base_trabajo_filtro)
stargazer(reg_1, reg_2, reg_3,
          type= "html",
          dep.var.labels = c("productividad de la empresa", "logaritmo valor agregado"),
          covariate.labels = c("gastos transporte insumos", "gastos transporte producto", "log gastos transporte insumos", "log gastos transporte producto", "vias primarias por cada 100 mil habitantes", "indice tamaño mercado interno", "Indice Departamental de Competitividad", "Cobertura de energia"),
          out= "Analysis/Tablas/resultados del trabajo.doc")

##Crear mapas usando datos espaciales en r
col <- st_read("Build/Inputs/MGN_DPTO_POLITICO.shp") %>% select(DPTO_CCDGO) %>% mutate(DPTO_CCDGO =as.numeric(DPTO_CCDGO))
base_trabajo_espacial <- import("Build/Inputs/Base_IDC_web_actualizado.xlsx", sheet=3) %>% filter(Año== 2018) %>%
  rename(DPTO_CCDGO =codigo_dane) %>% select(DPTO_CCDGO, Puntaje_general)
bases_unidas <- left_join(col,base_trabajo_espacial, "DPTO_CCDGO")
mapa <- ggplot() +
        geom_sf(data=bases_unidas,aes(fill=Puntaje_general)) +
      scale_fill_viridis(direction=-1, option = "F") + theme_test()
mapa
##Guardamos el mapa
ggsave("Build/Outputs/departamentos por IDC.png", mapa)