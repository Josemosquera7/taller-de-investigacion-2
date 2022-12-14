rm(list=ls())
require(pacman)
p_load(dplyr, tidyverse, ggplot2, stargazer, tidyr, rio, skimr, janitor)
base_manufacturera_2018 <-  import("Build/Inputs/EAM_2018.dta")
clean_names(base_manufacturera_2018)
##Como trabajamos en un proyecto en Github, no es necesario establecer un directorio de trabajo
Datos_IDC <-  import("Build/Inputs/Base_IDC_web_actualizado.xlsx", sheet= 3)
clean_names(Datos_IDC)
Datos_IDC_1 <-  Datos_IDC %>% filter(Año == 2018)
##dejamos las variables de interés para la encuesta manufacturera
base_manufacturera_final <- base_manufacturera_2018 %>% select(nordemp, dpto, ciiu4,periodo, c4r4c10t, c4r4c9t, c3r45c3, c3r42c3, c3r37c3, c3r38c3, c7r8c2, VALAGRI)  %>% 
  rename(actividad_económica = ciiu4, codigo_dane = dpto, trabajadores_hombres = c4r4c10t , trabajadores_mujeres = c4r4c9t, gastostransp_productos = c3r45c3, gastostransporte_insumos = c3r42c3 ,imp_industriacomercio = c3r37c3 , imp_predial_vehiculo = c3r38c3, inversion_transporte = c7r8c2, valor_agregado = VALAGRI)%>%
  mutate(total_trabajadores = trabajadores_hombres + trabajadores_mujeres) %>%
  mutate(proporcion_mujeres = trabajadores_mujeres / total_trabajadores *100)
##organizamos base de datos de indice departamental de competitividad
Datos_IDC_1 <- Datos_IDC_1 %>% select(codigo_dane, Departamento, Año, `INS-1-1`, `INS-2-2`, `INF-1-3`, `INF-2-1`, `INF-3-1`, `INF-3-2`,`LAB-1-1`, `LAB-2-1`, `TAM-1-1`, `TAM-2-1`, `INN-1-1`, `INN-3-2`,`Puntaje_general` ) %>% 
  rename(gestion_recursos = `INS-1-1`, capacidad_recaudo = `INS-2-2`, Cobertura_energia = `INF-1-3`, vias_primarias = `INF-2-1`, costo_transporte_interno = `INF-3-1`, costo_transporte_aduana = `INF-3-2`, densidad_empresarial = `INN-3-2`, tasa_glob_participacion = `LAB-1-1`, brecha_laboral_genero = `LAB-2-1`, tamaño_mercado_interno = `TAM-1-1`, tamaño_mercado_externo = `TAM-2-1`, investigacion = `INN-1-1`, IDC = `Puntaje_general`) %>% 
  transform(vias_primarias = as.numeric(vias_primarias))
Datos_IDC_Final <- transform(Datos_IDC_1, transporte_mercado_interno= as.numeric(costo_transporte_interno))
## uniremos bases de datos con un left join co  base en el codigo_dane de departamento
base_final <- left_join(x= base_manufacturera_final, y= Datos_IDC_Final, by=c("codigo_dane"))
view(base_final)
base_final_1 <- base_final %>% filter(valor_agregado > 0) %>%
                filter(total_trabajadores > 10) %>% 
                mutate(log_valagri = log(valor_agregado),
                       log_gastostrans_insumos = log(gastostransporte_insumos),
                       log_gastostrans_productos = log(gastostransp_productos))
base_final_2 <- base_final_1 %>%
                filter(gastostransp_productos > 0 ) %>% 
               filter(gastostransporte_insumos > 0)
export(base_final_1, "Build/Outputs/base_investigacion.xlsx")
export(base_final_2, "Build/Outputs/base_investigación_filtro_gastostransporte.xlsx")
