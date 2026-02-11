rm(list = ls())

#source(file = "scripts/limpieza y recode.R")

library(tidyverse)

datos <- read_csv("datos/datos_limpios.csv")

library(MASS)
library(tidyr)
library(ordinal)
library(stargazer)
library(Matrix)
library(lme4)
library(fastDummies)
library(estimatr)
library(marginaleffects)
library(modelsummary)
library(broom.mixed)
library(flextable)
library(officer)
library(patchwork) 


# Acomodamos factores -----------------------------------------------------

###
###
###
###
###
###

###
###
###
###
###
###

datosm <- datos %>%
  # PROV YEAR
  mutate(provyear = paste(prov, year, sep = " ")) %>% 
  # RENOMBRO
  rename(SEXO = q1,
         NED = ed,
         EDAD = q2,
         ideo = l1,
         household_income = q10er,
         local_income = q10e_prop) %>% 
  # CATEGORIAS
  mutate(partyidgober = ifelse(partyidgober == 0, "NO", "SI"),
         partyidpresi = ifelse(partyidpresi == 0, "NO", "SI"),
         copartisan = ifelse(copartisan == 0, "NO", "SI"),
         peronista = ifelse(peronista == 0, "No", "Yes"),
         nacional = case_when(fed1 == "El Presidente" ~ 1,
                              is.na(fed1) ~ 0,
                              TRUE ~ 0),
         provincial = case_when(fed2 == "El Gobernador" ~ 1,
                                is.na(fed2) ~ 0,
                                TRUE ~ 0),
         municipal = case_when(fed3 == "El Intendente" ~ 1,
                               is.na(fed3) ~ 0,
                               TRUE ~ 0),
         atrib_respon = nacional + provincial + municipal,
         pol1b = case_when(pol1 == "Nada" ~ 0,
                           pol1 == "Poco" ~ 1,
                           pol1 == "Algo" ~ 2,
                           pol1 == "Mucho" ~ 3),
         aprobacion = case_when(m1 == "Muy malo" ~ 1,
                                m1 == "Malo" ~ 2,
                                m1 == "Ni bueno ni malo" ~ 3,
                                m1 == "Bueno" ~ 4,
                                m1 == "Muy bueno" ~ 5),
         aprobaciong = case_when(m10 == "Muy malo" ~ 1,
                                m10 == "Malo" ~ 2,
                                m10 == "Ni bueno ni malo" ~ 3,
                                m10 == "Bueno" ~ 4,
                                m10 == "Muy bueno" ~ 5),
         pbgnac = case_when(year == 2012 ~ 585086/(41733000/1000),
                            year == 2014 ~ 587117/(42670000/1000),
                            year == 2017 ~ 604787/(44045000/1000)))%>%
  mutate(pbgdif = pbgpm - pbgnac) %>% 
  # FACTORES
  mutate(SEXO = as.factor(SEXO),
         #egoretro = as.factor(idio2),
         #socioretro = as.factor(soct2),
         #escuelas = as.factor(sd3new2),
         #salud = as.factor(sd6new2),
         partyidgober = as.factor(partyidgober),
         partyidpresi = as.factor(partyidpresi),
         copartisan = as.factor(copartisan)) %>% 
  # REORDENO FACTRES
  #mutate(socioretro = relevel(socioretro, ref = "Peor"),
  #       egoretro = relevel(egoretro, ref = "Peor"),
  #       escuelas = relevel(escuelas, ref = "Muy insatisfecho(a)"),
  #       salud = relevel(salud, ref = "Muy insatisfecho(a)")) %>% 
  dplyr::select(prov, year, provyear, SEXO, NED, EDAD, quintil,
                household_income, presidente, gobernador,
                #socioretro, egoretro,
                soct2r, idio2r, atrib_respon, pol1b, aprobacion, aprobaciong,
                #escuelas, salud,
                local_income, desempleo, desempleo2, desempleo3, dependencia, industrializacion,
                pbgpm, pbgnac, pbgdif, pbggw, pbggw2, pbggw3, pbggw4, tfpc,
                dependencia_fiscal1, dependencia_fiscal2, dependencia_fiscal_cat_1, dependencia_fiscal_cat_2,
                dependencia_fiscal_ratio1, dependencia_fiscal_ratio2, dependencia_fiscal_cat_ratio1, dependencia_fiscal_cat_ratio2,
                ideo, partyidgober, partyidpresi, copartisan, fpv, ofi, meses, desempleo1, peronista
                #corrupcion, inseguridad, auh, ayuda, servicios
  )

###
###
###
###
###
###

###
###
###
###
###
###

###
###
###
###
###
###

###
###
###
###
###
###


# Ego y socio por pool ----------------------------------------------------


# --- PASO 2: PREPARAR LOS DATOS ---

# Se seleccionan, transforman y limpian los datos.
datos_para_grafico <- datos %>%
  dplyr::select(year,
                `National Economy` = soct2,
                `Personal Economy` = idio2) %>%
  pivot_longer(
    cols = c("National Economy", "Personal Economy"),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response)) %>%
  # Se traducen las respuestas. La clave está en la siguiente línea.
  # Se crea un factor con un orden de niveles explícito. Este orden
  # ("Better", "Same", "Worse") dictará el orden de apilado de abajo hacia arriba.
  mutate(
    Response = recode(Response,
                      "Peor" = "Worse",
                      "Igual" = "Same",
                      "Mejor" = "Better"),
    Response = factor(Response, levels = c("Better", "Same", "Worse"))
  )

# Se crea un data frame separado para calcular las posiciones de las etiquetas.
datos_con_etiquetas <- datos_para_grafico %>%
  # Se cuentan las respuestas para cada grupo.
  count(year, Question, Response) %>%
  group_by(year, Question) %>%
  # Se ordenan las filas según el mismo orden de niveles del factor.
  # Este paso es crucial para que 'cumsum' funcione correctamente.
  arrange(desc(Response)) %>%
  mutate(
    proportion = n / sum(n),
    label_position = cumsum(proportion) - 0.5 * proportion
  )

# --- PASO 3: CONSTRUIR EL GRÁFICO ---

ggplot(datos_para_grafico, aes(x = factor(year), fill = Response)) +
  
  geom_bar(position = "fill") +
  
  geom_text(
    data = datos_con_etiquetas,
    aes(y = label_position, label = scales::percent(proportion, accuracy = 1)),
    color = "white",
    # El tamaño de las etiquetas de porcentaje se aumentó aquí
    size = 4.5
  ) +
  
  facet_wrap(~ Question) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Better" = "#00bfc4",
                               "Same" = "#999999",
                               "Worse" = "#f8766d")) +
  labs(
    x = "Year of Survey",
    y = "Proportion of Responses",
    fill = "Opinion"
  ) +
  # El tamaño base para todo el texto del tema (ejes, leyendas, etc.) se aumentó aquí
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom")

ggsave(
  filename = "graficos/economiaxanio.png",
  plot = last_plot(),
  width = 10,      # Ancho de la imagen en pulgadas
  height = 6,      # Alto de la imagen en pulgadas
  units = "in",
  dpi = 300        # Resolución (300 dpi es estándar para alta calidad)
)



# dependencia fiscal histograma -------------------------------------------


ggplot(datos, aes(x = dependencia_fiscal_ratio2)) +
  geom_histogram(binwidth = 0.3, fill = "#00bfc4", color = "black") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#f8766d", size = 1.3) + # Línea vertical en x=1
  labs(
    # title = "Distribution of Fiscal Dependency Ratio", # Título comentado como en tu ejemplo
    x = "Provincial/National Revenue Ratio",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 16) + # Se aplica el tamaño base de la fuente
  theme(
    # Puedes añadir más ajustes de tema si deseas alinear otros elementos como la leyenda, etc.
    # Por ejemplo, si tuvieras una leyenda en el histograma y quisieras que fuera abajo:
    # legend.position = "bottom"
  )

ggsave(
  filename = "graficos/depfiscal.png",
  plot = last_plot(),
  width = 10,      # Ancho de la imagen en pulgadas
  height = 6,      # Alto de la imagen en pulgadas
  units = "in",
  dpi = 300        # Resolución (300 dpi es estándar para alta calidad)
)

#ggplot(datosm_scaled, aes(x = dependencia_fiscal_ratio2_z)) +
#  geom_density(fill = "lightgreen", alpha = 0.7) +
#  labs(title = "Densidad del Ratio de Dependencia Fiscal",
#       x = "Ratio de Ingresos Provinciales/Locales",
#       y = "Densidad") +
#  theme_minimal()

#ggplot(datos, aes(y = dependencia_fiscal_ratio2)) +
#  geom_boxplot(fill = "lightcoral", color = "darkred") +
#  labs(title = "Boxplot del Ratio de Dependencia Fiscal",
#       y = "Ratio de Ingresos Provinciales/Locales") +
#  theme_minimal()




# dependencia fiscal mapa -------------------------------------------------


library(sf)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(patchwork) # Para organizar los subplots

provincias_sf <- st_read("datos/shapesdepto/geo_depto.shp")

provincias_sf <- provincias_sf %>%
  # Filtramos quitando la Antártida y los NA que se ven al final de tu captura
  filter(depto != "Antártida Argentina", 
         !is.na(depto)) %>% 
  # 3. COLAPSAR (DISSOLVE): De Departamentos -> Provincias
  group_by(prov) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup() %>%
  # Renombramos la columna 'prov' a 'nam' para que coincida con tu código anterior
  # O simplemente usamos 'prov' en el join más adelante.
  rename(nam = prov)

eaif <- read.csv("datos/eaif_provincial.csv", header = TRUE, sep = ",", dec = ",", stringsAsFactors = FALSE)

eaif$dependencia_fiscal <- round(eaif$dependencia_fiscal*100, 2)


eaif_mismoanio <- eaif %>%
  filter(year != 2011) %>%
  mutate(year = case_when(year == 2013  ~ 2014,
                          T ~ year)) %>%
  rename(dependencia_fiscal2 = dependencia_fiscal,
         dependencia_fiscal_ratio2 = dependencia_fiscal_ratio)%>%
  dplyr::select(prov, year, dependencia_fiscal2, dependencia_fiscal_ratio2)


eaif_mismoanio <- eaif_mismoanio %>%
  mutate(prov = recode(prov,
                       "Buenos Aires" = "Buenos Aires", # Ya tiene acento
                       "CABA" = "Ciudad Autónoma de Buenos Aires",
                       "Catamarca" = "Catamarca", # Ya tiene acento
                       "Chaco" = "Chaco",
                       "Chubut" = "Chubut",
                       "Cordoba" = "Córdoba",
                       "Corrientes" = "Corrientes",
                       "Entre Rios" = "Entre Ríos",
                       "Formosa" = "Formosa",
                       "Jujuy" = "Jujuy",
                       "La Pampa" = "La Pampa",
                       "La Rioja" = "La Rioja",
                       "Mendoza" = "Mendoza",
                       "Misiones" = "Misiones",
                       "Neuquen" = "Neuquén",
                       "Rio Negro" = "Río Negro",
                       "Salta" = "Salta",
                       "San Juan" = "San Juan",
                       "San Luis" = "San Luis",
                       "Santa Cruz" = "Santa Cruz",
                       "Santa Fe" = "Santa Fe",
                       "Santiago del Estero" = "Santiago del Estero",
                       "Tierra del Fuego" = "Tierra del Fuego",
                       "Tucuman" = "Tucumán"))

# Unir los datos de EAIF con los datos espaciales
# Es crucial que el resultado de la unión sea un objeto sf para que geom_sf funcione.
# Para esto, uniremos `eaif_mismoanio` con la tabla de atributos de `provincias_sf`,
# y luego convertiremos el resultado de nuevo a sf.

# Primero, haz un `left_join` para traer los datos a `provincias_sf`
provincias_datos_sf <- provincias_sf %>%
  left_join(eaif_mismoanio, by = c("nam" = "prov")) %>%
  # Asegúrate de que siga siendo un objeto sf después del join
  st_sf()

years_to_map <- unique(provincias_datos_sf$year)
plot_list <- list()

for (y in years_to_map) {
  # Filtra los datos, asegurando que sigues trabajando con un objeto sf
  data_for_plot <- provincias_datos_sf %>%
    filter(year == y)
  
  # Solo grafica si hay datos para el año y si es un objeto sf válido
  if (nrow(data_for_plot) > 0 && "sf" %in% class(data_for_plot)) {
    p <- data_for_plot %>%
      ggplot() +
      # Aquí no necesitas aes(geometry) ya que geom_sf lo infiere si el objeto es sf
      geom_sf(aes(fill = dependencia_fiscal_ratio2), color = "white", size = 0.2) +
      scale_fill_gradient(
        low = "lightblue",
        high = "darkblue",
        name = "Dependencia Fiscal"
      ) +
      labs(
        title = paste("Dependencia Fiscal Provincial en", y),
        subtitle = "Ingresos Totales Provinciales / Ingresos Totales"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right"
      )
    
    plot_list[[as.character(y)]] <- p
  } else {
    warning(paste("No hay datos válidos para el año", y, "o el objeto no es sf."))
  }
}

# Combina los plots solo si la lista no está vacía
if (length(plot_list) > 0) {
  mapa_final <- wrap_plots(plot_list, ncol = 3) # Usar wrap_plots es más robusto con listas
  print(mapa_final)
  
  ggsave("graficos/mapa.png", mapa_final, width = 15, height = 7, dpi = 300)
} else {
  message("No se generaron mapas debido a la falta de datos válidos.")
}



# sin antartida y solo 2017

library(sf)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(patchwork)

# 1. CONFIGURACIÓN
sf_use_s2(FALSE)

# 2. CARGA Y PROCESAMIENTO GEOGRÁFICO
provincias_sf <- st_read("datos/shapesdepto/geo_depto.shp")

provincias_sf <- provincias_sf %>%
  # Quitamos Antártida
  filter(depto != "Antártida Argentina") %>%
  
  # === CORRECCIÓN INTELIGENTE DE NOMBRES ===
  mutate(prov = case_when(
    # 1. CABA: Si dice "Comuna", o la provincia es una variante de CABA
    grepl("Comuna", depto) ~ "Ciudad Autónoma de Buenos Aires",
    prov %in% c("CABA", "Capital Federal", "Ciudad de Buenos Aires") ~ "Ciudad Autónoma de Buenos Aires",
    
    # 2. DELTA (Isla): Solo corregimos si la provincia está VACÍA (NA)
    # Si la provincia ya dice "Chaco" o "Misiones", NO LO TOCAMOS.
    is.na(prov) & depto %in% c("San Fernando", "Campana", "Tigre", "Zárate", "Baradero", "San Nicolás", "Ramallo", "San Pedro") ~ "Buenos Aires",
    
    # 3. Resto: Mantenemos lo que venía en el shape
    TRUE ~ prov
  )) %>%
  
  # Limpieza de geometría
  st_make_valid() %>%
  st_buffer(dist = 0.01) %>% 
  
  # FUSIONAR (Dissolve)
  group_by(prov) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup() %>%
  
  # Reparación final
  st_make_valid() %>%
  rename(nam = prov)

sf_use_s2(TRUE)

# 3. DATOS ECONÓMICOS
eaif <- read.csv("datos/eaif_provincial.csv", header = TRUE, sep = ",", dec = ",", stringsAsFactors = FALSE)
eaif$dependencia_fiscal <- round(eaif$dependencia_fiscal*100, 2)

eaif_mismoanio <- eaif %>%
  filter(year != 2011) %>%
  mutate(year = case_when(year == 2013  ~ 2014, TRUE ~ year)) %>%
  rename(dependencia_fiscal2 = dependencia_fiscal,
         dependencia_fiscal_ratio2 = dependencia_fiscal_ratio) %>%
  dplyr::select(prov, year, dependencia_fiscal2, dependencia_fiscal_ratio2)

eaif_2017 <- eaif_mismoanio %>%
  mutate(prov = recode(prov,
                       "Buenos Aires" = "Buenos Aires", 
                       "CABA" = "Ciudad Autónoma de Buenos Aires",
                       "Catamarca" = "Catamarca",
                       "Chaco" = "Chaco",
                       "Chubut" = "Chubut",
                       "Cordoba" = "Córdoba",
                       "Corrientes" = "Corrientes",
                       "Entre Rios" = "Entre Ríos",
                       "Formosa" = "Formosa",
                       "Jujuy" = "Jujuy",
                       "La Pampa" = "La Pampa",
                       "La Rioja" = "La Rioja",
                       "Mendoza" = "Mendoza",
                       "Misiones" = "Misiones",
                       "Neuquen" = "Neuquén",
                       "Rio Negro" = "Río Negro",
                       "Salta" = "Salta",
                       "San Juan" = "San Juan",
                       "San Luis" = "San Luis",
                       "Santa Cruz" = "Santa Cruz",
                       "Santa Fe" = "Santa Fe",
                       "Santiago del Estero" = "Santiago del Estero",
                       "Tierra del Fuego" = "Tierra del Fuego",
                       "Tucuman" = "Tucumán")) %>% 
  filter(year == 2017)

# 4. UNIÓN
provincias_datos_sf_2017 <- provincias_sf %>%
  left_join(eaif_2017, by = c("nam" = "prov")) %>%
  st_sf()

# 5. GRAFICAR
if (nrow(provincias_datos_sf_2017) > 0) {
  
  mapa_2017 <- provincias_datos_sf_2017 %>%
    ggplot() +
    geom_sf(aes(fill = dependencia_fiscal_ratio2), color = NA) +
    geom_sf(fill = NA, color = "white", size = 0.2) +
    
    scale_fill_gradient(
      low = "lightblue",
      high = "darkblue"
    ) +
    labs(
      title = "Argentina Fiscal Dependency",
      subtitle = "Provincial revenues / National revenues",
      fill = "Fiscal Dependency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "right"
    )
  
  print(mapa_2017)
  
  ggsave("graficos/mapa_2017.png", mapa_2017, width = 10, height = 7, dpi = 300)
}



# solo 2017 y con caba separado

library(sf)
library(dplyr)
library(ggplot2)
library(patchwork) 

# --- 1. CONFIGURACIÓN GEOMÉTRICA ---
# Desactivamos la geometría esférica para poder "aplanar" y arreglar los polígonos
sf_use_s2(FALSE)

# --- 2. CARGA Y PROCESAMIENTO GEOGRÁFICO ---
provincias_sf <- st_read("datos/shapesdepto/geo_depto.shp")

provincias_sf <- provincias_sf %>%
  # Quitamos Antártida. Mantenemos filas con NA por ahora (pueden ser islas importantes)
  filter(depto != "Antártida Argentina") %>%
  
  # === CORRECCIÓN DE NOMBRES Y ASIGNACIÓN DE PROVINCIAS ===
  mutate(prov = case_when(
    # A. CABA: Detectamos por nombre de Comuna o variantes de nombre de provincia
    grepl("Comuna", depto) ~ "Ciudad Autónoma de Buenos Aires",
    prov %in% c("CABA", "Capital Federal", "Ciudad de Buenos Aires") ~ "Ciudad Autónoma de Buenos Aires",
    
    # B. DELTA DEL PARANÁ (Islas):
    # Solo si la provincia es NA y el depto es uno de los conocidos del Delta,
    # lo asignamos a Buenos Aires. Si ya dice "Entre Ríos" o "Santa Fe", NO se toca.
    is.na(prov) & depto %in% c("San Fernando", "Campana", "Tigre", "Zárate", "Baradero", "San Nicolás", "Ramallo", "San Pedro") ~ "Buenos Aires",
    
    # C. Resto: Se mantiene igual
    TRUE ~ prov
  )) %>%
  
  # === REPARACIÓN DE GEOMETRÍA (EL "PEGAMENTO") ===
  st_make_valid() %>%
  # Buffer de 0.01 (aprox 1km): Expande los polígonos para que se solapen y tapen agujeros/rayas
  st_buffer(dist = 0.01) %>% 
  
  # === FUSIÓN (DISSOLVE) ===
  group_by(prov) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup() %>%
  
  # Reparación final tras la unión
  st_make_valid() %>%
  rename(nam = prov)

# Reactivamos geometría esférica (buena práctica para el ploteo final)
sf_use_s2(TRUE)

# --- 3. DATOS ECONÓMICOS ---
eaif <- read.csv("datos/eaif_provincial.csv", header = TRUE, sep = ",", dec = ",", stringsAsFactors = FALSE)
eaif$dependencia_fiscal <- round(eaif$dependencia_fiscal*100, 2)

eaif_mismoanio <- eaif %>%
  filter(year != 2011) %>%
  mutate(year = case_when(year == 2013  ~ 2014, TRUE ~ year)) %>%
  rename(dependencia_fiscal2 = dependencia_fiscal,
         dependencia_fiscal_ratio2 = dependencia_fiscal_ratio) %>%
  dplyr::select(prov, year, dependencia_fiscal2, dependencia_fiscal_ratio2)

eaif_2017 <- eaif_mismoanio %>%
  mutate(prov = recode(prov,
                       "Buenos Aires" = "Buenos Aires", 
                       "CABA" = "Ciudad Autónoma de Buenos Aires",
                       "Catamarca" = "Catamarca",
                       "Chaco" = "Chaco",
                       "Chubut" = "Chubut",
                       "Cordoba" = "Córdoba",
                       "Corrientes" = "Corrientes",
                       "Entre Rios" = "Entre Ríos",
                       "Formosa" = "Formosa",
                       "Jujuy" = "Jujuy",
                       "La Pampa" = "La Pampa",
                       "La Rioja" = "La Rioja",
                       "Mendoza" = "Mendoza",
                       "Misiones" = "Misiones",
                       "Neuquen" = "Neuquén",
                       "Rio Negro" = "Río Negro",
                       "Salta" = "Salta",
                       "San Juan" = "San Juan",
                       "San Luis" = "San Luis",
                       "Santa Cruz" = "Santa Cruz",
                       "Santa Fe" = "Santa Fe",
                       "Santiago del Estero" = "Santiago del Estero",
                       "Tierra del Fuego" = "Tierra del Fuego",
                       "Tucuman" = "Tucumán")) %>% 
  filter(year == 2017)

# --- 4. UNIÓN DE DATOS ---
provincias_datos_sf_2017 <- provincias_sf %>%
  left_join(eaif_2017, by = c("nam" = "prov")) %>%
  st_sf()

# Calculamos límites globales para asegurar que el zoom y el mapa tengan el mismo color
rango_min <- min(provincias_datos_sf_2017$dependencia_fiscal_ratio2, na.rm = TRUE)
rango_max <- max(provincias_datos_sf_2017$dependencia_fiscal_ratio2, na.rm = TRUE)

# --- 5. GENERACIÓN DEL GRÁFICO ---
if (nrow(provincias_datos_sf_2017) > 0) {
  
  # A. MAPA PRINCIPAL (ARGENTINA)
  mapa_arg <- provincias_datos_sf_2017 %>%
    ggplot() +
    # Capa de relleno (color=NA quita bordes internos y manchas)
    geom_sf(aes(fill = dependencia_fiscal_ratio2), color = NA) +
    # Capa de contorno blanco estético
    geom_sf(fill = NA, color = "white", size = 0.2) +
    
    scale_fill_gradient(
      low = "lightblue",
      high = "darkblue",
      limits = c(rango_min, rango_max) # Escala fijada
    ) +
    labs(
      #title = "Argentina Fiscal Dependency",
      #subtitle = "Provincial revenues / National revenues",
      fill = "Fiscal Dependency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "right"
    )
  
  # B. MAPA ZOOM (CABA)
  caba_sf <- provincias_datos_sf_2017 %>% 
    filter(nam == "Ciudad Autónoma de Buenos Aires")
  
  mapa_caba <- ggplot(caba_sf) +
    geom_sf(aes(fill = dependencia_fiscal_ratio2), color = NA) +
    geom_sf(fill = NA, color = "black", size = 0.5) + # Borde negro para el zoom
    scale_fill_gradient(
      low = "lightblue",
      high = "darkblue",
      limits = c(rango_min, rango_max) # Misma escala
    ) +
    theme_void() + # Sin ejes ni fondo
    theme(
      legend.position = "none",
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  # C. COMPOSICIÓN FINAL
  # Posicionamos el zoom a la derecha, altura Entre Ríos
  mapa_final <- mapa_arg + 
    inset_element(
      mapa_caba, 
      left = 0.80,   
      bottom = 0.58, 
      right = 0.90,  
      top = 0.68     
    )}

print(mapa_final)

ggsave("graficos/mapa_2017_con_caba.png", mapa_final, width = 10, height = 7, dpi = 300)






# Le agregamos malvinas (contorno natural) - el shape original tiene a las islas dentro de antartida




library(rnaturalearth) # Necesario para traer la silueta limpia de las islas

if (nrow(provincias_datos_sf_2017) > 0) {
  
  # 1. Recuperamos el valor de dependencia de Tierra del Fuego para pintar las islas igual
  valor_tdf <- provincias_datos_sf_2017 %>% 
    filter(nam == "Tierra del Fuego") %>% 
    pull(dependencia_fiscal_ratio2)
  
  # 2. Descargamos la geometría de Malvinas (Paquete rnaturalearth)
  # Nota: returnclass = "sf" nos devuelve un objeto compatible con ggplot
  malvinas_sf <- ne_countries(scale = "large", country = "Falkland Islands", returnclass = "sf") %>%
    mutate(dependencia_fiscal_ratio2 = valor_tdf) # Le asignamos el valor de TDF
  
  # A. MAPA PRINCIPAL (ARGENTINA) - Igual que antes
  mapa_arg <- provincias_datos_sf_2017 %>%
    ggplot() +
    geom_sf(aes(fill = dependencia_fiscal_ratio2), color = NA) +
    geom_sf(fill = NA, color = "white", size = 0.2) +
    scale_fill_gradient(
      low = "lightblue", high = "darkblue",
      limits = c(rango_min, rango_max) # Escala unificada
    ) +
    labs(fill = "Fiscal\nDependency") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "right"
    )
  
  # B. ZOOM CABA - Igual que antes
  caba_sf <- provincias_datos_sf_2017 %>% 
    filter(nam == "Ciudad Autónoma de Buenos Aires")
  
  mapa_caba <- ggplot(caba_sf) +
    geom_sf(aes(fill = dependencia_fiscal_ratio2), color = NA) +
    geom_sf(fill = NA, color = "black", size = 0.5) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", limits = c(rango_min, rango_max)) +
    theme_void() +
    theme(
      legend.position = "none",
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  # C. ZOOM MALVINAS (Nuevo)
  mapa_malvinas <- ggplot(malvinas_sf) +
    geom_sf(aes(fill = dependencia_fiscal_ratio2), color = NA) +
    # Borde negro para que parezca un inset
    geom_sf(fill = NA, color = "black", size = 0.3) + 
    scale_fill_gradient(low = "lightblue", high = "darkblue", limits = c(rango_min, rango_max)) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "transparent", color = NA)
    )
  
  # D. COMPOSICIÓN FINAL
  mapa_final <- mapa_arg + 
    # CABA a la derecha (altura Entre Ríos)
    inset_element(mapa_caba, left = 0.80, bottom = 0.58, right = 0.9, top = 0.68) +
    # MALVINAS en el Atlántico Sur (sin recuadro, solo la isla con borde)
    inset_element(mapa_malvinas, left = 0.60, bottom = 0.05, right = 0.80, top = 0.25)
  
}


print(mapa_final)
ggsave("graficos/mapa_2017_con_malvinas.png", mapa_final, width = 10, height = 7, dpi = 300)



###
###
###
###
###
###

###
###
###
###
###
###


# Modelo jerarquico nulo / VPC --------------------------------------------

###
###
###
###
###
###

###
###
###
###
###
###

# Modelo nulo intercepto aleatorio gobernador

nuloj <- glmer(gobernador ~ (1 | provyear), data = datosm, family = binomial(link = "logit"))
nulo <- glm(gobernador ~ 1, data = datosm, family = binomial(link = "logit"))


summary(nuloj) # lectura: log odds de votar al gobernador en una provincia promedio es de B = 0.19
# con una variancia de 1.01
summary(nulo)
# El likelihood ratio statistic pata testear la H0 de que los residuos son igual a 0 se calculan comparando
# los dos modelos. Nulo jerarquico (intercepto aleatorio) y nulo simple
logLik(nulo)-logLik(nuloj)
# test statistic = 224.12
-2*(-112.0623) # Con 1 DF, el test estadístico da 224, hay fuerte evidencia de que la varianza
# entre los grupos no es 0
# variance partition coefficient para modelo nuloj
1.01/(1.01+3.29) # 0.23

# graficamos

u0 <- ranef(nuloj, postVar = TRUE) # genera los residuos estimados del modelo con efectos aleatorios
u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ])
commid <- as.numeric(rownames(u0[[1]]))
u0tab <- cbind("commid" = commid, "u0" = u0[[1]], "u0se" = u0se)
colnames(u0tab)[2] <- "u0"
u0tab <- u0tab[order(u0tab$u0), ]
u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))
u0tab <- u0tab[order(u0tab$commid), ]
colnames(u0tab)[4] <- "u0rank"

u0tab <- u0tab %>% 
  mutate(prov = row.names(u0tab)) %>% 
  dplyr::select(-c(commid)) %>% 
  mutate(upper = u0 + u0se*1.96,
         lower = u0 - u0se*1.96)

prov <- u0tab$prov

ggplot(u0tab, aes(x=factor(prov, levels = prov), y = u0)) + # me muestra si los residuos son diferentes de 0, siendo 0 la media de los grupos (estandarizado)
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5), width = 0.3, color = "#00bfc4") +
  geom_point(position =position_dodge(width = 0.5), color = "#00bfc4") +
  # scale_color_manual(values = cbPalette[5:6])+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "#f8766d", size=0.8)+
  theme_minimal(base_size = 16)+
  theme(axis.text.x=element_text(angle = 90, size = 10),
        axis.ticks.x=element_blank()) +
  labs(x="Provinces/Years",
       y="Residuals")

ggsave(
  filename = "graficos/residuals.png",
  plot = last_plot(),
  width = 10,      # Ancho de la imagen en pulgadas
  height = 6,      # Alto de la imagen en pulgadas
  units = "in",
  dpi = 300        # Resolución (300 dpi es estándar para alta calidad)
)

###
###
###
###
###
###

###
###
###
###
###
###


# base con datos escalados ------------------------------------------------

###
###
###
###
###
###

###
###
###
###
###
###

# escalamos cuantitativas para que converja el modelo. Ahora la interpretación
# es cambio en log-odd de VD por cada desvio de edad y ned

datosm_scaled <- datosm %>%
  mutate(
    NED_z = as.numeric(scale(NED)),
    EDAD_z = as.numeric(scale(EDAD)),
    desempleo_z = as.numeric(scale(desempleo)),
    #desempleo1_z = as.numeric(scale(desempleo1)),
    desempleo2_z = as.numeric(scale(desempleo2)),
    desempleo3_z = as.numeric(scale(desempleo3)),
    industrializacion_z = as.numeric(scale(industrializacion)),
    dependencia_fiscal1_z = as.numeric(scale(dependencia_fiscal1)),
    dependencia_fiscal2_z = as.numeric(scale(dependencia_fiscal2)),
    dependencia_fiscal_ratio1_z = as.numeric(scale(dependencia_fiscal_ratio1)),
    dependencia_fiscal_ratio2_z = as.numeric(scale(dependencia_fiscal_ratio2)),
    pbgpm_z = as.numeric(scale(pbgpm)),
    pbggw_z = as.numeric(scale(pbggw)),
    pbggw2_z = as.numeric(scale(pbggw2)),
    pbggw3_z = as.numeric(scale(pbggw3)),
    pbggw4_z = as.numeric(scale(pbggw4)),
    pbgdif_z = as.numeric(scale(pbgdif)),
    tfpc_z = as.numeric(scale(tfpc)),
    local_income_z = as.numeric(scale(local_income))
  )

summarize_data <- function(df) {
  for (col in names(df)) {
    if (is.factor(df[[col]]) || is.character(df[[col]])) {
      cat("Variable Categórica:", col, "\n")
      print(table(df[[col]]))
      cat("\n")
    } else if (is.numeric(df[[col]])) {
      cat("Variable Continua:", col, "\n")
      print(summary(df[[col]]))
      cat("\n")
    }
  }
}

str(datosm_scaled)
summarize_data(datosm_scaled)

# para evaluar cuasi separación

datosm_scaled2 <- datosm_scaled %>% 
  filter(provyear != "Santiago del Estero 2014") %>% 
  filter(provyear != "San Juan 2012")


write_csv(datosm_scaled, "data.csv")

###
###
###
###
###
###

###
###
###
###
###
###

# modelos de testeo -------------------------------------------------------


#gob = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
#               #quintil +
#               partyidgober + 
#               ideo +
#               idio2r +
#               soct2r +
#               #fpv+
#               #pol1b +
#               #atrib_respon +
#               meses +
#               pbgpm_z +
#               tfpc_z +
#               tfpc_z:pbgpm_z+
#               #dependencia_fiscal_ratio2_z +
#               copartisan +
#               copartisan:soct2r +
#soct2r:atrib_respon +
#pbgpm_z:atrib_respon +
#soct2r:pol1b +
#pbgpm_z:pol1b +
#               (1|provyear), data = datosm_scaled,
#             family = binomial(link = "logit"),
#             control = glmerControl(optimizer = "bobyqa"))


#summary(gob)


###
###
###
###
###
###

###
###
###
###
###
###


# Modelos -----------------------------------------------------------------

###
###
###
###
###
###

###
###
###
###
###
###

# Modelo 1: Modelo base (local incom + dummy pr provincia y anio separadas / nota al pie household)

gob = glm(gobernador ~ SEXO + NED_z + EDAD_z +
            #quintil +
            partyidgober + 
            ideo +
            idio2r +
            soct2r +
            #fpv+
            #ofi+
            meses +
            #household_income +
            local_income_z +
            prov +
            factor(year),
          data = datosm_scaled, family = binomial(link = "logit"))

summary(gob)


# Jerárquico


# Modelo 2: agregamos copartisan y variable fiscal


gob2 = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
               #quintil +
               partyidgober + 
               ideo +
               idio2r +
               soct2r +
               #fpv+
               #ofi+
               meses +
               local_income_z +
               dependencia_fiscal_ratio2_z +
               copartisan +
               (1|provyear), data = datosm_scaled,
             family = binomial(link = "logit"),
             control = glmerControl(optimizer = "bobyqa"))


summary(gob2)

# Modelo 2b: agregamos copartisan y variable fiscal + int peronismo ned solo para grafico


gob2b = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
               #quintil +
               peronista +
               peronista:NED_z +
               partyidgober + 
               ideo +
               idio2r +
               soct2r +
               #fpv+
               #ofi+
               meses +
               local_income_z +
               dependencia_fiscal_ratio2_z +
               copartisan +
               (1|provyear), data = datosm_scaled,
             family = binomial(link = "logit"),
             control = glmerControl(optimizer = "bobyqa"))


summary(gob2b)


# Modelo 3: modelo 2 pero con pbg

gob3 = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
               #quintil +
               partyidgober + 
               ideo +
               idio2r +
               soct2r +
               #fpv+
               #ofi+
               meses +
               pbggw +
               #pbgpm_z +
               dependencia_fiscal_ratio2_z +
               copartisan +
               (1|provyear), data = datosm_scaled,
             family = binomial(link = "logit"),
             control = glmerControl(optimizer = "bobyqa"))


summary(gob3)



# Modelo 4: Interaccion dobles / Thoery testig


gob4 = glmer(gobernador ~ SEXO + NED_z + EDAD_z + #aprobaciong + #aprobacion +
               #quintil +
               #peronista +
               #peronista:NED_z+
               partyidgober + 
               ideo +
               idio2r +
               soct2r +
               #fpv+
               #ofi+
               meses +
               pbggw +
               #pbgpm_z +
               dependencia_fiscal_ratio2_z +
               copartisan +
               dependencia_fiscal_ratio2_z:pbggw +
               copartisan:soct2r +
               (1|provyear), data = datosm_scaled,
             family = binomial(link = "logit"),
             control = glmerControl(optimizer = "bobyqa"))


summary(gob4)


# Modelo 5: Triple interacción local

gob5 = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
               #quintil +
               partyidgober + 
               ideo +
               idio2r +
               soct2r +
               #fpv+
               #ofi+
               meses +
               pbggw +
               #pbgpm_z +
               copartisan + 
               dependencia_fiscal_cat_ratio2 + 
               copartisan:pbggw: +
               copartisan:dependencia_fiscal_cat_ratio2 +
               dependencia_fiscal_cat_ratio2:pbggw +
               copartisan:dependencia_fiscal_cat_ratio2:pbggw +
               (1|provyear), data = datosm_scaled,
             family = binomial(link = "logit"),
             control = glmerControl(optimizer = "bobyqa"))


summary(gob5)


# Modelo 6: triple interacción nacional

gob6 = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
               #quintil +
               partyidgober + 
               ideo +
               idio2r +
               soct2r +
               #fpv+
               #ofi+
               meses +
               pbggw +
               #pbgpm_z +
               dependencia_fiscal_cat_ratio2 +
               copartisan + 
               copartisan:soct2r: +
               copartisan:dependencia_fiscal_cat_ratio2 +
               dependencia_fiscal_cat_ratio2:soct2r +
               copartisan:dependencia_fiscal_cat_ratio2:soct2r +
               (1|provyear), data = datosm_scaled,
             family = binomial(link = "logit"),
             control = glmerControl(optimizer = "Nelder_Mead"))
#control = glmerControl(optimizer = "bobyqa"))
summary(gob6)

###
###
###
###
###
###

###
###
###
###
###
###

# Gráfico PROB PREDICHAS todas las variables ------------------------------

###
###
###
###
###
###

###
###
###
###
###
###

# EN ESTOS GRAFICOS NO SE CONSIDERA EL EFECTO ALEATORIOD EL GRUPO PARA LOS ERRORES ESTANDAR, re.form=NULL
# POR ESO EL WARNING DE R

# --- PASO 2: CALCULAR PREDICCIONES (sin cambios) ---
pred_soct2r <- predictions(gob2, newdata = datagrid(soct2r = c(1, 2, 3)))
pred_idio2r <- predictions(gob2, newdata = datagrid(idio2r = c(1, 2, 3)))
pred_partyid <- predictions(gob2, newdata = datagrid(partyidgober = c("NO", "SI")))
pred_ideo <- predictions(gob2, newdata = datagrid(ideo = 1:10))
pred_ned <- predictions(gob2, newdata = datagrid(NED_z = seq(-2, 2, by = 0.5)))
pred_ned_peron <- predictions(gob2b, 
                              newdata = datagrid(NED_z = seq(-2, 2, by = 0.5), 
                                                 peronista = c("No", "Yes")))
# --- PASO 3: CREAR CADA GRÁFICO INDIVIDUALMENTE ---

# Se definen elementos comunes para todos los gráficos
theme_common <- theme_bw(base_size = 14) +
  theme(strip.background = element_blank(), 
        plot.title = element_text(hjust = 0, face = "bold"),
        axis.title.x = element_blank()) # No se necesita título en el eje X

# Gráfico 1: Sociotropic
p1 <- ggplot(pred_soct2r, aes(x = factor(soct2r, labels = c("Worse", "Same", "Better")), y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "#00bfc4") +
  geom_point(size = 3, color = "#00bfc4") +
  labs(title = "Sociotropic", y = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  theme_common

# Gráfico 2: Egotropic
p2 <- ggplot(pred_idio2r, aes(x = factor(idio2r, labels = c("Worse", "Same", "Better")), y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "#f8766d") +
  geom_point(size = 3, color = "#f8766d") +
  labs(title = "Egotropic", y = NULL) + # Sin etiqueta en el eje Y para no repetir
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  theme_common

# Gráfico 3: Party ID
p3 <- ggplot(pred_partyid, aes(x = factor(partyidgober, labels = c("No", "Yes")), y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "#7CAE00") +
  geom_point(size = 3, color = "#7CAE00") +
  labs(title = "Party ID", y = NULL)+ #, y = "Predicted Probability") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  theme_common

# Gráfico 4: Ideology
pred_ideo$ideo_factor <- factor(pred_ideo$ideo, levels = 1:10, labels = c("Left", 2:9, "Right"))
p4 <- ggplot(pred_ideo, aes(x = ideo_factor, y = estimate, group = 1)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "#C77CFF") +
  geom_line(color = "#C77CFF") +
  geom_point(size = 3, color = "#C77CFF") +
  labs(title = "Ideology", y = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  theme_common +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 5: Education
p5 <- ggplot(pred_ned, aes(x = NED_z, y = estimate, group = 1)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "#FF62BC") +
  geom_line(color = "#FF62BC") +
  geom_point(size = 3, color = "#FF62BC") +
  labs(title = "Education (z)", y = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  theme_common

# Gráfico 6: Panel de Referencia
#p6 <- ggplot() +
#  annotate("text", x = 0, y = 0, size = 4, color = "grey30",
#           label = "Predicted Probabilities\n95% Confidence Intervals") +
#  labs(title = "Reference") +
#  theme_bw(base_size = 14) +
#  theme(plot.title = element_text(hjust = 0, face = "bold"),
#        panel.grid = element_blank(),
#        axis.text = element_blank(),
#        axis.ticks = element_blank(),
#        axis.title = element_blank(),
#        panel.border = element_rect(color = "grey80"))


# Gráfico 7: Educación x Peronismo
p7 <- ggplot(pred_ned_peron, aes(x = NED_z, y = estimate, group = peronista, color = peronista)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = peronista), alpha = 0.1, color = NA) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Education by Gov. Party ID", y = NULL, color = "Peronist", fill = "Peronist") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  scale_color_manual(values = c("No" = "grey40", "Yes" = "#0073C2")) + # Colores sugeridos
  scale_fill_manual(values = c("No" = "grey40", "Yes" = "#0073C2")) +
  theme_common +
  theme(legend.position = "bottom") # Para que no ocupe espacio lateral


# --- PASO 4: UNIR TODOS LOS GRÁFICOS CON PATCHWORK (MÉTODO CORREGIDO) ---

# La sintaxis (p1 + p2) / (p3 + p4) / (p5 + p6) es la forma directa en patchwork
# de crear una grilla de 3 filas y 2 columnas.
# El operador '+' coloca los gráficos uno al lado del otro.
# El operador '/' apila los resultados verticalmente.
grafico_final_combinado <- (p1 + p2) / 
  (p3 + p4) / 
  (p5 + p7) #/
  #(p6)

# Muestra el gráfico combinado en la ventana de Plots de RStudio
print(grafico_final_combinado)



# --- PASO 4: UNIR CON PATCHWORK Y AGREGAR TÍTULO GLOBAL ---


# Aplicamos el título global y ajustes de diseño
grafico_final_combinado2 <- grafico_final_combinado + 
  plot_annotation(
    title = 'Predicted probabilities: 95% confidence interval',
    theme = theme(
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5, margin = margin(b = 20))
    )
  )


# --- PASO 5: GUARDAR EL GRÁFICO COMBINADO ---
# Se ajustan las dimensiones para que se adecúen mejor a una grilla vertical.
ggsave(
  filename = "graficos/predicciones_final2.png",
  plot = grafico_final_combinado2,
  width = 8,  # Ancho para dos gráficos
  height = 11, # Alto para tres filas de gráficos
  units = "in",
  dpi = 300
)


# Grafico PROD PREDICHAS interacción nacional gob 4 -----------------------

# --- PASO 2: CALCULAR LAS PROBABILIDADES PREDICHAS ---
# Se calculan las predicciones para cada combinación de 'soct2r' y 'copartisan'.
# re.form = NA se añade por buena práctica para silenciar la advertencia de los modelos mixtos.

predicciones_interaccion <- predictions(
  gob4,
  newdata = datagrid(
    soct2r = c(1, 2, 3),
    copartisan = c("NO", "SI")
  ),
  re.form = NA # Se añade para silenciar la advertencia de los modelos mixtos.
)


# --- PASO 3: PREPARAR LOS DATOS PARA EL GRÁFICO ---
# Se convierte la variable numérica 'soct2r' a un factor con etiquetas en inglés.
datos_para_grafico <- predicciones_interaccion %>%
  mutate(
    soct2r_factor = factor(soct2r, 
                           levels = c(1, 2, 3), 
                           labels = c("Worse", "Same", "Better")),
    copartisan_factor = factor(copartisan,
                               levels = c("NO", "SI"),
                               labels = c("No", "Yes"))
  )


# --- PASO 4: CONSTRUIR EL GRÁFICO ---
ggplot(datos_para_grafico, aes(x = soct2r_factor, y = estimate, color = copartisan_factor, group = copartisan)) +
  # GEOM 1: Las barras de error verticales.
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.1,
    size = 1
  ) +
  
  # GEOM 2: Las líneas que conectan los puntos.
  geom_line(size = 1.2) +
  
  # GEOM 3: Los puntos encima de las líneas y barras de error.
  geom_point(size = 3.5) +
  
  # ETIQUETAS Y TÍTULOS EN INGLÉS
  labs(
    #title = "Predicted Probability of Voting for the Governor",
    #subtitle = "Interaction between National Economic Perception and Co-partisanship",
    x = "Evaluation of the National Economy",
    y = "Probability Vote for Incumbent Governor",
    color = "Vertical copartisanship"
  ) +
  
  # ESCALAS Y TEMAS
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_color_manual(values = c("No" = "#f8766d", "Yes" = "#00bfc4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")


# --- PASO 5: GUARDAR EL GRÁFICO EN PNG ---
# Se guarda el gráfico en un archivo PNG de alta calidad y formato cuadrado.
ggsave(
  filename = "graficos/interaccion_soct2r_copartisan.png",
  plot = last_plot(),
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

###
###
###
###
###
###

###
###
###
###
###
###

# Grafico PROD PREDICHAS interacción local gob 4 -----------------------

###
###
###
###
###
###

###
###
###
###
###
###



# --- PASO 2: CALCULAR LAS PROBABILIDADES PREDICHAS ---
# Se calculan las predicciones para la interacción entre las dos variables continuas.
# Se usan valores representativos (media y +/- 1 desvío estándar) para ambas.

# 2a. Definir los valores para las variables que queremos variar
valores_pbggw <- with(datosm_scaled, mean(pbggw, na.rm = TRUE) + c(-1, 0, 1) * sd(pbggw, na.rm = TRUE))
#valores_dependencia <- with(datosm_scaled, mean(dependencia_fiscal_ratio2_z, na.rm = TRUE) + c(-0.5, 0, 0.5) * sd(dependencia_fiscal_ratio2_z, na.rm = TRUE))
valores_dependencia <- with(datosm_scaled, mean(dependencia_fiscal_ratio2_z, na.rm = TRUE) + c(-0.5, 0.5) * sd(dependencia_fiscal_ratio2_z, na.rm = TRUE))

# 2b. Crear la rejilla base solo con las variables que varían
newdata_manual <- expand.grid(
  pbggw = valores_pbggw,
  dependencia_fiscal_ratio2_z = valores_dependencia
)

# 2c. Añadir el resto de variables del modelo, fijadas a su media o moda
# !! IMPORTANTE !!: Reemplaza 'variable_problema' con el nombre que encontraste en el Paso 1.
# Si hay alguna otra variable 'factor' en tu modelo, debes añadirla aquí con su moda.
# La función Mode() calcula el valor más frecuente.
Mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

newdata_manual$SEXO <- Mode(datosm_scaled$SEXO)
newdata_manual$NED_z <- mean(datosm_scaled$NED_z, na.rm = TRUE)
newdata_manual$EDAD_z <- mean(datosm_scaled$EDAD_z, na.rm = TRUE)
newdata_manual$partyidgober <- Mode(datosm_scaled$partyidgober)
newdata_manual$ideo <- mean(datosm_scaled$ideo, na.rm = TRUE)
newdata_manual$idio2r <- mean(datosm_scaled$idio2r, na.rm = TRUE)
newdata_manual$soct2r <- mean(datosm_scaled$soct2r, na.rm = TRUE)
newdata_manual$meses <- mean(datosm_scaled$meses, na.rm = TRUE)
newdata_manual$copartisan <- Mode(datosm_scaled$copartisan)
# newdata_manual$variable_problema <- Mode(datosm_scaled$variable_problema) # <-- Si tuvieras una, la añadirías así.


# PASO 3: Calcular las Predicciones Usando la Rejilla Manual


predicciones_interaccion_cont <- predictions(
  gob4,
  newdata = newdata_manual, # Usamos nuestra rejilla manual
  re.form = NA
)


# --- PASO 4: PREPARAR LOS DATOS PARA EL GRÁFICO ---
datos_para_grafico_cont <- predicciones_interaccion_cont %>%
  mutate(
    `Fiscal Dependence Level` = factor(
      case_when(
        round(dependencia_fiscal_ratio2_z, 2) == round(min(dependencia_fiscal_ratio2_z), 2) ~ "Dependent",
        #round(dependencia_fiscal_ratio2_z, 2) == round(median(dependencia_fiscal_ratio2_z), 2) ~ "Mean",
        round(dependencia_fiscal_ratio2_z, 2) == round(max(dependencia_fiscal_ratio2_z), 2) ~ "Autonomous"
      ),
      # levels = c("Low (-0.5 SD)", "Mean", "High (+0.5 SD)")
      levels = c("Dependent", "Autonomous")
    )
  )


# --- PASO 5: CONSTRUIR Y GUARDAR EL GRÁFICO ---
grafico_interaccion_cont <- ggplot(datos_para_grafico_cont, 
                                   aes(x = pbggw, y = estimate, 
                                       color = `Fiscal Dependence Level`, 
                                       group = `Fiscal Dependence Level`)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = `Fiscal Dependence Level`), 
              alpha = 0.2, linetype = "dashed") +
  geom_line(size = 1.2) +
  geom_point(size = 3.5) +
  labs(
    #title = "Predicted Probability of Voting for the Governor",
    #subtitle = "Interaction between Local Economic Growth and Fiscal Dependence",
    x = "Provincial GDP  Growth",
    y = "Probability Vote for Incumbent Governor",
    color = "Fiscal Dependence",
    fill = "Fiscal Dependence"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")

print(grafico_interaccion_cont)

ggsave(
  filename = "graficos/interaccion_pbggw_fiscal_dependenceV2.png",
  plot = grafico_interaccion_cont,
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

###
###
###
###
###
###

###
###
###
###
###
###


# Grafico de efectos marginales gob 4 (interacción local) -----------------

###
###
###
###
###
###

###
###
###
###
###
###

library(marginaleffects)
library(ggplot2)
library(dplyr)

# --- PASO 1: Definir la secuencia ACOTADA ---
# Aquí forzamos que el eje X vaya solo de -1 a 1
seq_dependencia <- seq(-1, 1, length.out = 100)

# --- PASO 2: Calcular los Efectos Marginales ---
mfx_interaccion_acotado <- slopes(
  gob4,
  variables = "pbggw", 
  newdata = datagrid(
    # A. La variable moderadora (Eje X) con el rango acotado
    dependencia_fiscal_ratio2_z = seq_dependencia,
    
    # B. Variables fijas (Tus controles manuales)
    pbggw        = mean(datosm_scaled$pbggw, na.rm = TRUE),
    SEXO         = Mode(datosm_scaled$SEXO),
    NED_z        = mean(datosm_scaled$NED_z, na.rm = TRUE),
    EDAD_z       = mean(datosm_scaled$EDAD_z, na.rm = TRUE),
    partyidgober = Mode(datosm_scaled$partyidgober),
    ideo         = mean(datosm_scaled$ideo, na.rm = TRUE),
    idio2r       = mean(datosm_scaled$idio2r, na.rm = TRUE),
    soct2r       = mean(datosm_scaled$soct2r, na.rm = TRUE),
    meses        = mean(datosm_scaled$meses, na.rm = TRUE),
    copartisan   = Mode(datosm_scaled$copartisan)
  ),
  re.form = NA,
  type = "response"
)

# --- PASO 3: Construir el Gráfico ---
grafico_efectos_marginales <- ggplot(mfx_interaccion_acotado, aes(x = dependencia_fiscal_ratio2_z, y = estimate)) +
  
  # Línea de referencia en 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  
  # Intervalo y Línea
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#2c7fb8") +
  geom_line(color = "#2c7fb8", size = 1.2) +
  
  # Rug plot (Mostrando datos reales)
  # OJO: geom_rug usará todos los datos, pero el coord_cartesian de abajo lo cortará visualmente
  geom_rug(data = datosm_scaled, aes(x = dependencia_fiscal_ratio2_z), 
           inherit.aes = FALSE, alpha = 0.2, sides = "b") +
  
  labs(
    x = "Fiscal Dependence (z-score)",
    y = "Marginal Effect of Local Economy on Incumbent Support"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  # ESTA LÍNEA ASEGURA EL CORTE VISUAL EXACTO (Corta el Rug plot también)
  coord_cartesian(xlim = c(-1, 1)) +
  theme_bw(base_size = 14)

# Imprimir y Guardar
print(grafico_efectos_marginales)

ggsave(
  filename = "graficos/efecto_marginal_interaccion_local.png",
  plot = grafico_efectos_marginales,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)


###
###
###
###
###
###

###
###
###
###
###
###

# Graficos PROB PREDICHAS gob6 ----------------------------------------------------------------

###
###
###
###
###
###

###
###
###
###
###
###

# --- PASO 1: Cargar las Librerías Necesarias ---
library(marginaleffects)
library(ggplot2)
library(dplyr) # Necesaria para glimpse, etc.


# --- PASO 2: Crear el "Grid" de Datos para la Predicción ---
# Construimos una tabla hipotética con las combinaciones de interés.
grid_de_datos <- datagrid(
  model = gob6,
  soct2r      = sort(unique(na.omit(datosm_scaled$soct2r))),
  dependencia_fiscal_cat_ratio2 = c(0, 1),
  copartisan  = c("NO", "SI")
)


# --- PASO 3: Calcular las Predicciones ---
# Usamos el grid anterior para calcular las probabilidades predichas y sus IC.
predicciones_finales <- predictions(
  gob6,
  newdata = grid_de_datos,
  re.form = NA,
  conf_level = 0.90
)


# --- PASO INTERMEDIO: Calcular intervalos manualmente ---


predicciones_finales2 <- predicciones_finales %>% 
  mutate(conf.low = estimate - (std.error*1.96),
         conf.high = estimate + (std.error*1.96))


# --- PASO 4: Replicar el Gráfico Original ---

# Preparamos los datos para el gráfico, creando una nueva columna para el facet
# que es un factor con el orden de niveles deseado ("No Dependiente" primero).
datos_para_grafico <- predicciones_finales %>%
  mutate(
    panel_titulo = ifelse(dependencia_fiscal_cat_ratio2 == 1, "Provincia Dependiente", "Provincia No Dependiente"),
    panel_titulo = factor(panel_titulo, levels = c("Provincia No Dependiente", "Provincia Dependiente"))
  )

# Definimos el valor del dodge para usarlo consistentemente en todas las capas
dodge_width <- 0.2

# Creamos el gráfico replicando la estética de la imagen original
ggplot(datos_para_grafico, aes(x = factor(soct2r), y = estimate, color = copartisan)) +
  
  # GEOM 1: Las barras de error verticales.
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.15, # Ancho de las "alas" de la barra de error
    position = position_dodge(width = dodge_width)
  ) +
  
  # GEOM 2: Las líneas que conectan los puntos.
  geom_line(
    aes(group = copartisan),
    position = position_dodge(width = dodge_width)
  ) +
  
  # GEOM 3: Los puntos encima de las líneas y barras de error.
  geom_point(
    position = position_dodge(width = dodge_width),
    size = 2.5 # Tamaño de los puntos
  ) +
  
  # Usamos la nueva variable ordenada para el facet_wrap.
  facet_wrap(~ panel_titulo) +
  
  # Replicamos las etiquetas y títulos de la imagen.
  labs(
    title = "Probabilidad de Votar al Gobernador (Método Rápido: Delta)",
    subtitle = "Intervalos de confianza calculados con el método delta",
    x = "Opinión sobre la Economía Nacional",
    y = "Probabilidad Predicha de Voto",
    color = "Alineamiento Presidente-Gobernador"
  ) +
  
  # Replicamos los colores y las etiquetas de la leyenda.
  scale_color_manual(
    values = c("NO" = "#f8766d", "SI" = "#00bfc4"), # Rojo y Teal de ggplot
    labels = c("NO" = "NO", "SI" = "SI")
  ) +
  
  # Replicamos las etiquetas del eje X.
  scale_x_discrete(labels = c("1" = "Mala", "2" = "Regular", "3" = "Buena")) +
  
  # Replicamos la escala y el formato del eje Y.
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, NA), # Empieza en 0 y se ajusta automáticamente hacia arriba
    expand = expansion(mult = c(0, 0.05)) # Elimina espacio bajo el 0%
  ) +
  
  # Usamos el tema theme_bw() que coincide con la imagen.
  theme_bw() +
  
  # Posicionamos la leyenda en la parte inferior.
  theme(legend.position = "bottom")

###
###
###
###
###
###

###
###
###
###
###
###

# Grafico SLOPE gob 6  ----------------------------------------------------------

###
###
###
###
###
###

###
###
###
###
###
###

# --- PASO 1: CÁLCULO DE LOS EFECTOS MARGINALES (PENDIENTES) ---

# Se utiliza la función slopes() para calcular el efecto de 'soct2r' (Opinión Económica).
# El argumento 'by' asegura que se calcule una pendiente separada para cada combinación
# de 'copartisan' (Alineamiento) y 'dependencia_fiscal_cat_ratio2' (Dependencia Fiscal).
# 'conf_level = 0.90' establece los intervalos de confianza al 90%.
pendientes_economia_90 <- slopes(
  gob6,
  variables = "soct2r",
  by = c("copartisan", "dependencia_fiscal_cat_ratio2"),
  conf_level = 0.90
)

# --- PASO 2: PREPARACIÓN DE LOS DATOS PARA EL GRÁFICO ---
# Se traduce al inglés en el momento de crear la nueva columna.
datos_grafico_combinado <- pendientes_economia_90 %>%
  mutate(
    `Fiscal Dependency` = factor(
      ifelse(dependencia_fiscal_cat_ratio2 == 1, "Dependent", "Autonomous"),
      levels = c("Autonomous", "Dependent")
    )
  )

# --- PASO 3: CONSTRUCCIÓN DEL GRÁFICO CON GGPLOT2 (EN INGLÉS) ---

dodge_width <- 0.3

ggplot(datos_grafico_combinado, 
       aes(x = copartisan, y = estimate, color = copartisan, shape = `Fiscal Dependency`)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.0,
                size = 1,
                position = position_dodge(width = dodge_width)) +
  
  geom_point(size = 4.5, 
             position = position_dodge(width = dodge_width)) +
  
  # SECCIÓN DE ETIQUETAS (LABS) COMPLETAMENTE TRADUCIDA
  labs(
    #title = "Effect of National Economy on Vote, Conditional on Co-partisanship and Fiscal Dependence",
    #subtitle = "Marginal effect of 'National Economic Opinion' with 90% confidence intervals",
    x = "Vertical Copartisanship",
    y = "Impact of National Economy on Pr. Vote for Incumbent Governor",
    color = "Vertical Copartisanship", # Título para la leyenda de color
    shape = "Fiscal Dependence"   # Título para la leyenda de forma
  ) +
  
  # ESCALAS CORREGIDAS PARA TRADUCCIÓN Y LEYENDAS LIMPIAS
  
  # 1. Traduce las etiquetas del eje X
  scale_x_discrete(labels = c("NO" = "No", "SI" = "Yes")) +
  
  # 2. Controla los colores y las etiquetas de la leyenda de color
  scale_color_manual(
    values = c("NO" = "#f8766d", "SI" = "#00bfc4"),
    labels = c("NO" = "No", "SI" = "Yes")
  ) +
  
  # 3. Controla las formas y las etiquetas de la leyenda de forma
  scale_shape_manual(
    values = c("Autonomous" = 16, "Dependent" = 17)
  ) +
  
  # TEMA
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        # Se añade un pequeño margen entre las leyendas
        legend.box = "horizontal",
        legend.spacing.x = unit(0.5, 'cm'))


ggsave(
  filename = "graficos/slopes_interaccion_triple.png",
  plot = last_plot(),
  width = 10, # Ancho
  height = 8,  # Alto
  units = "in",
  dpi = 300
)


# Grafico SLOPE gob 5  ----------------------------------------------------------

###
###
###
###
###
###

###
###
###
###
###
###

# --- PASO 1: CÁLCULO DE LOS EFECTOS MARGINALES (PENDIENTES) ---


pendientes_economia_90 <- slopes(
  gob5,
  variables = "pbggw3",
  by = c("copartisan", "dependencia_fiscal_cat_ratio2"),
  conf_level = 0.90
)

# --- PASO 2: PREPARACIÓN DE LOS DATOS PARA EL GRÁFICO ---
# Se traduce al inglés en el momento de crear la nueva columna.
datos_grafico_combinado <- pendientes_economia_90 %>%
  mutate(
    `Fiscal Dependency` = factor(
      ifelse(dependencia_fiscal_cat_ratio2 == 1, "Dependent", "Non-Dependent"),
      levels = c("Non-Dependent", "Dependent")
    )
  )

# --- PASO 3: CONSTRUCCIÓN DEL GRÁFICO CON GGPLOT2 (EN INGLÉS) ---

dodge_width <- 0.3

ggplot(datos_grafico_combinado, 
       aes(x = copartisan, y = estimate, color = copartisan, shape = `Fiscal Dependency`)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.0,
                linewidth = 1,
                position = position_dodge(width = dodge_width)) +
  
  geom_point(size = 4.5, 
             position = position_dodge(width = dodge_width)) +
  
  # SECCIÓN DE ETIQUETAS (LABS) COMPLETAMENTE TRADUCIDA
  labs(
    #title = "Effect of National Economy on Vote, Conditional on Co-partisanship and Fiscal Dependence",
    #subtitle = "Marginal effect of 'National Economic Opinion' with 90% confidence intervals",
    x = "Co-partisan with President",
    y = "Impact of Local Economy on the Probability of Voting",
    color = "Co-partisanship", # Título para la leyenda de color
    shape = "Fiscal Dependency"   # Título para la leyenda de forma
  ) +
  
  # ESCALAS CORREGIDAS PARA TRADUCCIÓN Y LEYENDAS LIMPIAS
  
  # 1. Traduce las etiquetas del eje X
  scale_x_discrete(labels = c("NO" = "No", "SI" = "Yes")) +
  
  # 2. Controla los colores y las etiquetas de la leyenda de color
  scale_color_manual(
    values = c("NO" = "#f8766d", "SI" = "#00bfc4"),
    labels = c("NO" = "No", "SI" = "Yes")
  ) +
  
  # 3. Controla las formas y las etiquetas de la leyenda de forma
  scale_shape_manual(
    values = c("Non-Dependent" = 16, "Dependent" = 17)
  ) +
  
  # TEMA
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        # Se añade un pequeño margen entre las leyendas
        legend.box = "horizontal",
        legend.spacing.x = unit(0.5, 'cm'))


ggsave(
  filename = "graficos/slopes_interaccion_triple.png",
  plot = last_plot(),
  width = 10, # Ancho
  height = 8,  # Alto
  units = "in",
  dpi = 300
)

# Grafico triple interacción local gob5 (pbg y dependencia continu --------

###
###
###
###
###
###

###
###
###
###
###
###

# Se calcula el efecto (pendiente) de 'pbggw' (crecimiento del PBG local).
# Para la variable continua 'dependencia_fiscal2_z', se eligen valores representativos:
# -1 (un desvío estándar por debajo de la media), 0 (la media) y 1 (un desvío estándar por encima).
# 'copartisan' se mantiene como una variable categórica.
pendientes_economia_local <- slopes(
  gob5,  # Se usa el modelo correcto
  variables = "pbggw",
  newdata = datagrid(
    copartisan = c("NO", "SI"),
    dependencia_fiscal2_z = c(-1, 0, 1)
  ),
  conf_level = 0.90 # Se mantiene la confianza al 90%
)

# --- PASO 3: PREPARACIÓN DE LOS DATOS PARA EL GRÁFICO ---

# Se crea una nueva columna con etiquetas claras para los niveles de dependencia fiscal,
# y se convierte en un factor para controlar el orden en la leyenda.
datos_grafico_combinado_nuevo <- pendientes_economia_local %>%
  mutate(
    `Fiscal Dependence` = factor(
      case_when(
        dependencia_fiscal2_z == -1 ~ "Low (-1 SD)",
        dependencia_fiscal2_z ==  0 ~ "Mean (0)",
        dependencia_fiscal2_z ==  1 ~ "High (+1 SD)"
      ),
      levels = c("Low (-1 SD)", "Mean (0)", "High (+1 SD)")
    )
  )

# --- PASO 4: CONSTRUCCIÓN DEL GRÁFICO CON GGPLOT2 ---

# El 'dodge' sigue siendo útil para separar los puntos.
dodge_width <- 0.3

ggplot(datos_grafico_combinado_nuevo, 
       # La estética principal ahora usa 'Fiscal Dependence' para la forma.
       aes(x = copartisan, y = estimate, color = copartisan, shape = `Fiscal Dependence`)) +
  
  # La línea de referencia en cero sigue siendo crucial.
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  
  # Las barras de error y los puntos se separan con 'position_dodge'.
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.1,
                size = 1,
                position = position_dodge(width = dodge_width)) +
  
  geom_point(size = 4.5, 
             position = position_dodge(width = dodge_width)) +
  
  # Se actualizan todas las etiquetas para reflejar las nuevas variables.
  labs(
    title = "Effect of Local Economic Growth (PBG) on Voting",
    subtitle = "Conditional on Co-partisanship and Fiscal Dependence (90% CI)",
    x = "Co-partisan with Governor",
    y = "Impact of Local Economic Growth (pbggw)",
    color = "Co-partisanship",
    shape = "Fiscal Dependence"
  ) +
  
  # Se mantienen los colores para 'copartisan'.
  scale_color_manual(values = c("NO" = "#f8766d", "SI" = "#00bfc4")) +
  
  # Se definen tres formas distintas para los tres niveles de dependencia fiscal.
  scale_shape_manual(values = c("Low (-1 SD)" = 16, "Mean (0)" = 17, "High (+1 SD)" = 15)) +
  
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")

# Tabla -------------------------------------------------------------------

###
###
###
###
###
###

###
###
###
###
###
###

# PASO 0 y 1: CARGAR LIBRERIS Y CORRER MODELOS

# PASO 2: PREPARACIÓN PARA LA TABLA

# 2.1. Crear una lista nombrada con todos los modelos.
lista_modelos <- list(
  "M1" = gob,
  "M2 (H)" = gob2,
  "M3 (H)" = gob3,
  "M4 (H)" = gob4,
  "M5 (H)" = gob5,
  "M6 (H)" = gob6
)

# 2.2. Definir los nombres y el orden de los coeficientes (versión final y limpia).
# NOTA: Asegúrate de que los niveles de tus factores coincidan (ej. "SI" en 'partyidgoberSI').
# Puedes verificar los nombres exactos con `coef(gob8)`.
coef_map_original <- c(
  # --- Variables sociodemográficas
  'SEXOMujer'                         = 'Gender (Woman vs. Man)',
  'EDAD_z'                            = 'Age (z)',
  'NED_z'                             = 'Education (z)',
  
  # --- Variables políticas y de evaluación
  'partyidgoberSI'                    = 'Party ID (Yes vs. No)',
  'ideo'                              = 'Ideology',
  'idio2r'                            = 'Egotropic Evaluation',
  'soct2r'                            = 'Sociotropic Evaluation',
  'meses'                             = 'Month',
  'copartisanSI'                      = 'Vertical Copartisanship (Yes vs. No)',
  
  # --- Variables económicas y fiscales
  'household_income'                  = 'Household Income',
  'local_income'                      = 'Local Income',
  'local_income_z'                    = 'Local Income (z)',
  'pbgpm_z'                           = 'GGP p/c (z)',
  'dependencia_fiscal_ratio2_z'       = 'Fiscal Dependence (z)',
  'dependencia_fiscal_cat_ratio2'     = 'Fiscal Dependence (c)',
  
  # --- Interacciones dobles
  'soct2r:copartisanSI'                                = 'Copartisanship (Yes) x Sociotropic',
  'pbgpm_z:copartisanSI'                               = 'Copartisanship (Yes) x GGP p/c (z)',
  'soct2r:dependencia_fiscal_cat_ratio2'               = 'Fiscal Dependence (c) x Sociotropic',
  'pbgpm_z:dependencia_fiscal_ratio2_z'                = 'Fiscal Dependence (z) x GGP p/c (z)',
  'pbgpm_z:dependencia_fiscal_cat_ratio2'              = 'Fiscal Dependence (c) x GGP p/c (z)',
  'dependencia_fiscal_ratio2_z:copartisanSI'           = 'Copartisanship (Yes) x Fiscal Dependence',
  
  # --- Interacciones triples
  'soct2r:dependencia_fiscal_cat_ratio2:copartisanSI'    = 'Copartisanship x Fiscal Dep. (c) x Sociotropic ',
  'pbgpm_z:copartisanSI:dependencia_fiscal_cat_ratio2'   = 'Copartisanship x Fiscal Dep. (c) x GGP p/c (z)'
)


coef_map <- c(
  'idio2r'                            = 'Egotropic Evaluation',
  'soct2r'                            = 'Sociotropic Evaluation',
  'partyidgoberSI'                    = 'Party ID (Yes vs. No)',
  'local_income_z'                    = 'Local Income (z)',
  'pbggw'                             = 'GDP growth',
  'dependencia_fiscal_ratio2_z'       = 'Fiscal Dependence (z)',
  'copartisanSI'                      = 'Vertical Copartisanship (Yes vs. No)',
  'pbggw:dependencia_fiscal_ratio2_z' = 'Fiscal Dependence (z) x GDP growth',
  'soct2r:copartisanSI'                 = 'Copartisanship (Yes) x Sociotropic',
  
  'SEXOMujer'                         = 'Gender (Woman vs. Man)',
  'EDAD_z'                            = 'Age (z)',
  'NED_z'                             = 'Education (z)',
  'ideo'                              = 'Ideology',
  'meses'                             = 'Month',
  
  'dependencia_fiscal_cat_ratio2'     = 'Fiscal Dependence (c)',
  'pbggw:dependencia_fiscal_cat_ratio2' = 'Fiscal Dependence (c) x GDP growth',
  'pbggw:copartisanSI:dependencia_fiscal_cat_ratio2'   = 'Copartisanship x Fiscal Dep. (c) x GDP growth',
  'soct2r:dependencia_fiscal_cat_ratio2'  = 'Fiscal Dependence (c) x Sociotropic',
  'soct2r:dependencia_fiscal_cat_ratio2:copartisanSI'    = 'Copartisanship x Fiscal Dep. (c) x Sociotropic '
)


# PASO 3 (MODIFICADO): GENERAR, PERSONALIZAR Y EXPORTAR LA TABLA

# Primero, creamos la tabla pero en lugar de guardarla, la asignamos a un objeto.
# Para ello, usamos output = "flextable"
tabla_ft <- modelsummary(
  lista_modelos,
  output = "flextable", # ¡Este es el cambio clave!
  title = "Binomial logistic models",
  coef_map = coef_map,
  coef_omit = "Intercept|prov|year",
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_map = c("nobs", "aic", "bic", "logLik", "r.squared.marginal", "r.squared.conditional"),
  notes = list("Log-odds coefficients are shown. Standard errors are in parentheses.",
               "Significance levels: * p < 0.1, ** p < 0.05, *** p < 0.01")
)

# Ahora que tenemos el objeto 'tabla_ft', podemos aplicarle formato
# Usaremos el operador de pipe %>% para encadenar las modificaciones

# --- Aplica las personalizaciones de la tabla (fuente, anchos, etc.) ---
# Nota: La tubería (%>%) se detiene aquí. El resultado es un objeto flextable formateado.
tabla_con_formato <- tabla_ft %>%
  
  # --- AÑADIDO: Agregar una línea de subtítulo en el encabezado ---
  # Cambia el texto dentro de 'values' por el que prefieras
  flextable::add_header_lines(values = "Dependent Variable: Vote for Governor (1=Yes)") %>%
  
  # 1. Cambiar el tamaño de la fuente para toda la tabla
  flextable::fontsize(size = 8, part = "all") %>%
  
  # 2. Ajustar el ancho de las columnas (¡LA SOLUCIÓN A LAS FILAS ALTAS!)
  #    Hacemos la primera columna (nombres) más ancha y las otras más angostas.
  #    Las unidades están en pulgadas. ¡Juega con estos números!
  flextable::width(j = 1, width = 2.5) %>%      # Columna 1 (predictores) con 2.5 pulgadas de ancho
  flextable::width(j = 2:6, width = 0.75) %>%   # Columnas 2 a 9 (modelos) con 0.75 pulgadas cada una
  
  # 3. Reducir el espacio vertical dentro de las celdas (hace las filas más compactas)
  flextable::padding(padding.top = 2, padding.bottom = 2, part = "all")

# --- Crea un objeto separado con las propiedades de la página (¡EL PASO CLAVE!) ---
# Usamos prop_section() del paquete 'officer' para definir la orientación horizontal.
propiedades_pagina <- officer::prop_section(
  #page_size = officer::page_size(orient = "landscape") # horizontal
  page_size = officer::page_size(orient = "portrait")
)

# --- Finalmente, guarda el objeto 'flextable' ya modificado en un archivo .docx ---
# Le pasamos las propiedades de la página al argumento 'pr_section'.
flextable::save_as_docx(
  tabla_con_formato, 
  path = "Tabla_Modelos_vertical.docx", # Le ponemos un nuevo nombre
  pr_section = propiedades_pagina
)

# ¡Listo! Revisa tu directorio de trabajo para el nuevo archivo.
#print("Tabla horizontal guardada exitosamente en 'Tabla_Modelos_Horizontal.docx'")

###
###
###
###
###
###

###
###
###
###
###
###

# modelos anexo -----------------------------------------------------------


###
###
###
###
###
###

###
###
###
###
###
###


# Modelo 4 para anexos agregadas e individuales de control

# desempleo 2

gob4d2 = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
                 #quintil +
                 #peronista +
                 #peronista:NED_z+
                 partyidgober + 
                 ideo +
                 idio2r +
                 soct2r +
                 #fpv+
                 #ofi+
                 meses +
                 desempleo2_z +
                 #pbgpm_z +
                 dependencia_fiscal_ratio2_z +
                 copartisan +
                 dependencia_fiscal_ratio2_z:desempleo2_z +
                 copartisan:soct2r +
                 (1|provyear), data = datosm_scaled,
               family = binomial(link = "logit"),
               control = glmerControl(optimizer = "bobyqa"))


summary(gob4d2)


# desempleo 3


gob4d3 = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
                 #quintil +
                 #peronista +
                 #peronista:NED_z+
                 partyidgober + 
                 ideo +
                 idio2r +
                 soct2r +
                 #fpv+
                 #ofi+
                 meses +
                 desempleo3_z +
                 #pbgpm_z +
                 dependencia_fiscal_ratio2_z +
                 copartisan +
                 dependencia_fiscal_ratio2_z:desempleo3_z +
                 copartisan:soct2r +
                 (1|provyear), data = datosm_scaled,
               family = binomial(link = "logit"),
               control = glmerControl(optimizer = "bobyqa"))


summary(gob4d3)


# pbg 3


gob4pbg3 = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
                   #quintil +
                   #peronista +
                   #peronista:NED_z+
                   partyidgober + 
                   ideo +
                   idio2r +
                   soct2r +
                   #fpv+
                   #ofi+
                   meses +
                   pbggw3_z +
                   #pbgpm_z +
                   dependencia_fiscal_ratio2_z +
                   copartisan +
                   dependencia_fiscal_ratio2_z:pbggw3_z +
                   copartisan:soct2r +
                   (1|provyear), data = datosm_scaled,
                 family = binomial(link = "logit"),
                 control = glmerControl(optimizer = "bobyqa"))


summary(gob4pbg3)


# pbg 4


gob4pbg4 = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
                   #quintil +
                   #peronista +
                   #peronista:NED_z+
                   partyidgober + 
                   ideo +
                   idio2r +
                   soct2r +
                   #fpv+
                   #ofi+
                   meses +
                   pbggw4_z +
                   #pbgpm_z +
                   dependencia_fiscal_ratio2_z +
                   copartisan +
                   dependencia_fiscal_ratio2_z:pbggw4_z +
                   copartisan:soct2r +
                   (1|provyear), data = datosm_scaled,
                 family = binomial(link = "logit"),
                 control = glmerControl(optimizer = "bobyqa"))


summary(gob4pbg4)


# pbg dif


gob4dif = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
                  #quintil +
                  #peronista +
                  #peronista:NED_z+
                  partyidgober + 
                  ideo +
                  idio2r +
                  soct2r +
                  #fpv+
                  #ofi+
                  meses +
                  pbgdif +
                  #pbgpm_z +
                  dependencia_fiscal_ratio2_z +
                  copartisan +
                  dependencia_fiscal_ratio2_z:pbgdif +
                  copartisan:soct2r +
                  (1|provyear), data = datosm_scaled,
                family = binomial(link = "logit"),
                control = glmerControl(optimizer = "bobyqa"))


summary(gob4dif)


# quintall


gob4q = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
                quintil +
                #peronista +
                #peronista:NED_z+
                partyidgober + 
                ideo +
                idio2r +
                soct2r +
                #fpv+
                #ofi+
                meses +
                pbggw +
                #pbgpm_z +
                dependencia_fiscal_ratio2_z +
                copartisan +
                dependencia_fiscal_ratio2_z:pbggw +
                copartisan:soct2r +
                (1|provyear), data = datosm_scaled,
              family = binomial(link = "logit"),
              control = glmerControl(optimizer = "bobyqa"))


summary(gob4q)


# voto anterior


gob4v = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
                #quintil +
                #peronista +
                #peronista:NED_z+
                partyidgober + 
                ideo +
                idio2r +
                soct2r +
                #fpv+
                ofi+
                meses +
                pbggw +
                #pbgpm_z +
                dependencia_fiscal_ratio2_z +
                copartisan +
                dependencia_fiscal_ratio2_z:pbggw +
                copartisan:soct2r +
                (1|provyear), data = datosm_scaled,
              family = binomial(link = "logit"),
              control = glmerControl(optimizer = "bobyqa"))


summary(gob4v)

# aprobacion presidencial

gob4a = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
                #quintil +
                #peronista +
                #peronista:NED_z+
                partyidgober + 
                ideo +
                idio2r +
                soct2r +
                #fpv+
                #ofi+
                aprobacion +
                meses +
                pbggw +
                #pbgpm_z +
                dependencia_fiscal_ratio2_z +
                copartisan +
                dependencia_fiscal_ratio2_z:pbggw +
                copartisan:soct2r +
                (1|provyear), data = datosm_scaled,
              family = binomial(link = "logit"),
              control = glmerControl(optimizer = "bobyqa"))


summary(gob4a)

# aprobacion gobernador

gob4b = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
                #quintil +
                #peronista +
                #peronista:NED_z+
                partyidgober + 
                ideo +
                idio2r +
                soct2r +
                #fpv+
                #ofi+
                aprobaciong +
                meses +
                pbggw +
                #pbgpm_z +
                dependencia_fiscal_ratio2_z +
                copartisan +
                dependencia_fiscal_ratio2_z:pbggw +
                copartisan:soct2r +
                (1|provyear), data = datosm_scaled,
              family = binomial(link = "logit"),
              control = glmerControl(optimizer = "bobyqa"))


summary(gob4b)



# Todos los modelos para reemplazo de economía local por subjetiva (household)

# Modelo 1: Modelo base (local incom + dummy pr provincia y anio separadas / nota al pie household)

gobh = glm(gobernador ~ SEXO + NED_z + EDAD_z +
            #quintil +
            partyidgober + 
            ideo +
            idio2r +
            soct2r +
            #fpv+
            #ofi+
            meses +
            household_income +
            #local_income_z +
            prov +
            factor(year),
          data = datosm_scaled, family = binomial(link = "logit"))

summary(gobh)


# Modelo 2: agregamos copartisan y variable fiscal


gob2h = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
               #quintil +
               partyidgober + 
               ideo +
               idio2r +
               soct2r +
               #fpv+
               #ofi+
               meses +
               household_income +
               dependencia_fiscal_ratio2_z +
               copartisan +
               (1|provyear), data = datosm_scaled,
             family = binomial(link = "logit"),
             control = glmerControl(optimizer = "bobyqa"))


summary(gob2h)


# Modelo 3: modelo 2 pero con pbg

gob3h = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
               #quintil +
               partyidgober + 
               ideo +
               idio2r +
               soct2r +
               #fpv+
               #ofi+
               meses +
               household_income +
               #pbgpm_z +
               dependencia_fiscal_ratio2_z +
               copartisan +
               (1|provyear), data = datosm_scaled,
             family = binomial(link = "logit"),
             control = glmerControl(optimizer = "bobyqa"))


summary(gob3h)



# Modelo 4: Interaccion dobles / Thoery testig


gob4h = glmer(gobernador ~ SEXO + NED_z + EDAD_z + #aprobaciong + #aprobacion +
               #quintil +
               #peronista +
               #peronista:NED_z+
               partyidgober + 
               ideo +
               idio2r +
               soct2r +
               #fpv+
               #ofi+
               meses +
               household_income +
               #pbgpm_z +
               dependencia_fiscal_ratio2_z +
               copartisan +
               dependencia_fiscal_ratio2_z:household_income +
               copartisan:soct2r +
               (1|provyear), data = datosm_scaled,
             family = binomial(link = "logit"),
             control = glmerControl(optimizer = "bobyqa"))


summary(gob4h)


# Modelo 5: Triple interacción local

gob5h = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
               #quintil +
               partyidgober + 
               ideo +
               idio2r +
               soct2r +
               #fpv+
               #ofi+
               meses +
               household_income +
               #pbgpm_z +
               copartisan + 
               dependencia_fiscal_cat_ratio2 + 
               copartisan:household_income +
               copartisan:dependencia_fiscal_cat_ratio2 +
               dependencia_fiscal_cat_ratio2:household_income +
               copartisan:dependencia_fiscal_cat_ratio2:household_income +
               (1|provyear), data = datosm_scaled,
             family = binomial(link = "logit"),
             control = glmerControl(optimizer = "bobyqa"))


summary(gob5h)


# Modelo 6: triple interacción nacional

gob6h = glmer(gobernador ~ SEXO + NED_z + EDAD_z +
               #quintil +
               partyidgober + 
               ideo +
               idio2r +
               soct2r +
               #fpv+
               #ofi+
               meses +
               household_income +
               #pbgpm_z +
               dependencia_fiscal_cat_ratio2 +
               copartisan + 
               copartisan:soct2r: +
               copartisan:dependencia_fiscal_cat_ratio2 +
               dependencia_fiscal_cat_ratio2:soct2r +
               copartisan:dependencia_fiscal_cat_ratio2:soct2r +
               (1|provyear), data = datosm_scaled,
             family = binomial(link = "logit"),
             control = glmerControl(optimizer = "Nelder_Mead"))
#control = glmerControl(optimizer = "bobyqa"))
summary(gob6h)



###
###
###
###
###
###

###
###
###
###
###
###

# Tabla anexo agregadas alternativas -------------------------------------------------------------------

###
###
###
###
###
###

###
###
###
###
###
###

# PASO 0 y 1: CARGAR LIBRERIS Y CORRER MODELOS

# PASO 2: PREPARACIÓN PARA LA TABLA

# 2.1. Crear una lista nombrada con todos los modelos.
lista_modelos <- list(
  "M1 (Unemplyment)" = gob4d2,
  "M2 (Unemployment Dif.)" = gob4d3,
  "M3 (GDP growth mean 2)" = gob4pbg3,
  "M4 (GDP growth mean 4)" = gob4pbg4,
  "M5 (GDP dif nac/prov)" = gob4dif)

# 2.2. Definir los nombres y el orden de los coeficientes (versión final y limpia).



coef_map <- c(
  'idio2r'                            = 'Egotropic Evaluation',
  'soct2r'                            = 'Sociotropic Evaluation',
  'partyidgoberSI'                    = 'Party ID (Yes vs. No)',
  'desempleo2_z'                             = 'Unemployment',
  'desempleo3_z'                             = 'Unemployment (Dif)',
  'pbggw3_z'                             = 'GDP growth mean 2',
  'pbggw4_z'                             = 'GDP growth mean 4',
  'pbgdif'                             = 'GDP dif nac/prov',
  'dependencia_fiscal_ratio2_z'       = 'Fiscal Dependence (z)',
  'copartisanSI'                      = 'Vertical Copartisanship (Yes vs. No)',
  'desempleo2_z:dependencia_fiscal_ratio2_z' = 'Fiscal Dependence (z) x Unemployment',
  'desempleo3_z:dependencia_fiscal_ratio2_z' = 'Fiscal Dependence (z) x Unemployment (Dif)',
  'pbggw3_z:dependencia_fiscal_ratio2_z' = 'Fiscal Dependence (z) x GDP growth mean 2',
  'pbggw4_z:dependencia_fiscal_ratio2_z' = 'Fiscal Dependence (z) x GDP growth mean 4',
  'pbgdif:dependencia_fiscal_ratio2_z' = 'Fiscal Dependence (z) x GDP dif nac/prov',
  'soct2r:copartisanSI'                 = 'Copartisanship (Yes) x Sociotropic',
  
  'SEXOMujer'                         = 'Gender (Woman vs. Man)',
  'EDAD_z'                            = 'Age (z)',
  'NED_z'                             = 'Education (z)',
  'ideo'                              = 'Ideology',
  'meses'                             = 'Month'
)


# PASO 3 (MODIFICADO): GENERAR, PERSONALIZAR Y EXPORTAR LA TABLA

# Primero, creamos la tabla pero en lugar de guardarla, la asignamos a un objeto.
# Para ello, usamos output = "flextable"
tabla_ft <- modelsummary(
  lista_modelos,
  output = "flextable", # ¡Este es el cambio clave!
  title = "Binomial logistic models",
  coef_map = coef_map,
  coef_omit = "Intercept|prov|year",
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_map = c("nobs", "aic", "bic", "logLik", "r.squared.marginal", "r.squared.conditional"),
  notes = list("Log-odds coefficients are shown. Standard errors are in parentheses.",
               "Significance levels: * p < 0.1, ** p < 0.05, *** p < 0.01")
)

# Ahora que tenemos el objeto 'tabla_ft', podemos aplicarle formato
# Usaremos el operador de pipe %>% para encadenar las modificaciones

# --- Aplica las personalizaciones de la tabla (fuente, anchos, etc.) ---
# Nota: La tubería (%>%) se detiene aquí. El resultado es un objeto flextable formateado.
tabla_con_formato <- tabla_ft %>%
  
  # --- AÑADIDO: Agregar una línea de subtítulo en el encabezado ---
  # Cambia el texto dentro de 'values' por el que prefieras
  flextable::add_header_lines(values = "Dependent Variable: Vote for Governor (1=Yes)") %>%
  
  # 1. Cambiar el tamaño de la fuente para toda la tabla
  flextable::fontsize(size = 8, part = "all") %>%
  
  # 2. Ajustar el ancho de las columnas (¡LA SOLUCIÓN A LAS FILAS ALTAS!)
  #    Hacemos la primera columna (nombres) más ancha y las otras más angostas.
  #    Las unidades están en pulgadas. ¡Juega con estos números!
  flextable::width(j = 1, width = 2.5) %>%      # Columna 1 (predictores) con 2.5 pulgadas de ancho
  flextable::width(j = 2:6, width = 0.75) %>%   # Columnas 2 a 9 (modelos) con 0.75 pulgadas cada una
  
  # 3. Reducir el espacio vertical dentro de las celdas (hace las filas más compactas)
  flextable::padding(padding.top = 2, padding.bottom = 2, part = "all")

# --- Crea un objeto separado con las propiedades de la página (¡EL PASO CLAVE!) ---
# Usamos prop_section() del paquete 'officer' para definir la orientación horizontal.
propiedades_pagina <- officer::prop_section(
  #page_size = officer::page_size(orient = "landscape") # horizontal
  page_size = officer::page_size(orient = "portrait")
)

# --- Finalmente, guarda el objeto 'flextable' ya modificado en un archivo .docx ---
# Le pasamos las propiedades de la página al argumento 'pr_section'.
flextable::save_as_docx(
  tabla_con_formato, 
  path = "Tabla_Modelos_vertical_anexo.docx", # Le ponemos un nuevo nombre
  pr_section = propiedades_pagina
)

# ¡Listo! Revisa tu directorio de trabajo para el nuevo archivo.
#print("Tabla horizontal guardada exitosamente en 'Tabla_Modelos_Horizontal.docx'")

###
###
###
###
###
###

###
###
###
###
###
###


# Tabla anexo quintil, voto anterior y aprobacion -------------------------------------

###
###
###
###
###
###

###
###
###
###
###
###
# PASO 0 y 1: CARGAR LIBRERIS Y CORRER MODELOS

# PASO 2: PREPARACIÓN PARA LA TABLA

# 2.1. Crear una lista nombrada con todos los modelos.
lista_modelos <- list(
  "M1 (Quintil)" = gob4q,
  "M2 (Previous Vote)" = gob4v,
  "M3 (Presidential Approval)" = gob4a,
  "M3 (Gubernatorial Approval)" = gob4b)

# 2.2. Definir los nombres y el orden de los coeficientes (versión final y limpia).



coef_map <- c(
  'idio2r'                            = 'Egotropic Evaluation',
  'soct2r'                            = 'Sociotropic Evaluation',
  'partyidgoberSI'                    = 'Party ID (Yes vs. No)',
  'quintil'                           = 'Income quintile (asset index)',
  'ofi'                               = 'Vote in the previous election',
  'aprobacion'                        = 'Presidential Approval',
  'aprobaciong'                        = 'Gubernatorial Approval',
  'pbggw'                             = 'GDP growth',
  'dependencia_fiscal_ratio2_z'       = 'Fiscal Dependence (z)',
  'copartisanSI'                      = 'Vertical Copartisanship (Yes vs. No)',
  'pbggw:dependencia_fiscal_ratio2_z' = 'GDP growth x Fiscal Dependence (z)',
  'soct2r:copartisanSI'                 = 'Copartisanship (Yes) x Sociotropic',
  
  'SEXOMujer'                         = 'Gender (Woman vs. Man)',
  'EDAD_z'                            = 'Age (z)',
  'NED_z'                             = 'Education (z)',
  'ideo'                              = 'Ideology',
  'meses'                             = 'Month'
)


# PASO 3 (MODIFICADO): GENERAR, PERSONALIZAR Y EXPORTAR LA TABLA

# Primero, creamos la tabla pero en lugar de guardarla, la asignamos a un objeto.
# Para ello, usamos output = "flextable"
tabla_ft <- modelsummary(
  lista_modelos,
  output = "flextable", # ¡Este es el cambio clave!
  title = "Binomial logistic models",
  coef_map = coef_map,
  coef_omit = "Intercept|prov|year",
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_map = c("nobs", "aic", "bic", "logLik", "r.squared.marginal", "r.squared.conditional"),
  notes = list("Log-odds coefficients are shown. Standard errors are in parentheses.",
               "Significance levels: * p < 0.1, ** p < 0.05, *** p < 0.01")
)

# Ahora que tenemos el objeto 'tabla_ft', podemos aplicarle formato
# Usaremos el operador de pipe %>% para encadenar las modificaciones

# --- Aplica las personalizaciones de la tabla (fuente, anchos, etc.) ---
# Nota: La tubería (%>%) se detiene aquí. El resultado es un objeto flextable formateado.
tabla_con_formato <- tabla_ft %>%
  
  # --- AÑADIDO: Agregar una línea de subtítulo en el encabezado ---
  # Cambia el texto dentro de 'values' por el que prefieras
  flextable::add_header_lines(values = "Dependent Variable: Vote for Governor (1=Yes)") %>%
  
  # 1. Cambiar el tamaño de la fuente para toda la tabla
  flextable::fontsize(size = 8, part = "all") %>%
  
  # 2. Ajustar el ancho de las columnas (¡LA SOLUCIÓN A LAS FILAS ALTAS!)
  #    Hacemos la primera columna (nombres) más ancha y las otras más angostas.
  #    Las unidades están en pulgadas. ¡Juega con estos números!
  flextable::width(j = 1, width = 2.5) %>%      # Columna 1 (predictores) con 2.5 pulgadas de ancho
  flextable::width(j = 2:5, width = 0.75) %>%   # Columnas 2 a 9 (modelos) con 0.75 pulgadas cada una
  
  # 3. Reducir el espacio vertical dentro de las celdas (hace las filas más compactas)
  flextable::padding(padding.top = 2, padding.bottom = 2, part = "all")

# --- Crea un objeto separado con las propiedades de la página (¡EL PASO CLAVE!) ---
# Usamos prop_section() del paquete 'officer' para definir la orientación horizontal.
propiedades_pagina <- officer::prop_section(
  #page_size = officer::page_size(orient = "landscape") # horizontal
  page_size = officer::page_size(orient = "portrait")
)

# --- Finalmente, guarda el objeto 'flextable' ya modificado en un archivo .docx ---
# Le pasamos las propiedades de la página al argumento 'pr_section'.
flextable::save_as_docx(
  tabla_con_formato, 
  path = "Tabla_Modelos_vertical_anexo2.docx", # Le ponemos un nuevo nombre
  pr_section = propiedades_pagina
)

# Tabla anexo modelos paper con household ---------------------------------

###
###
###
###
###
###

###
###
###
###
###
###

# PASO 0 y 1: CARGAR LIBRERIS Y CORRER MODELOS

# PASO 2: PREPARACIÓN PARA LA TABLA

# 2.1. Crear una lista nombrada con todos los modelos.
lista_modelos <- list(
  "M1" = gobh,
  "M2 (H)" = gob2h,
  "M3 (H)" = gob3h,
  "M4 (H)" = gob4h,
  "M5 (H)" = gob5h,
  "M6 (H)" = gob6h
)



coef_map <- c(
  'idio2r'                            = 'Egotropic Evaluation',
  'soct2r'                            = 'Sociotropic Evaluation',
  'partyidgoberSI'                    = 'Party ID (Yes vs. No)',
  'household_income'                  = 'Household Income Deterioration',
  'dependencia_fiscal_ratio2_z'       = 'Fiscal Dependence (z)',
  'copartisanSI'                      = 'Vertical Copartisanship (Yes vs. No)',
  'household_income:dependencia_fiscal_ratio2_z' = 'Fiscal Dependence (z) x HID',
  'soct2r:copartisanSI'                 = 'Copartisanship (Yes) x Sociotropic',
  
  'SEXOMujer'                         = 'Gender (Woman vs. Man)',
  'EDAD_z'                            = 'Age (z)',
  'NED_z'                             = 'Education (z)',
  'ideo'                              = 'Ideology',
  'meses'                             = 'Month',
  
  'dependencia_fiscal_cat_ratio2'     = 'Fiscal Dependence (c)',
  'household_income:dependencia_fiscal_cat_ratio2' = 'Fiscal Dependence (c) x HID',
  'household_income:copartisanSI:dependencia_fiscal_cat_ratio2'   = 'Copartisanship x Fiscal Dep. (c) x HID',
  'soct2r:dependencia_fiscal_cat_ratio2'  = 'Fiscal Dependence (c) x Sociotropic',
  'soct2r:dependencia_fiscal_cat_ratio2:copartisanSI'    = 'Copartisanship x Fiscal Dep. (c) x Sociotropic '
)


# PASO 3 (MODIFICADO): GENERAR, PERSONALIZAR Y EXPORTAR LA TABLA

# Primero, creamos la tabla pero en lugar de guardarla, la asignamos a un objeto.
# Para ello, usamos output = "flextable"
tabla_ft <- modelsummary(
  lista_modelos,
  output = "flextable", # ¡Este es el cambio clave!
  title = "Binomial logistic models",
  coef_map = coef_map,
  coef_omit = "Intercept|prov|year",
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_map = c("nobs", "aic", "bic", "logLik", "r.squared.marginal", "r.squared.conditional"),
  notes = list("Log-odds coefficients are shown. Standard errors are in parentheses.",
               "Significance levels: * p < 0.1, ** p < 0.05, *** p < 0.01")
)

# Ahora que tenemos el objeto 'tabla_ft', podemos aplicarle formato
# Usaremos el operador de pipe %>% para encadenar las modificaciones

# --- Aplica las personalizaciones de la tabla (fuente, anchos, etc.) ---
# Nota: La tubería (%>%) se detiene aquí. El resultado es un objeto flextable formateado.
tabla_con_formato <- tabla_ft %>%
  
  # --- AÑADIDO: Agregar una línea de subtítulo en el encabezado ---
  # Cambia el texto dentro de 'values' por el que prefieras
  flextable::add_header_lines(values = "Dependent Variable: Vote for Governor (1=Yes)") %>%
  
  # 1. Cambiar el tamaño de la fuente para toda la tabla
  flextable::fontsize(size = 8, part = "all") %>%
  
  # 2. Ajustar el ancho de las columnas (¡LA SOLUCIÓN A LAS FILAS ALTAS!)
  #    Hacemos la primera columna (nombres) más ancha y las otras más angostas.
  #    Las unidades están en pulgadas. ¡Juega con estos números!
  flextable::width(j = 1, width = 2.5) %>%      # Columna 1 (predictores) con 2.5 pulgadas de ancho
  flextable::width(j = 2:6, width = 0.75) %>%   # Columnas 2 a 9 (modelos) con 0.75 pulgadas cada una
  
  # 3. Reducir el espacio vertical dentro de las celdas (hace las filas más compactas)
  flextable::padding(padding.top = 2, padding.bottom = 2, part = "all")

# --- Crea un objeto separado con las propiedades de la página (¡EL PASO CLAVE!) ---
# Usamos prop_section() del paquete 'officer' para definir la orientación horizontal.
propiedades_pagina <- officer::prop_section(
  #page_size = officer::page_size(orient = "landscape") # horizontal
  page_size = officer::page_size(orient = "portrait")
)

# --- Finalmente, guarda el objeto 'flextable' ya modificado en un archivo .docx ---
# Le pasamos las propiedades de la página al argumento 'pr_section'.
flextable::save_as_docx(
  tabla_con_formato, 
  path = "Tabla_Modelos_anexo_household.docx", # Le ponemos un nuevo nombre
  pr_section = propiedades_pagina
)

# ¡Listo! Revisa tu directorio de trabajo para el nuevo archivo.
#print("Tabla horizontal guardada exitosamente en 'Tabla_Modelos_Horizontal.docx'")

