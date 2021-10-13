# Paquetes ----
options(scipen=999)
Sys.setlocale("LC_TIME", "es_ES")

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
    if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

evens <- function(x){
    vector <- c()
    for (i in 1:length(x)){
        if (x[i] %% 2 == 0){ #this was the first error line
            vector <- c(vector, x[i]) ##this was the seconds error line
        }
    }
    vector
}

if(!require("tseries")) install.packages("tseries") & require("tseries")
if(!require("forecast")) install.packages("forecast") & require("forecast")
if(!require("lubridate")) install.packages("lubridate") & require("lubridate")
if(!require("hot.deck")) install.packages("hot.deck") & require("hot.deck")
if(!require("zoo")) install.packages("zoo") & require("zoo")
if(!require("stringi")) install.packages("stringi") & require("stringi")
if(!require("gganimate")) install.packages("gganimate") & require("gganimate")
if(!require("gridExtra")) install.packages("gridExtra") & require("gridExtra")
if(!require("ggthemes")) install.packages("ggthemes") & require("ggthemes")
if(!require("hrbrthemes")) install.packages("hrbrthemes") & require("hrbrthemes")
if(!require("magick")) install.packages("magick") & require("magick")
if(!require("scales")) install.packages("scales") & require("scales")
if(!require("RColorBrewer")) install.packages("RColorBrewer") & require("RColorBrewer")
if(!require("foreign")) install.packages("foreign") & require("foreign")
if(!require("srvyr")) install.packages("srvyr") & require("srvyr")
if(!require("openxlsx")) install.packages("openxlsx") & require("openxlsx")
if(!require("ggalt")) install.packages("ggalt") & require("ggalt")


# Tidyverse <3
require(tidyverse)

# Directorio ----

paste_inp <- function(x){paste0("01_datos_crudos/", x)}
paste_out       <- function(x){paste0("02_datoas_limpios/", x)}
paste_info      <- function(x){paste0("03_infobites/", x)}

# Colores MCV -----
mcv_discrete <- c(
    "#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41"
)

mcv_semaforo <- c(
    "#00b783", # verde
    "#E8D92E", # amarillo
    "#ffbd41", # naranja
    "#ff6260" # rojo
)

mcv_blacks <- c("black", "#D2D0CD", "#777777")

# Abrir bases ----
imss_tot <- readxl::read_excel(paste_inp("01_02_tot_imss.xlsx"))%>% 
    arrange(anio, mes) 
imss_tot_ent <- readxl::read_excel(paste_inp("02_02_imss_ent.xlsx"))%>% 
    mutate(mes = as.numeric(mes))%>% 
    arrange(anio, mes) 
imss_sector1 <- readxl::read_excel(paste_inp("01_02_sector1.xlsx"))%>% 
    arrange(anio, mes) 

# Desestacionalizar ----
## Nacional ----
tempo <- imss_tot  %>% 
    filter(anio > 1997) %>% 
    mutate(periodo = zoo::as.yearmon(paste0(anio, "-", mes))) %>% 
    select(periodo, tot_imss) %>% 
    glimpse

d_ts <- ts(tempo$tot_imss, frequency = 12, start = 1998)
ggseasonplot(d_ts, main = paste0("Puestos registrados ante el IMSS"))

decomposed_d_additive <- decompose(d_ts, type = "additive")
d_desest_tempo_tempo <- seasadj(decomposed_d_additive) 


d_desest_tempo_2 <- 
    data.frame(tot_imss=as.matrix(d_desest_tempo_tempo), fecha_m=zoo::as.yearmon(time(d_desest_tempo_tempo))) 

d_desest <- d_desest_tempo_2

## Entidades ----
cves <- unique(imss_tot_ent$cve_ent)
# Primero hacer para un grupo porque si no crashea
i = 1
d_desest_cve_ent <- d_desest_tempo_2
for(i in 2:length(cves)){
    
    tempo <- imss_tot_ent %>% 
        filter(cve_ent == cves[i]) %>% 
        mutate(periodo = zoo::as.yearmon(paste0(anio, "-", mes))) %>% 
        select(periodo, tot_imss) %>% 
        glimpse
    
    d_ts <- ts(tempo$tot_imss, frequency = 12, start = 1998)
    # ggseasonplot(d_ts, main = paste0("Puestos registrados ante el IMSS (", sectores_tit[i], ")"))
    
    decomposed_d_additive <- decompose(d_ts, type = "additive")
    d_desest_tempo_tempo <- seasadj(decomposed_d_additive) 
    
    
    d_desest_tempo_2 <- 
        data.frame(tot_imss=as.matrix(d_desest_tempo_tempo), fecha_m=zoo::as.yearmon(time(d_desest_tempo_tempo))) %>% 
        mutate(cve_ent = cves[i])
    
    d_desest_cve_ent <- bind_rows(d_desest_tempo_2, d_desest_cve_ent)
    
}



## Sectores económicos ----
imss_sector1 <- imss_sector1 %>% 
    filter(anio > 1997) %>% 
    mutate(
        sector = case_when(
            sector_economico_1 == 0 ~ "Actividades primarias",
            sector_economico_1 == 1 ~ "Industrias extractivas",
            sector_economico_1 == 3 ~ "Industrias de transformación",
            sector_economico_1 == 4 ~ "Construcción",
            sector_economico_1 == 5 ~ "Electricidad y agua potable",
            sector_economico_1 == 6 ~ "Comercio",
            sector_economico_1 == 7 ~ "Transportes y comunicaciones",
            sector_economico_1 == 8 ~ "Servicios para empresas, personas y el hogar",
            sector_economico_1 == 9 ~ "Servicios sociales y comunales",
        )
    ) %>% 
    drop_na(sector)

sectores <- unique(imss_sector1$sector_economico_1)
sectores_tit <- unique(imss_sector1$sector)


# Primero hacer para un grupo porque si no crashea
i = 1
d_desest_sec <- d_desest_tempo_2
for(i in 2:length(sectores)){
    
    tempo <- imss_sector1 %>% 
        filter(sector_economico_1 == sectores[i]) %>% 
        mutate(periodo = zoo::as.yearmon(paste0(anio, "-", mes))) %>% 
        select(periodo, tot_imss) %>% 
        glimpse
    
    d_ts <- ts(tempo$tot_imss, frequency = 12, start = 1998)
    # ggseasonplot(d_ts, main = paste0("Puestos registrados ante el IMSS (", sectores_tit[i], ")"))
    
    decomposed_d_additive <- decompose(d_ts, type = "additive")
    d_desest_tempo_tempo <- seasadj(decomposed_d_additive) 
    
    
    d_desest_tempo_2 <- 
        data.frame(tot_imss=as.matrix(d_desest_tempo_tempo), fecha_m=zoo::as.yearmon(time(d_desest_tempo_tempo))) %>% 
        mutate(
            sector_ord = sectores[i],
            sector = sectores_tit[i])
    
    d_desest_sec <- bind_rows(d_desest_tempo_2, d_desest_sec)
    
}


## Guardar bases desestacionalizadas ----
openxlsx::write.xlsx(d_desest, paste_out("01_02_imss_tot_desest.xlsx"))
openxlsx::write.xlsx(d_desest_cve_ent, paste_out("01_02_imss_tot_desest_cve_ent.xlsx"))
openxlsx::write.xlsx(d_desest_sec, paste_out("01_02_imss_tot_desest_sec.xlsx"))

# Las dos crisis ----

## Nacional ----
crisis_2008 <- d_desest %>% 
    filter(fecha_m > "2008-08-02") %>% 
    mutate(
        tot_imss_1 = first(tot_imss),
        fin_crisis = ifelse(
            tot_imss>=tot_imss_1 & fecha_m != first(fecha_m), 1, 0
        ),
        mes_crisis = row_number()-1
    ) %>% 
    filter(fin_crisis == 0) %>% 
    bind_rows(
        d_desest %>% 
            filter(fecha_m > "2008-08-02") %>% 
            mutate(
                tot_imss_1 = first(tot_imss),
                fin_crisis = ifelse(
                    tot_imss>=tot_imss_1 & fecha_m != first(fecha_m), 1, 0
                ),
                mes_crisis = row_number()-1
            ) %>% 
            filter(fin_crisis == 1) %>% 
            filter(fecha_m == first(fecha_m))
    ) %>% 
    glimpse

crisis_2020 <- d_desest %>% 
    filter(fecha_m > "2020-01-02") %>% 
    mutate(
        tot_imss_1 = first(tot_imss),
        fin_crisis = ifelse(
            tot_imss>=tot_imss_1 & fecha_m != first(fecha_m), 1, 0
        ),
        mes_crisis = row_number()-1
    ) %>% 
    filter(fin_crisis == 0) %>% 
    bind_rows(
        d_desest %>% 
            filter(fecha_m > "2020-01-02") %>% 
            mutate(
                tot_imss_1 = first(tot_imss),
                fin_crisis = ifelse(
                    tot_imss>=tot_imss_1 & fecha_m != first(fecha_m), 1, 0
                ),
                mes_crisis = row_number()-1
            ) %>% 
            filter(fin_crisis == 1) %>% 
            filter(fecha_m == first(fecha_m))
    ) %>% 
    glimpse

## Estatal ----
crisis_2008_cve_ent <- d_desest_cve_ent %>% 
    filter(fecha_m > "2008-08-02") %>% 
    group_by(cve_ent) %>% 
    mutate(
        tot_imss_1 = first(tot_imss),
        fin_crisis = ifelse(
            tot_imss>=tot_imss_1 & fecha_m != first(fecha_m), 1, 0
        )
    ) %>% 
    filter(fecha_m < "2016-01-01") %>% 
    glimpse

crisis_2020_cve_ent <- d_desest_cve_ent %>% 
    filter(fecha_m > "2020-01-02") %>% 
    group_by(cve_ent) %>% 
    mutate(
        tot_imss_1 = first(tot_imss),
        fin_crisis = ifelse(
            tot_imss>=tot_imss_1 & fecha_m != first(fecha_m), 1, 0
        )#,
        #mes_crisis = row_number()-1
    ) 

## Sectores ----
crisis_2008_sec <- d_desest_sec %>% 
    filter(fecha_m > "2008-08-02") %>% 
    group_by(sector) %>% 
    mutate(
        tot_imss_1 = first(tot_imss),
        fin_crisis = ifelse(
            tot_imss>=tot_imss_1 & fecha_m != first(fecha_m), 1, 0
        )
    ) %>% 
    filter(fecha_m < "2016-01-01") %>% 
    glimpse

crisis_2020_sec <- d_desest_sec %>% 
    filter(fecha_m > "2020-01-02") %>% 
    group_by(sector) %>% 
    mutate(
        tot_imss_1 = first(tot_imss),
        fin_crisis = ifelse(
            tot_imss>=tot_imss_1 & fecha_m != first(fecha_m), 1, 0
        )
    ) %>% 
    glimpse

## Guardar bases crisis ----
openxlsx::write.xlsx(crisis_2008, paste_out("01_02_crisis_2008.xlsx"))
openxlsx::write.xlsx(crisis_2008_cve_ent, paste_out("01_02_crisis_2008_cve_ent.xlsx"))
openxlsx::write.xlsx(crisis_2008_sec, paste_out("01_02_crisis_2008_sec.xlsx"))

openxlsx::write.xlsx(crisis_2020, paste_out("01_02_crisis_2020.xlsx"))
openxlsx::write.xlsx(crisis_2020_cve_ent, paste_out("01_02_crisis_2020_cve_ent.xlsx"))
openxlsx::write.xlsx(crisis_2020_sec, paste_out("01_02_crisis_2020_sec.xlsx"))

# Plots ----
## 1. Caída y recuperación de las dos crisis ----
titulo      <- "Caída y recuperación de las dos crisis"
subtitulo   <- "Puestos de trabajo registrados ante el IMSS (cifras desestacionalizadas)"
eje_y       <- "Variación porcentual"
eje_x <- "Meses desde el inicio de la crisis"

g <- 
    ggplot(
        crisis_2008 %>% 
            mutate(crisis = "Crisis financiera global 2008",
                   var_prop = (tot_imss-tot_imss_1)/tot_imss_1) %>% 
            bind_rows(
                crisis_2020 %>% 
                    mutate(crisis = "Crisis COVID-19 2020",
                           var_prop = (tot_imss-tot_imss_1)/tot_imss_1)
            ),
        aes(
            x = mes_crisis,
            y = var_prop,
            group = crisis,
            col = crisis
        )
    ) +
    geom_line(size = 1.8) +
    geom_point(aes(y = ifelse((crisis == "Crisis COVID-19 2020" & mes_crisis == 19) | fin_crisis == 1,var_prop, NA),group = crisis), size = 3) +
    scale_y_continuous(eje_y, labels = scales::percent)+#, breaks = seq(0,12500000,2500000), limits = c(0,15000000)) +
    scale_x_continuous(eje_x, breaks = 0:21) +
    scale_color_manual("", values = c(mcv_discrete[1], mcv_discrete[3]))  +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo,
        fill= "", shape="", y = eje_y, x = eje_x
    ) +
    theme(plot.title      = element_text(size = 35, face = "bold", colour = "#6950D8"),
          plot.subtitle     = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.margin       = margin(0.5, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
          plot.caption      = element_text(size = 20),
          strip.text.x      = element_text(size = 15),
          panel.grid.minor  = element_blank(),
          panel.background  = element_rect(fill = "transparent",colour = NA),
          text              = element_text(family = "Ubuntu"),
          axis.title.x      = element_text(size = 25),
          axis.title.y      = element_text(size = 25),
          axis.text.x       = element_text(size = 20, vjust = 0.5),
          axis.text.y       = element_text(size = 20),
          legend.text       = element_text(size = 25),
          legend.position   = "top")
ggimage::ggbackground(g, paste_info("00_plantillas/02_imss.pdf"))
ggsave(filename = paste_info("01_02_04_crisis_desest.png"), width = 23, height = 12, dpi = 100, bg= "transparent")

## 2. Sectores ----
## 2.3. Industrias de la transformación ----
i = 3
d_sec_plot <- 
    crisis_2008_sec %>% 
    filter(sector_ord == sectores[i]) %>% 
    mutate(crisis = "Crisis financiera global 2008",
           var_prop = (tot_imss-tot_imss_1)/tot_imss_1) %>% 
    filter(fin_crisis==0) %>% 
    bind_rows(
        crisis_2008_sec %>% 
            filter(sector_ord == sectores[i]) %>% 
            mutate(crisis = "Crisis financiera global 2008",
                   var_prop = (tot_imss-tot_imss_1)/tot_imss_1) %>% 
            filter(fin_crisis == 1) %>% 
            filter(fecha_m == first(fecha_m))
    ) %>% 
    mutate(mes_crisis = row_number()-1) %>% 
    bind_rows(
        crisis_2020_sec %>% 
            filter(sector_ord == sectores[i]) %>% 
            filter(fecha_m < "2021-10-01") %>% 
            #filter(fecha_m < "2021-04-01") %>% 
            mutate(crisis = "Crisis COVID-19 2020",
                   var_prop = (tot_imss-tot_imss_1)/tot_imss_1,
                   mes_crisis = row_number()-1)
    ) 


titulo      <- "Caída y recuperación de las dos crisis en la Industria de la Transformación"
subtitulo   <- "Puestos de trabajo registrados ante el IMSS (cifras desestacionalizadas)"
eje_y       <- "Variación porcentual"
eje_x <- "Meses desde el inicio de la crisis"

g <- 
    ggplot(
        d_sec_plot,
        aes(
            x = mes_crisis,
            y = var_prop,
            group = crisis,
            col = crisis
        )
    ) +
    geom_line(size = 1.8) +
    geom_point(aes(y = ifelse((crisis == "Crisis COVID-19 2020" & mes_crisis == 19) | (crisis != "Crisis COVID-19 2020" & fin_crisis == 1),var_prop, NA),group = crisis), size = 3) +
    scale_y_continuous(eje_y, labels = scales::percent)+#, breaks = seq(0,12500000,2500000), limits = c(0,15000000)) +
    scale_x_continuous(eje_x, breaks = 0:40) +
    scale_color_manual("", values = c(mcv_discrete[1], mcv_discrete[3]))  +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo,
        fill= "", shape="", y = eje_y, x = eje_x
    ) +
    theme(plot.title      = element_text(size = 35, face = "bold", colour = "#6950D8"),
          plot.subtitle     = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.margin       = margin(0.5, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
          plot.caption      = element_text(size = 20),
          strip.text.x      = element_text(size = 15),
          panel.grid.minor  = element_blank(),
          panel.background  = element_rect(fill = "transparent",colour = NA),
          text              = element_text(family = "Ubuntu"),
          axis.title.x      = element_text(size = 25),
          axis.title.y      = element_text(size = 25),
          axis.text.x       = element_text(size = 20, vjust = 0.5),
          axis.text.y       = element_text(size = 20),
          legend.text       = element_text(size = 25),
          legend.position   = "top")

ggimage::ggbackground(g, paste_info("00_plantillas/02_imss.pdf"))
ggsave(filename = paste_info("01_02_05_03_crisis_sec_transf_desest.png"), width = 23, height = 12, dpi = 100, bg= "transparent")


## 2.4. Construcción ----
i = 4
d_sec_plot <- 
    crisis_2008_sec %>% 
    filter(sector_ord == sectores[i]) %>% 
    mutate(crisis = "Crisis financiera global 2008",
           var_prop = (tot_imss-tot_imss_1)/tot_imss_1) %>% 
    filter(fin_crisis==0) %>% 
    bind_rows(
        crisis_2008_sec %>% 
            filter(sector_ord == sectores[i]) %>% 
            mutate(crisis = "Crisis financiera global 2008",
                   var_prop = (tot_imss-tot_imss_1)/tot_imss_1) %>% 
            filter(fin_crisis == 1) %>% 
            filter(fecha_m == first(fecha_m))
    ) %>% 
    mutate(mes_crisis = row_number()-1) %>% 
    bind_rows(
        crisis_2020_sec %>% 
            filter(sector_ord == sectores[i]) %>% 
            filter(fecha_m < "2021-10-01") %>% 
            mutate(crisis = "Crisis COVID-19 2020",
                   var_prop = (tot_imss-tot_imss_1)/tot_imss_1,
                   mes_crisis = row_number()-1)
    ) 


titulo      <- "Caída y recuperación de las dos crisis en el sector Construcción"
subtitulo   <- "Puestos de trabajo registrados ante el IMSS (cifras desestacionalizadas)"
eje_y       <- "Variación porcentual"
eje_x <- "Meses desde el inicio de la crisis"

g <- 
    ggplot(
        d_sec_plot,
        aes(
            x = mes_crisis,
            y = var_prop,
            group = crisis,
            col = crisis
        )
    ) +
    geom_line(size = 1.8) +
    geom_point(aes(y = ifelse((crisis == "Crisis COVID-19 2020" & mes_crisis == 19) | (crisis != "Crisis COVID-19 2020" & fin_crisis == 1),var_prop, NA),group = crisis), size = 3) +
    scale_y_continuous(eje_y, labels = scales::percent)+#, breaks = seq(0,12500000,2500000), limits = c(0,15000000)) +
    scale_x_continuous(eje_x, breaks = 0:40) +
    scale_color_manual("", values = c(mcv_discrete[1], mcv_discrete[3]))  +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo,
        fill= "", shape="", y = eje_y, x = eje_x
    ) +
    theme(plot.title      = element_text(size = 35, face = "bold", colour = "#6950D8"),
          plot.subtitle     = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.margin       = margin(0.5, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
          plot.caption      = element_text(size = 20),
          strip.text.x      = element_text(size = 15),
          panel.grid.minor  = element_blank(),
          panel.background  = element_rect(fill = "transparent",colour = NA),
          text              = element_text(family = "Ubuntu"),
          axis.title.x      = element_text(size = 25),
          axis.title.y      = element_text(size = 25),
          axis.text.x       = element_text(size = 20, vjust = 0.5),
          axis.text.y       = element_text(size = 20),
          legend.text       = element_text(size = 25),
          legend.position   = "top")

ggimage::ggbackground(g, paste_info("00_plantillas/02_imss.pdf"))
ggsave(filename = paste_info("01_02_05_04_crisis_sec_construccion_desest.png"), width = 23, height = 12, dpi = 100, bg= "transparent")

## 2.6. Comercio ----
i = 6
d_sec_plot <- 
    crisis_2008_sec %>% 
    filter(sector_ord == sectores[i]) %>% 
    mutate(crisis = "Crisis financiera global 2008",
           var_prop = (tot_imss-tot_imss_1)/tot_imss_1) %>% 
    filter(fecha_m < "2010-04-01") %>% 
    mutate(mes_crisis = row_number()-1) %>% 
    bind_rows(
        crisis_2020_sec %>% 
            filter(sector_ord == sectores[i]) %>% 
            filter(fecha_m < "2021-10-01") %>% 
            mutate(crisis = "Crisis COVID-19 2020",
                   var_prop = (tot_imss-tot_imss_1)/tot_imss_1,
                   mes_crisis = row_number()-1)
    ) 


titulo      <- "Caída y recuperación de las dos crisis en el sector Comercio"
subtitulo   <- "Puestos de trabajo registrados ante el IMSS (cifras desestacionalizadas)"
eje_y       <- "Variación porcentual"
eje_x <- "Meses desde el inicio de la crisis"

g <- 
    ggplot(
        d_sec_plot,
        aes(
            x = mes_crisis,
            y = var_prop,
            group = crisis,
            col = crisis
        )
    ) +
    geom_line(size = 1.8) +
    geom_point(aes(y = ifelse((crisis == "Crisis COVID-19 2020" & mes_crisis == 19) | (crisis != "Crisis COVID-19 2020" & fecha_m == "2010-03-01"),var_prop, NA),group = crisis), size = 3) +
    scale_y_continuous(eje_y, labels = scales::percent)+#, breaks = seq(0,12500000,2500000), limits = c(0,15000000)) +
    scale_x_continuous(eje_x, breaks = 0:40) +
    scale_color_manual("", values = c(mcv_discrete[1], mcv_discrete[3]))  +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo,
        fill= "", shape="", y = eje_y, x = eje_x
    ) +
    theme(plot.title      = element_text(size = 35, face = "bold", colour = "#6950D8"),
          plot.subtitle     = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.margin       = margin(0.5, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
          plot.caption      = element_text(size = 20),
          strip.text.x      = element_text(size = 15),
          panel.grid.minor  = element_blank(),
          panel.background  = element_rect(fill = "transparent",colour = NA),
          text              = element_text(family = "Ubuntu"),
          axis.title.x      = element_text(size = 25),
          axis.title.y      = element_text(size = 25),
          axis.text.x       = element_text(size = 20, vjust = 0.5),
          axis.text.y       = element_text(size = 20),
          legend.text       = element_text(size = 25),
          legend.position   = "top")

ggimage::ggbackground(g, paste_info("00_plantillas/02_imss.pdf"))
ggsave(filename = paste_info("01_02_05_06_crisis_sec_comercio_desest.png"), width = 23, height = 12, dpi = 100, bg= "transparent")

## 2.7. Transportes y comunicaciones ----
i = 7
d_sec_plot <- 
    crisis_2008_sec %>% 
    filter(sector_ord == sectores[i]) %>% 
    mutate(crisis = "Crisis financiera global 2008",
           var_prop = (tot_imss-tot_imss_1)/tot_imss_1) %>% 
    filter(fin_crisis==0) %>% 
    bind_rows(
        crisis_2008_sec %>% 
            filter(sector_ord == sectores[i]) %>% 
            mutate(crisis = "Crisis financiera global 2008",
                   var_prop = (tot_imss-tot_imss_1)/tot_imss_1) %>% 
            filter(fin_crisis == 1) %>% 
            filter(fecha_m == first(fecha_m))
    ) %>% 
    mutate(mes_crisis = row_number()-1) %>% 
    bind_rows(
        crisis_2020_sec %>% 
            filter(sector_ord == sectores[i]) %>% 
            filter(fecha_m < "2021-10-01") %>% 
            mutate(crisis = "Crisis COVID-19 2020",
                   var_prop = (tot_imss-tot_imss_1)/tot_imss_1,
                   mes_crisis = row_number()-1)
    ) 


titulo      <- "Caída y recuperación de las dos crisis en el sector Transportes y comunicaciones"
subtitulo   <- "Puestos de trabajo registrados ante el IMSS (cifras desestacionalizadas)"
eje_y       <- "Variación porcentual"
eje_x <- "Meses desde el inicio de la crisis"

g <- 
    ggplot(
        d_sec_plot,
        aes(
            x = mes_crisis,
            y = var_prop,
            group = crisis,
            col = crisis
        )
    ) +
    geom_line(size = 1.8) +
    geom_point(aes(y = ifelse((crisis == "Crisis COVID-19 2020" & mes_crisis == 19) | (crisis != "Crisis COVID-19 2020" & fin_crisis == 1),var_prop, NA),group = crisis), size = 3) +
    scale_y_continuous(eje_y, labels = scales::percent)+#, breaks = seq(0,12500000,2500000), limits = c(0,15000000)) +
    scale_x_continuous(eje_x, breaks = 0:40) +
    scale_color_manual("", values = c(mcv_discrete[1], mcv_discrete[3]))  +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo,
        fill= "", shape="", y = eje_y, x = eje_x
    ) +
    theme(plot.title      = element_text(size = 35, face = "bold", colour = "#6950D8"),
          plot.subtitle     = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.margin       = margin(0.5, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
          plot.caption      = element_text(size = 20),
          strip.text.x      = element_text(size = 15),
          panel.grid.minor  = element_blank(),
          panel.background  = element_rect(fill = "transparent",colour = NA),
          text              = element_text(family = "Ubuntu"),
          axis.title.x      = element_text(size = 25),
          axis.title.y      = element_text(size = 25),
          axis.text.x       = element_text(size = 20, vjust = 0.5),
          axis.text.y       = element_text(size = 20),
          legend.text       = element_text(size = 25),
          legend.position   = "top")

ggimage::ggbackground(g, paste_info("00_plantillas/02_imss.pdf"))
ggsave(filename = paste_info("01_02_05_07_crisis_sec_transp_desest.png"), width = 23, height = 12, dpi = 100, bg= "transparent")


## 2.8. Servicios para empresas, personas y el hogar ----
i = 8
d_sec_plot <- 
    crisis_2008_sec %>% 
    filter(sector_ord == sectores[i]) %>% 
    mutate(crisis = "Crisis financiera global 2008",
           var_prop = (tot_imss-tot_imss_1)/tot_imss_1) %>% 
    filter(fin_crisis==0) %>% 
    bind_rows(
        crisis_2008_sec %>% 
            filter(sector_ord == sectores[i]) %>% 
            mutate(crisis = "Crisis financiera global 2008",
                   var_prop = (tot_imss-tot_imss_1)/tot_imss_1) %>% 
            filter(fin_crisis == 1) %>% 
            filter(fecha_m == first(fecha_m))
    ) %>% 
    mutate(mes_crisis = row_number()-1) %>% 
    bind_rows(
        crisis_2020_sec %>% 
            filter(sector_ord == sectores[i]) %>% 
            filter(fecha_m < "2021-10-01") %>% 
            mutate(crisis = "Crisis COVID-19 2020",
                   var_prop = (tot_imss-tot_imss_1)/tot_imss_1,
                   mes_crisis = row_number()-1)
    ) 


titulo      <- "Caída y recuperación de las dos crisis en el sector\nServicios para empresas, personas y el hogar"
subtitulo   <- "Puestos de trabajo registrados ante el IMSS (cifras desestacionalizadas)"
eje_y       <- "Variación porcentual"
eje_x <- "Meses desde el inicio de la crisis"

g <- 
    ggplot(
        d_sec_plot,
        aes(
            x = mes_crisis,
            y = var_prop,
            group = crisis,
            col = crisis
        )
    ) +
    geom_line(size = 1.8) +
    geom_point(aes(y = ifelse((crisis == "Crisis COVID-19 2020" & mes_crisis == 19) | (crisis != "Crisis COVID-19 2020" & fin_crisis == 1),var_prop, NA),group = crisis), size = 3) +
    scale_y_continuous(eje_y, labels = scales::percent)+#, breaks = seq(0,12500000,2500000), limits = c(0,15000000)) +
    scale_x_continuous(eje_x, breaks = 0:40) +
    scale_color_manual("", values = c(mcv_discrete[1], mcv_discrete[3]))  +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo,
        fill= "", shape="", y = eje_y, x = eje_x
    ) +
    theme(plot.title      = element_text(size = 35, face = "bold", colour = "#6950D8"),
          plot.subtitle     = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.margin       = margin(0.5, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
          plot.caption      = element_text(size = 20),
          strip.text.x      = element_text(size = 15),
          panel.grid.minor  = element_blank(),
          panel.background  = element_rect(fill = "transparent",colour = NA),
          text              = element_text(family = "Ubuntu"),
          axis.title.x      = element_text(size = 25),
          axis.title.y      = element_text(size = 25),
          axis.text.x       = element_text(size = 20, vjust = 0.5),
          axis.text.y       = element_text(size = 20),
          legend.text       = element_text(size = 25),
          legend.position   = "top")

ggimage::ggbackground(g, paste_info("00_plantillas/02_imss.pdf"))
ggsave(filename = paste_info("01_02_05_08_crisis_sec_serv_emp_pers_hog_desest.png"), width = 23, height = 12, dpi = 100, bg= "transparent")
