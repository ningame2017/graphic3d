require(pacman)
pacman::p_load(ggrepel,shiny,DT,pdftools, tidyverse,plotly, treemapify,magick,readxl, purr)
library(purrr)
options(scipen = 9999, digits = 2)


# Función para limpiar archivos temporales antiguos
clean_temp_files <- function(temp_dir, age_limit) {
  temp_files <- list.files(temp_dir, full.names = TRUE)
  for (file in temp_files) {
    if (file.info(file)$mtime < Sys.time() - age_limit) {
      file.remove(file)
    }
  }
}
clean_temp_files(tempdir(), age_limit = 100)  # 1 hora de límite de edad

## unir bases
historico1 <- dir("C:/Users/NJvaloyesM/Documents/PPA/Analisis/Historico/", full.names = T) %>% 
    map_df(read_excel) %>% mutate(Nov.Act.=ifelse(is.na(Nov.Act.),"NV",Nov.Act.))
historico1 <- unique(historico1) %>% 
    dplyr::select(-27) %>% 
    rename(ESTADO = ESTADO...1)


historico <- read_csv2("~/PPA/Analisis/historico.csv") %>% 
    filter(PERIODO != "JULIO_2024")

historico <- rbind(historico,historico1)

historico <- historico %>% mutate(PERIODO = str_to_upper(PERIODO),
                                  PERIODO = case_when(PERIODO == "FEBRERO_22" ~ "FEBRERO_2022",
                                            PERIODO == "MARZO_22" ~ "MARZO_2022",
                                            PERIODO == "ABRIL_22" ~ "ABRIL_2022",
                                            PERIODO == "JULIO_22" ~ "JULIO_2022",
                                            PERIODO == "AGOSTO_22" ~ "AGOSTO_2022",
                                            PERIODO == "OCTUBRE_22" ~ "OCTUBRE_2022",
                                            PERIODO == "ABRIL_23" ~ "ABRIL_2023",
                                            PERIODO == "OCTUBRE 2023" ~ "OCTUBRE_2023",
                                            PERIODO == "ABRIL_24" ~ "ABRIL_2024",
                                            PERIODO == "JULIO 2024" ~ "JULIO_2024",
                                    TRUE ~ PERIODO  # Mantener el valor original si no cumple ninguna condición
                                  ))

setwd("~/PPA/analisis")

ciudades = c("select all",unique(historico$Ciudad))
historico$PERIODO <- factor(historico$PERIODO, levels =  c("FEBRERO_2022","MARZO_2022","ABRIL_2022","MAYO_2022","JUNIO_2022","JULIO_2022","AGOSTO_2022","SEPTIEMBRE_2022","OCTUBRE_2022","NOVIEMBRE_2022","DICIEMBRE_2022",
                                                           "ENERO_2023","FEBRERO_2023","MARZO_2023","ABRIL_2023","MAYO_2023","JUNIO_2023","JULIO_2023","AGOSTO_2023","SEPTIEMBRE_2023","OCTUBRE_2023","NOVIEMBRE_2023",
                                                           "DICIEMBRE_2023","ENERO_2024","FEBRERO_2024","MARZO_2024","ABRIL_2024","MAYO_2024","JUNIO_2024","JULIO_2024"))
unique(historico$PERIODO)
### fotos
archivo <- NULL
a<-1
vecto <- list.dirs("//DRA-D-154/ACamachoN/ICCV-ICCP/2024",recursive = FALSE)

for(i in vecto){
    lista <- list.files(i)
    for (j in lista){
        archivo[a] <- paste(i, j,sep = "/")
        a<- a+1
    }
}

for(i in archivo){
    lista <- list.files(i)
    for (j in lista){
        archivo[a] <- paste(i, j,sep = "/")
        a<- a+1
    }
}


for(i in archivo){
    lista <- list.files(i)
    for (j in lista){
        archivo[a] <- paste(i, j,sep = "/")
        a<- a+1
    }
}

fotos <- as.data.frame(archivo)
fotos$consecutivo <- seq(1:length(fotos$archivo))

separadas <- str_split_fixed (fotos$archivo,"/",10)
colnames(separadas) <- c("vacio","vacio","servidor","nombre","Investigacion","ano","mes",
                         "Ciudad","sub_inv","archivo")
separadas <- as.data.frame(separadas)
separadas$consecutivo <- seq(1:length(separadas$vacio))

separadas$vacio<-NULL

separadas <- separadas %>% filter(archivo != "" & archivo != "Thumbs.db")
separadas$vacio<-NULL

final <-  left_join(separadas, fotos, by = "consecutivo") %>% 
    mutate(GOOGLE = str_replace_all(archivo.y, pattern = "/",replacement =  "\\\\"))


final<- final %>% filter(sub_inv %in% c("CONSTRUCCION","CONSTRUCCIÓN","COSNTRUCCIÓN","Construcción"))

soportes_CONSTRUCCION <- readxl::read_excel("~/PPA/Analisis/soportes_CONSTRUCCION_2022_2023.xlsx")
final <- rbind(soportes_CONSTRUCCION, final)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    
    # Application title
    titlePanel("Aplicativo de consulta de CONSTRUCCION"),
    sidebarPanel(selectInput("tipo","Tipo:",
                             choices = c("Consolidado", "Analisis"), selected = "Analisis"),
                 selectInput("periodo","Periodo:",
                             choices = unique(historico$PERIODO), selected = "JULIO_2024"),
                 selectInput("estadisticos","nombre articulo:",
                             choices = sort(unique(paste0(historico$Cod.Art.," - ",historico$Nom.Art.)))),
                 textInput("todo","Elija Fuente:",
                           value = "Fuente"),
                 actionButton("descargar","Descargar", class = "btn-info"),
                 selectInput("coti","Ciudad:",
                             ciudades, multiple = TRUE, selected = "select all"),
                 br(),
                 selectInput("serie","Fuente:",
                             choices = "", multiple = TRUE),
                 br(),
                 plotlyOutput("variacion"),
                 br(),
                 plotOutput("contri"),
                 ),
    
    mainPanel(
        tabsetPanel(
            
            tabPanel("Estadisticos",
                     dataTableOutput("estadisticos"),
                     plotlyOutput("tridi"),
                     plotOutput("tendencia")),
            
            
            tabPanel("Soportes",selectInput("soport","Soportes:", choices = NULL,multiple = FALSE, width = 4000),
                             selectInput("pagina","Pagina:", choices = seq(1,20,1), width= 50),
                             plotOutput("foto")
                             ),
                             #dataTableOutput("to_sop")
        
        
        tabPanel("Estados",
                 dataTableOutput("estado"),
                 actionButton("error","ERRORES", class = "btn-info"),
                 actionButton("faltantes","FALTANTES", class = "btn-info")),
        
        
        tabPanel("Historico",
                 plotOutput("serie")),
        
       
        tabPanel("Linea",
             plotOutput("linea")),
    
    
    
        tabPanel("Cotizaciones",
             dataTableOutput("fuentes"))
    
    )
    
   
    ),
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
    
    modif <- reactive({historico %>% mutate(Articulo = paste0(Cod.Art.," - ",Nom.Art.))
    })
    
    observe({
      
        soportes <- final %>% filter (str_detect(archivo.x, input$todo)) %>% dplyr::select(10)
        
        updateSelectInput(session,"soport","Soportes:",
                                   choices = unique(soportes))
        })
    
    
    output$soportes <- renderDataTable({    
      tabla <- final %>% 
        filter(archivo.y==input$soport) %>% 
        dplyr::select(10) 
      
      DT::datatable(tabla, rownames = FALSE)
      
    })
    
    output$foto <- renderImage({
      lista <- final %>% filter(archivo.y==input$soport) %>% 
        slice(1)
      
      #file.copy(lista$archivo.y, "C:/Users/NJvaloyesM/Documents/PPA/Analisis/Temporal", overwrite = TRUE)
      
      setwd("~/PPA/analisis/pdf")
      if(str_detect(lista$archivo.y,".pdf")){
        bitmap <- pdf_render_page(lista$archivo.y, page = as.numeric(input$pagina), dpi = 100)
        poster <- image_read(bitmap)
        magick::image_write(poster, "imagen.png")
        list(src = paste0("C:/Users/NJvaloyesM/Documents/PPA/Analisis/pdf/","imagen.png"),width=1000, height=700)
      }else{
        list(src = lista$archivo.y,width=700, height=900) 
      }
    })
   
    observe({
        
        fuentes <- modif() %>% 
            mutate(Id.Fte = as.factor(Id.Fte), Fuente = paste0(Id.Fte,"-",`Nombre Fuente`)) %>%
                       filter (Articulo == input$estadisticos)
        
        updateSelectInput(session,"serie","Fuente:",
                              
                          choices = sort(unique(fuentes$Fuente)))
    })
    
    observe({
        if(input$tipo=="Consolidado"){
            articulos <- modif() %>% 
                mutate(Articulo = paste0(Cod.Art.," - ",Nom.Art.)) %>% 
                filter(PERIODO == input$periodo) 
            
            updateSelectInput(session,"estadisticos","nombre articulo:",
                             choices = sort(unique(paste0(articulos$Articulo))))}else{
            
        articulos <- modif() %>% 
            mutate(Id.Fte = as.factor(Id.Fte)) %>% 
            filter(Id.Fte==input$todo & PERIODO == input$periodo) 
            
        
        updateSelectInput(session,"estadisticos","nombre articulo:",
                                              choices = sort(unique(paste0(articulos$Cod.Art.," - ",articulos$Nom.Art.))))}
    })
    
    observeEvent(input$descargar,{
        lista <- final %>% filter(archivo.y==input$soport) %>% 
            slice(1)
        
        file.copy(lista$archivo.y, "C:/Users/NJvaloyesM/Documents/PPA/Analisis/Temporal", overwrite = TRUE)
    })
    
    observeEvent(input$error,{
        lista <- modif() %>% filter(Id.Fte == input$todo & PERIODO == input$periodo & Articulo == input$estadisticos) %>% 
            dplyr::select(3,Articulo,14,1,4,5,9,11,15:23,26)
        setwd("~/PPA/analisis/ERRORES")
        writexl::write_xlsx(lista,"error.xlsx")
        errores <- dir("C:/Users/NJvaloyesM/Documents/PPA/Analisis/ERRORES/", full.names = T) %>% 
            map_df(read_excel)
        errores <- unique(errores)
        writexl::write_xlsx(errores,"errores.xlsx")
        
        
    })
    
    observeEvent(input$faltantes,{
        lista <- modif() %>% filter(Id.Fte == input$todo & PERIODO == input$periodo & Articulo == input$estadisticos) %>% 
            dplyr::select(3,Articulo,14,1,4,5,9,11,15:23,26)
        setwd("~/PPA/analisis/FALTANTES")
        writexl::write_xlsx(lista,"faltante.xlsx")
        faltantes <- dir("C:/Users/NJvaloyesM/Documents/PPA/Analisis/FALTANTES/", full.names = T) %>% 
            map_df(read_excel)
        faltantes <- unique(faltantes)
        writexl::write_xlsx(faltantes,"faltantes.xlsx")
        
        
    })
     
    
    output$articulo <- renderImage({
        fotos2 <- imagenes %>% filter(Articulo==input$estadisticos)
        list(src = fotos2$imagenes,width=300, height=400)
    })
    
    
    output$variacion <- renderPlotly({
        union <- modif() %>% 
            filter(PERIODO == input$periodo, Articulo %in% input$estadisticos & `Pre Bas.Act.` != 0 & Var != 0 & ESTADO %in% c("ANALIZADO CENTRAL","CONTROL DE CALIDAD CENTRAL","REVISADO CENTRAL") & Nov.Act. != "CR") 
            
        
        # Definir el número de niveles del embudo
        niveles <- round((max(union$Var)-min(union$Var))/10,0)
        
        # Altura del embudo
        z_vals <- seq(min(union$Var), max(union$Var), length.out = niveles)
        
        # Radios en la base y en la cima del embudo
        radio_base <- 1
        radio_cima <- 5
        
        # Crear los radios interpolados
        radios <- seq(radio_base, radio_cima, length.out = niveles)
        
        # Definir las coordenadas x, y en base a coordenadas polares
        theta <- seq(0, 2 * pi, length.out = 100)
        
        # Crear una matriz para las coordenadas x, y, z
        x <- outer(radios, cos(theta))
        y <- outer(radios, sin(theta))
        z <- matrix(rep(z_vals, each = 100), nrow = niveles, byrow = TRUE)
        
        # Generar puntos de variaciones aleatorias dentro del embudo
        set.seed(42)  # Para reproducibilidad
        puntos_n <- length(union$Var)
        z_puntos <- union$Var
        radios_puntos <- radio_base + (radio_cima - radio_base) * (z_puntos - min(z_vals)) / (max(z_vals) - min(z_vals))
        theta_puntos <- runif(puntos_n, min = 0, max = 2 * pi)
        x_puntos <- radios_puntos * cos(theta_puntos)
        y_puntos <- radios_puntos * sin(theta_puntos)
        
        # Crear el gráfico de embudo 3D con plotly
        fig <- plot_ly(type = 'surface',
                       x = x,
                       y = y,
                       z = z,
                       opacity = 0.2,
                       surfacecolor = matrix(rep(NA, length(x) * length(y)), nrow = length(x)),
                       showscale = FALSE,
                       contours = list(
                         x = list(show = TRUE, color = 'black'),
                         y = list(show = TRUE, color = 'black'),
                         z = list(show = TRUE, color = 'black'))) %>%
          add_trace(type = 'scatter3d',
                    mode = 'markers',
                    x = x_puntos,
                    y = y_puntos,
                    z = z_puntos,
                    marker = list(size = 5, color = "black")) %>%
          layout(title = 'Embudo 3D de Variaciones con Cuadrícula',
                 scene = list(
                   xaxis = list(title = 'X'),
                   yaxis = list(title = 'Y'),
                   zaxis = list(title = 'Variación')))
        
        fig
        
        
    })
    
    output$serie <- renderPlot({
        serie <- modif() %>% 
            mutate(PERIODO = factor(PERIODO, levels = c("FEBRERO_2022","MARZO_2022","ABRIL_2022","MAYO_2022","JUNIO_2022","JULIO_2022","AGOSTO_2022","SEPTIEMBRE_2022","OCTUBRE_2022","NOVIEMBRE_2022","DICIEMBRE_2022",
                                                        "ENERO_2023","FEBRERO_2023","MARZO_2023","ABRIL_2023","MAYO_2023","JUNIO_2023","JULIO_2023","AGOSTO_2023","SEPTIEMBRE_2023","OCTUBRE_2023","NOVIEMBRE_2023",
                                                        "DICIEMBRE_2023","ENERO_2024","FEBRERO_2024","MARZO_2024","ABRIL_2024","MAYO_2024","JUNIO_2024","JULIO_2024")),
                   Fuente = paste0(Id.Fte,"-",`Nombre Fuente`)) %>% 
            filter(Articulo == input$estadisticos & Fuente %in% c(input$serie) & Nov.Act. != "PE") 
            
        
        serie <- unique(serie)
        ggplot(serie, aes(PERIODO, `Pre Bas.Act.`, group=Fuente, label = paste0(Nov.Act.," (",Var,"%)")))+
            geom_line(aes(color= Ciudad))+
            geom_point()+
            geom_text_repel()+
            theme(legend.position = "right", axis.text = element_text(angle = 90))
    })
    
    output$linea <- renderPlot({
        if(input$coti != "select all"){
        linea <- modif() %>% 
            mutate(PERIODO = factor(PERIODO, levels = c("FEBRERO_2022","MARZO_2022","ABRIL_2022","MAYO_2022","JUNIO_2022","JULIO_2022","AGOSTO_2022","SEPTIEMBRE_2022","OCTUBRE_2022","NOVIEMBRE_2022","DICIEMBRE_2022",
                                                        "ENERO_2023","FEBRERO_2023","MARZO_2023","ABRIL_2023","MAYO_2023","JUNIO_2023","JULIO_2023","AGOSTO_2023","SEPTIEMBRE_2023","OCTUBRE_2023","NOVIEMBRE_2023",
                                                        "DICIEMBRE_2023","ENERO_2024","FEBRERO_2024","MARZO_2024","ABRIL_2024","MAYO_2024","JUNIO_2024","JULIO_2024"))) %>%
            filter(Articulo == input$estadisticos & Nov.Act. %in% c("NV","SI") & Ciudad %in% input$coti) %>%
            group_by(Articulo, PERIODO) %>% 
            summarise(cotizaciones = n(), promedio = round(mean(Var, na.rm = TRUE),2)) %>% 
            arrange(PERIODO) %>% 
            mutate(acumulado= cumsum(promedio))
        }
        else{
            linea <- modif() %>% 
                mutate(PERIODO = factor(PERIODO, levels = c("FEBRERO_2022","MARZO_2022","ABRIL_2022","MAYO_2022","JUNIO_2022","JULIO_2022","AGOSTO_2022","SEPTIEMBRE_2022","OCTUBRE_2022","NOVIEMBRE_2022","DICIEMBRE_2022",
                                                            "ENERO_2023","FEBRERO_2023","MARZO_2023","ABRIL_2023","MAYO_2023","JUNIO_2023","JULIO_2023","AGOSTO_2023","SEPTIEMBRE_2023","OCTUBRE_2023","NOVIEMBRE_2023",
                                                            "DICIEMBRE_2023","ENERO_2024","FEBRERO_2024","MARZO_2024","ABRIL_2024","MAYO_2024","JUNIO_2024","JULIO_2024"))) %>%
                filter(Articulo == input$estadisticos & Nov.Act. %in% c("NV","SI")) %>%
                group_by(Articulo, PERIODO) %>% 
                summarise(cotizaciones = n(), promedio = round(mean(Var, na.rm = TRUE),2)) %>% 
                arrange(PERIODO) %>% 
                mutate(acumulado= cumsum(promedio))
        }
        acum <- linea %>% 
            dplyr::select(1,2,5)
    
        linea <- unique(linea)
        ggplot(linea, aes(PERIODO, promedio, group = 1, color = "Promedio"))+
            geom_bar(stat= "identity")+
            geom_text(data = linea, aes(label = paste0(signif(promedio,4),"%")), vjust= -1)+
            #geom_line(aes(y= acumulado, group = 1, color = "Acumulado", label = signif(acumulado,4)))+
            geom_smooth()+
            theme_minimal()+
            theme(legend.position = "right", axis.text = element_text(angle = 90))
            
    })
    
    output$estadisticos <- renderDataTable({
        if(input$coti != "select all"){
        
        cuadro <- modif() %>%
            filter(Articulo == input$estadisticos & `Pre Bas.Act.` != 0 & ESTADO %in% c("DISPONIBLE PARA NIVEL CENTRAL","PARA VERIFICAR NIVEL CENTRAL","ANALIZADO CENTRAL","CONTROL DE CALIDAD CENTRAL","REVISADO CENTRAL","DISPONIBLE PARA CALCULO") & PERIODO == input$periodo & Ciudad %in% input$coti) %>%  
            group_by(Articulo,Cod.Art.) %>% 
            summarise(conteo = n(), media_actual = round(mean(`Pre Bas.Act.`, na.rm = T),digits = 0),
                      media_anterior = round(mean(`Pre Bas.Ant.`, na.rm = T),digits = 0),
                      mediana = round(median(`Pre Bas.Act.`, na.rm = T),digits = 0),
                      minimo = round(min (`Pre Bas.Act.`, na.rm = T),digits = 0), 
                      maximo = round(max(`Pre Bas.Act.`, na.rm = T),digits = 0),
                      desviacion = round(sd(`Pre Bas.Act.`, na.rm = T),digits = 0))
        }
        else{
            cuadro <- modif() %>%
                filter(Articulo == input$estadisticos & `Pre Bas.Act.` != 0 & ESTADO %in% c("DISPONIBLE PARA NIVEL CENTRAL","PARA VERIFICAR NIVEL CENTRAL","ANALIZADO CENTRAL","CONTROL DE CALIDAD CENTRAL","REVISADO CENTRAL","DISPONIBLE PARA CALCULO") & PERIODO == input$periodo) %>%  
                group_by(Articulo,Cod.Art.) %>% 
                summarise(conteo = n(), media_actual = round(mean(`Pre Bas.Act.`, na.rm = T),digits = 0),
                          media_anterior = round(mean(`Pre Bas.Ant.`, na.rm = T),digits = 0),
                          mediana = round(median(`Pre Bas.Act.`, na.rm = T),digits = 0),
                          minimo = round(min (`Pre Bas.Act.`, na.rm = T),digits = 0), 
                          maximo = round(max(`Pre Bas.Act.`, na.rm = T),digits = 0),
                          desviacion = round(sd(`Pre Bas.Act.`, na.rm = T),digits = 0))   
        }
        
        cuadro %>% DT::datatable(rownames = FALSE) %>% formatCurrency(c('media_actual','media_anterior', 'mediana','minimo','maximo','desviacion')) %>% 
                   DT::formatStyle(columns = c(1:9), fontSize = "70%")
        
    })  
    
    output$grafico <- renderPlot({
       
        if(input$coti != "select all"){
        nombre<-modif() %>% 
            filter(Articulo==input$estadisticos	 & PERIODO == input$periodo & Ciudad %in% input$coti & Nov.Act. %in% c("NV","SI","CR")) 
        
        extremo <- nombre %>% 
            summarise(upper = quantile(`Pre Bas.Act.`,0.75, na.rm = TRUE) + (1.5 * IQR(`Pre Bas.Act.`, na.rm = TRUE)),
                      lower = quantile(`Pre Bas.Act.`,0.25, na.rm = TRUE) - (1.5 * IQR(`Pre Bas.Act.`, na.rm = TRUE)))
        eti<-nombre %>% filter(Var !=0 & Nov.Act. %in% c("NV","CR","SI","IN"))
        grafica_hist <- nombre %>%  
            ggplot(aes(Articulo, `Pre Bas.Act.`))+
            geom_boxplot()+
            geom_jitter(position = position_jitter(seed = 1), aes(size= Can.Act., color = Ciudad), alpha = 0.6)+
            geom_text_repel(data=eti %>% filter(Var <= -10 | Var >= 10), position = position_jitter(seed = 1), aes(label=ifelse((Var <= -10 | Var >= 10),paste0(CodMpio," ",Var,"%",Nov.Act.), "")))+
            theme_minimal()+
            #scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})+
            ggtitle(nombre$Nom.Art.)
        }
        else{
            nombre<-modif() %>% 
                filter(Articulo==input$estadisticos	 & PERIODO == input$periodo & Nov.Act. %in% c("NV","SI","CR"))
            
            extremo <- nombre %>% 
                summarise(upper = quantile(`Pre Bas.Act.`,0.75, na.rm = TRUE) + (1.5 * IQR(`Pre Bas.Act.`, na.rm = TRUE)),
                          lower = quantile(`Pre Bas.Act.`,0.75, na.rm = TRUE) - (1.5 * IQR(`Pre Bas.Act.`, na.rm = TRUE)))
            eti<-nombre %>% filter((Var != 0 & Nov.Act. %in% c("NV","CR","SI","IN")))
            grafica_hist <- nombre %>%  
                ggplot(aes(Articulo, `Pre Bas.Act.`))+
                geom_boxplot()+
                geom_jitter(position = position_jitter(seed = 1), aes(size= Can.Act., color = Ciudad), alpha = 0.6)+
                geom_text_repel(data=eti %>% filter(Var <= -10 | Var >= 10), position = position_jitter(seed = 1), aes(label=ifelse((Var <= -10 | Var >= 10),paste0(CodMpio," ",Var,"%",Nov.Act.), "")))+
                theme_minimal()+
                #scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})+
                ggtitle(nombre$Nom.Art.) 
        }
        grafica_hist
        
    })
    
    output$tridi <- renderPlotly({
      
      # Set seed for reproducibility
      set.seed(123)
      
      # Function to generate points randomly within a sphere
      points_in_sphere <- function(radius, n_points) {
        u <- runif(n_points, 0, 1)
        v <- runif(n_points, 0, 1)
        w <- runif(n_points, 0, 1)
        
        r <- radius * u^(1/3)
        theta <- 2 * pi * v
        phi <- acos(2 * w - 1)
        
        x <- r * sin(phi) * cos(theta)
        y <- r * sin(phi) * sin(theta)
        z <- r * cos(phi)
        
        return(data.frame(x, y, z))
      }
      
      # Function to generate the surface of a sphere
      sphere_surface <- function(radius, n_points) {
        theta <- seq(0, 2 * pi, length.out = n_points)
        phi <- seq(0, pi, length.out = n_points)
        
        x <- outer(radius * sin(phi), cos(theta))
        y <- outer(radius * sin(phi), sin(theta))
        z <- outer(radius * cos(phi), rep(1, n_points))
        
        return(list(x = c(x), y = c(y), z = c(z)))
      }
      
      
      if(input$coti != "select all"){
      
      data <- modif() %>%
        filter(Articulo==input$estadisticos	 & PERIODO == input$periodo & Ciudad %in% c(input$coti) & Nov.Act. %in% c("NV","SI","CR") & ESTADO %in% c("DISPONIBLE PARA NIVEL CENTRAL","PARA VERIFICAR NIVEL CENTRAL","ANALIZADO CENTRAL","CONTROL DE CALIDAD CENTRAL","REVISADO CENTRAL","DISPONIBLE PARA CALCULO")) 
      
      # Generate data
      n_observations <- length(data$`Pre Bas.Act.`)
      n_observations <- sort(n_observations)
      mean_price <- mean(data$`Pre Bas.Act.`)
      sd_price <- sd(data$`Pre Bas.Act.`)
      
      
      # Calculate overall mean price and standard deviation
      overall_mean_price <- mean(data$`Pre Bas.Act.`)
      overall_sd_price <- sd(data$`Pre Bas.Act.`)
      
      # Calculate overall mean price, standard deviation, and IQR
      overall_mean_price <- mean(data$`Pre Bas.Act.`)
      overall_sd_price <- sd(data$`Pre Bas.Act.`)
      iqr_price <- IQR(data$`Pre Bas.Act.`)
      
      # Generate points inside the sphere
      sphere_points <- points_in_sphere(overall_sd_price, n_observations)
      
      # Adjust points to be centered around the overall mean price
      sphere_points$x <- data$`Pre Bas.Act.`
      sphere_points$y <- sphere_points$y
      sphere_points$z <- sphere_points$z
      
      data <- data %>% arrange(`Pre Bas.Act.`)
      sphere_points <- sphere_points %>% arrange(x)
      sphere_points <- cbind(data,sphere_points)
      
      # Identify outliers based on IQR
      outlier_threshold_upper <- quantile(data$`Pre Bas.Act.`, 0.75) + 1.5 * iqr_price
      outlier_threshold_lower <- quantile(data$`Pre Bas.Act.`, 0.25) - 1.5 * iqr_price
      outliers <- which(data$`Pre Bas.Act.` > outlier_threshold_upper | data$`Pre Bas.Act.` < outlier_threshold_lower)
      
      # Generate the sphere surface
      sphere_surf <- sphere_surface(iqr_price / 2, 50)
      
      ## soporte de la fuente
      
      
      sphere_points_fuente <- sphere_points %>% filter(Id.Fte == input$todo)
      
      tabla <- final %>% 
        filter(str_detect(archivo.x,as.character(input$todo))) %>% 
        mutate(fuente_2 = as.character(input$todo))
      
      
      fuente <- sphere_points_fuente 
      
      fuente <- fuente %>% separate(PERIODO, c("mes","ano"),"_",extra = "merge", remove = FALSE)
      
      fuente <- fuente %>% mutate(mes = str_to_sentence(mes),
                                  ano = paste0("20", str_sub(ano, nchar(ano)-1,nchar(ano))))
      
      
      ## numerar los meses
      
      meses <- data.frame(numero= c("01","02","03","04","05","06","07","08","09","10","11","12"),
                          mes = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")) %>% 
        mutate(mes_num = paste0(numero,". ",mes)) %>% 
        select(-1)
      
      union <- left_join(fuente, meses, by = "mes")
      
      union <- union %>%  mutate(mes = mes_num, Id.Fte = as.character(Id.Fte))
      
      sphere_points_fuente <- left_join(union,tabla, by = c("mes","ano","Id.Fte"="fuente_2")) %>% slice(1)
      
      # Create the 3D plot
      plot <- plot_ly()
      
      plot <- add_markers(plot,
                          x = ~sphere_points$x, 
                          y = ~sphere_points$y, 
                          z = ~sphere_points$z,# Ajusta según los ejes deseados
                          type = "scatter3d", mode = "markers",
                          marker = list(color = "blue", size = 5, opacity = 0.7),
                          text = ~paste("Info:", sphere_points$Ciudad," - Precio:",sphere_points$`Pre Bas.Act.`," - Var:",sphere_points$Var),
                          hoverinfo = 'text') 
      
      plot <- add_markers(plot,
                          x = ~sphere_points_fuente$x, 
                          y = ~sphere_points_fuente$y, 
                          z = ~sphere_points_fuente$z,# Ajusta según los ejes deseados
                          customdata = ~sphere_points_fuente$GOOGLE,
                          type = "scatter3d", mode = "markers",
                          marker = list(color = "orange", size = 8, opacity = 0.7),
                          text = ~paste("Info:","Fuente:", sphere_points_fuente$Id.Fte,"Precio:",sphere_points_fuente$`Pre Bas.Act.`," ",sphere_points_fuente$GOOGLE),
                          hoverinfo = 'text')
      
      # Plot the overall mean price as a central point
      plot <- add_trace(plot, x = ~overall_mean_price, y = ~0, z = ~0,
                        type = "scatter3d", mode = "markers",
                        marker = list(symbol = 'circle', size = 10, opacity = 1, color = 'red'),
                        name = paste("Overall Mean Price:", round(overall_mean_price, 2)))
      
      # Plot the sphere surface
      plot <- add_trace(plot, x = ~sphere_surf$x + overall_mean_price, y = ~sphere_surf$y, z = ~sphere_surf$z,
                        type = "scatter3d", mode = "markers",
                        marker = list(color = "green", size = 1, opacity = 0.3),
                        showlegend = FALSE)
      
      # Annotate outliers in red
      if (length(outliers) > 0) {
        outlier_points <- sphere_points[outliers, ]
        plot <- add_trace(plot, x = ~outlier_points$x, y = ~outlier_points$y, z = ~outlier_points$z,
                          type = "scatter3d", mode = "markers",
                          marker = list(color = "red", size = 5, opacity = 0.7),
                          name = "Outliers")
      }
      
      # Calculate statistics
      overall_mean_price <- mean(data$`Pre Bas.Act.`)
      overall_sd_price <- sd(data$`Pre Bas.Act.`)
      iqr_price <- IQR(data$`Pre Bas.Act.`)
      min_price <- min(data$`Pre Bas.Act.`)
      max_price <- max(data$`Pre Bas.Act.`)
      q1_price <- quantile(data$`Pre Bas.Act.`, 0.25)
      q3_price <- quantile(data$`Pre Bas.Act.`, 0.75)
      
      # Layout configuration
      plot <- plot %>% layout(scene = list(xaxis = list(title = "X"),
                                           yaxis = list(title = "Y"),
                                           zaxis = list(title = "Z")))
      }
      else{
        data <- modif() %>% 
          filter(Articulo==input$estadisticos	 & PERIODO == input$periodo & Nov.Act. %in% c("NV","SI","CR") & ESTADO %in% c("DISPONIBLE PARA NIVEL CENTRAL","PARA VERIFICAR NIVEL CENTRAL","ANALIZADO CENTRAL","CONTROL DE CALIDAD CENTRAL","REVISADO CENTRAL","DISPONIBLE PARA CALCULO")) 
        
      
        # Generate data
        n_observations <- length(data$`Pre Bas.Act.`)
        n_observations <- sort(n_observations)
        mean_price <- mean(data$`Pre Bas.Act.`)
        sd_price <- sd(data$`Pre Bas.Act.`)
        
        
        # Calculate overall mean price and standard deviation
        overall_mean_price <- mean(data$`Pre Bas.Act.`)
        overall_sd_price <- sd(data$`Pre Bas.Act.`)
        
        # Calculate overall mean price, standard deviation, and IQR
        overall_mean_price <- mean(data$`Pre Bas.Act.`)
        overall_sd_price <- sd(data$`Pre Bas.Act.`)
        iqr_price <- IQR(data$`Pre Bas.Act.`)
        
        # Generate points inside the sphere
        sphere_points <- points_in_sphere(overall_sd_price, n_observations)
        
        # Adjust points to be centered around the overall mean price
        sphere_points$x <- data$`Pre Bas.Act.`
        sphere_points$y <- sphere_points$y
        sphere_points$z <- sphere_points$z
        
        data <- data %>% arrange(`Pre Bas.Act.`)
        sphere_points <- sphere_points %>% arrange(x)
        sphere_points <- cbind(data,sphere_points)
        
        # Identify outliers based on IQR
        outlier_threshold_upper <- quantile(data$`Pre Bas.Act.`, 0.75) + 1.5 * iqr_price
        outlier_threshold_lower <- quantile(data$`Pre Bas.Act.`, 0.25) - 1.5 * iqr_price
        outliers <- which(data$`Pre Bas.Act.` > outlier_threshold_upper | data$`Pre Bas.Act.` < outlier_threshold_lower)
        
        # Generate the sphere surface
        sphere_surf <- sphere_surface(iqr_price / 2, 50)
        
        sphere_points_fuente <- sphere_points %>% filter(Id.Fte == input$todo)
        
        tabla <- final %>% 
          filter(str_detect(archivo.x,as.character(input$todo))) %>% 
          mutate(fuente_2 = as.character(input$todo))
        
        
        fuente <- sphere_points_fuente 
        
        fuente <- fuente %>% separate(PERIODO, c("mes","ano"),"_",extra = "merge", remove = FALSE)
        
        fuente <- fuente %>% mutate(mes = str_to_sentence(mes),
                                    ano = paste0("20", str_sub(ano, nchar(ano)-1,nchar(ano))))
        
        
        ## numerar los meses
        
        meses <- data.frame(numero= c("01","02","03","04","05","06","07","08","09","10","11","12"),
                            mes = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")) %>% 
          mutate(mes_num = paste0(numero,". ",mes)) %>% 
          select(-1)
        
        union <- left_join(fuente, meses, by = "mes")
        
        union <- union %>%  mutate(mes = mes_num, Id.Fte = as.character(Id.Fte))
        
        sphere_points_fuente <- left_join(union,tabla, by = c("mes","ano","Id.Fte"="fuente_2")) %>% slice(1)
        
        # Create the 3D plot
        plot <- plot_ly()
        
        # Plot the points inside the sphere
        plot <- add_markers(plot,
                            x = ~sphere_points$x, 
                            y = ~sphere_points$y, 
                            z = ~sphere_points$z,# Ajusta según los ejes deseados
                            type = "scatter3d", mode = "markers",
                            marker = list(color = "blue", size = 5, opacity = 0.7),
                            text = ~paste("Info:", sphere_points$Ciudad," - Precio:",sphere_points$`Pre Bas.Act.`," - Var:",sphere_points$Var),
                            hoverinfo = 'text')
        
        plot <- add_markers(plot,
                            x = ~sphere_points_fuente$x, 
                            y = ~sphere_points_fuente$y, 
                            z = ~sphere_points_fuente$z,# Ajusta según los ejes deseados
                            customdata = ~sphere_points_fuente$GOOGLE,
                            type = "scatter3d", mode = "markers",
                            marker = list(color = "orange", size = 5, opacity = 0.7),
                            text = ~paste("Info:","Fuente:", sphere_points_fuente$Id.Fte.,"Precio:",sphere_points_fuente$`Pre Bas.Act.`," ", sphere_points_fuente$GOOGLE),
                            hoverinfo = 'text')
        
        # Plot the overall mean price as a central point
        plot <- add_trace(plot, x = ~overall_mean_price, y = ~0, z = ~0,
                          type = "scatter3d", mode = "markers",
                          marker = list(symbol = 'circle', size = 10, opacity = 1, color = 'red'),
                          name = paste("Overall Mean Price:", round(overall_mean_price, 2)))
        
        # Plot the sphere surface
        plot <- add_trace(plot, x = ~sphere_surf$x + overall_mean_price, y = ~sphere_surf$y, z = ~sphere_surf$z,
                          type = "scatter3d", mode = "markers",
                          marker = list(color = "green", size = 1, opacity = 0.3),
                          showlegend = FALSE)
        
        # Annotate outliers in red
        if (length(outliers) > 0) {
          outlier_points <- sphere_points[outliers, ]
          plot <- add_trace(plot, x = ~outlier_points$x, y = ~outlier_points$y, z = ~outlier_points$z,
                            type = "scatter3d", mode = "markers",
                            marker = list(color = "red", size = 5, opacity = 0.7),
                            name = "Outliers")
        }
        
        # Calculate statistics
        overall_mean_price <- mean(data$`Pre Bas.Act.`)
        overall_sd_price <- sd(data$`Pre Bas.Act.`)
        iqr_price <- IQR(data$`Pre Bas.Act.`)
        min_price <- min(data$`Pre Bas.Act.`)
        max_price <- max(data$`Pre Bas.Act.`)
        q1_price <- quantile(data$`Pre Bas.Act.`, 0.25)
        q3_price <- quantile(data$`Pre Bas.Act.`, 0.75)
        
        
        
        # Layout configuration
        plot <- plot %>% layout(scene = list(xaxis = list(title = "Precio Base"),
                                             yaxis = list(title = "Y"),
                                             zaxis = list(title = "Z")))
       
      }
      plot
      
    })
    
    # Función para abrir archivo dependiendo del sistema operativo
    open_file <- function(GOOGLE) {
      if (.Platform$OS.type == "windows") {
        shell.exec(GOOGLE)
      } else if (Sys.info()["sysname"] == "Darwin") {
        system(paste("open", shQuote(GOOGLE)))
      } else if (Sys.info()["sysname"] == "Linux") {
        system(paste("xdg-open", shQuote(GOOGLE)))
      } else {
        showNotification("No se puede abrir el archivo en este sistema operativo.", type = "error")
      }
    }
    
    # Observador para los clics en el gráfico
    observeEvent(event_data("plotly_click"), {
      click_data <- event_data("plotly_click")
      if (!is.null(click_data)) {
        url <- click_data$customdata
        if (!is.null(url)) {
          open_file(url)  # Abre la URL en una nueva pestaña del navegador
        }else {
          showNotification("El archivo no existe en el servidor.", type = "error")
        }
      } else {
        showNotification("No hay archivo asociado a este punto.", type = "error")
      }
    }
    )
    
    output$estado <- renderDataTable({
        if(input$coti != "select all"){
        estado <- modif() %>% 
            filter(Articulo == input$estadisticos & PERIODO == input$periodo & Ciudad %in% input$coti) %>% 
            dplyr::select(1,4,5,9,11,15:23,26) %>% 
            arrange(desc(Var))
        }
        else{
            estado <- modif() %>%  
                filter(Articulo == input$estadisticos & PERIODO == input$periodo) %>% 
                dplyr::select(1,4,5,9,11,15:23,26) %>% 
                arrange(desc(Var))  
        }
        estado %>% DT::datatable(rownames = FALSE) %>% 
                   DT::formatStyle(columns = c(1:15), fontSize = "70%")
    })
    
    
   
     ## articulos que varian
    output$tendencia<- renderPlot({
        if(input$coti != "select all"){
        tender <- modif() %>% mutate(PERIODO = factor(PERIODO, levels = c("FEBRERO_2022","MARZO_2022","ABRIL_2022","MAYO_2022","JUNIO_2022","JULIO_2022","AGOSTO_2022","SEPTIEMBRE_2022","OCTUBRE_2022","NOVIEMBRE_2022","DICIEMBRE_2022",
                                                                          "ENERO_2023","FEBRERO_2023","MARZO_2023","ABRIL_2023","MAYO_2023","JUNIO_2023","JULIO_2023","AGOSTO_2023","SEPTIEMBRE_2023","OCTUBRE_2023","NOVIEMBRE_2023",
                                                                          "DICIEMBRE_2023","ENERO_2024","FEBRERO_2024","MARZO_2024","ABRIL_2024","MAYO_2024","JUNIO_2024","JULIO_2024")),
                                      Fuente = paste0(Id.Fte,"-",`Nombre Fuente`)) %>% 
            filter(Articulo == input$estadisticos, PERIODO == input$periodo, Var != 0 & Nov.Act. %in% c("NV","SI") & Ciudad %in% input$coti & Nov.Act. != "PE") %>%  
            arrange(desc(Var))
        }
        else{
            tender <- modif() %>% mutate(PERIODO = factor(PERIODO, levels = c("FEBRERO_2022","MARZO_2022","ABRIL_2022","MAYO_2022","JUNIO_2022","JULIO_2022","AGOSTO_2022","SEPTIEMBRE_2022","OCTUBRE_2022","NOVIEMBRE_2022","DICIEMBRE_2022",
                                                                              "ENERO_2023","FEBRERO_2023","MARZO_2023","ABRIL_2023","MAYO_2023","JUNIO_2023","JULIO_2023","AGOSTO_2023","SEPTIEMBRE_2023","OCTUBRE_2023","NOVIEMBRE_2023",
                                                                              "DICIEMBRE_2023","ENERO_2024","FEBRERO_2024","MARZO_2024","ABRIL_2024","MAYO_2024","JUNIO_2024","JULIO_2024")),
                                           Fuente = paste0(Id.Fte,"-",`Nombre Fuente`)) %>% 
                filter(Articulo == input$estadisticos, PERIODO == input$periodo, Var != 0 & Nov.Act. %in% c("NV","SI") & Nov.Act. != "PE") %>%  
                arrange(desc(Var))
        }
        
        tendencia <- unique(tender)
        
         ggplot(tendencia, aes(x= reorder(Fuente, desc(Var)), y=Var, fill = Ciudad))+
            geom_col()+
            geom_hline(yintercept = 0, color= 1, lwd = 0.2)+
            theme(legend.position = "right", axis.text = element_text(angle = 90))+
            scale_x_discrete(labels= substr(tendencia$Fuente, 1,15))
        
        
    })
    
    ## registros por fuente
    output$fuentes <- renderDataTable({
        
        empresas <- historico %>%
            mutate(Id.Fte = as.factor(Id.Fte), 
                   Fuente = paste0(Id.Fte,"-",`Nombre Fuente`)) %>% 
            filter(PERIODO == input$periodo, Id.Fte == input$todo) %>% 
            dplyr::select(Nom.Art., `Pre Bas.Ant.`,`Pre Bas.Act.`,Nov.Act.,Var) 
            
        
        DT::datatable(empresas, rownames = FALSE)
        
    }) 
    
    
    
    output$contri <- renderPlot({
        contri <-modif()%>% 
            filter(Articulo == input$estadisticos & PERIODO == input$periodo  & `Pre Bas.Act.` != 0 & Nov.Act.!= "CR" & ESTADO %in% c("DISPONIBLE PARA NIVEL CENTRAL","PARA VERIFICAR NIVEL CENTRAL","ANALIZADO CENTRAL","CONTROL DE CALIDAD CENTRAL","REVISADO CENTRAL","DISPONIBLE PARA CALCULO")) %>%
            group_by(Ciudad, Articulo) %>%
            summarise(Cotizaciones = n(), 
                      VPL = round(mean(Var, na.rm = TRUE),2)) # variacion promedio local
        
        VPAN <- sum(abs(contri$VPL)) # variacion promedio nacional acumulada
        
        prom_nal <-modif() %>%
            filter(Articulo == input$estadisticos & PERIODO == input$periodo & `Pre Bas.Act.` != 0 & Nov.Act.!= "CR" & ESTADO %in% c("DISPONIBLE PARA NIVEL CENTRAL","PARA VERIFICAR NIVEL CENTRAL","ANALIZADO CENTRAL","CONTROL DE CALIDAD CENTRAL","REVISADO CENTRAL","DISPONIBLE PARA CALCULO")) %>%
            group_by(Articulo)%>%
            summarise(Varia_m = round(mean(Var),2)) # variacion promedio nacional
        
        contri_nal <- contri %>% 
            mutate(Contribucion = round(abs(VPL)/VPAN*100,2),
                   Real= round((Contribucion/100) * prom_nal$Varia_m,2))
    
    eti <- contri_nal %>% filter(Contribucion !=0)
    
    ggplot(contri_nal, aes(area= Contribucion, fill = Ciudad, label =paste0(Contribucion,"%"," (",Ciudad,")")))+
        geom_treemap()+
        geom_treemap_text(data=eti,color = "black",place = "center", size = 10)+
        theme(legend.position = "none")
    
    })
   
    
}

# Run the application 
shinyApp(ui = ui, server = server)
