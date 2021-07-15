# Sesión 8. Dashboards con Shiny - Entorno GUI.

# Postwork 8. Sesión del 08/07/2021.


# Instrucciones (obtenidas directamente del repositorio de BEDU en la sesión correspondiente):

# 1. Para este "postwork" genera un dashboard en un solo archivo 'app.R'; para ello realiza lo siguiente.
# 2. Ejecuta el código "momios.R".
# 3. Almacena los gráficos resultantes en formato ".png".
# 4. Crea un "dashboard" donde se muestren los resultados con cuatro pestañas;
# 4.1. Una pestaña con gráficas de barras, donde en el eje de las 'X' se muestren los goles de los equipos
# local y visitante, con un menú de selección (en 'choices()' deberán aparecer estas variables). Utiliza la
# geometría de tipo barras, además de aplicar un 'facet_wrap()' con la variable del equipo visitante.
# 4.2. Una pestaña donde agregues las imágenes de las gráficas del "Postwork 3".
# 4.3. Otra pestaña donde coloques el "data table" del fichero 'match.data.csv'.
# 4.4. Por último, una pestaña donde agregues las imágenes de las gráficas de los factores de ganancia promedio
# y máximo.


# SOLUCIÓN:

# Si es necesario, deberán instalarse las siguientes librerías:
# "shiny", "shinydashboard", "shinythemes", "ggplot2", "plotly", "dplyr", "tidyr", "ggfortify", "autoplotly",
# "zoo", "ggthemes", "fbRanks", "DT", "dashboardthemes", "fbranks", "devtools".

library(devtools)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(ggfortify)
library(autoplotly)
library(zoo)
library(ggthemes)
library(fbRanks)
library(DT)
library(dashboardthemes)

match.data <- read.csv("match.data.csv", stringsAsFactors = FALSE)
match.data$date <- as.Date(match.data$date)

data.1720 <- read.csv("data_1720.csv")

mom.max <- read.csv("momios_max.csv")
mom.prom <- read.csv("momios_prom.csv")

listasoccer <- create.fbRanks.dataframes("soccer.csv")
fecha <- unique(listasoccer$scores$date)

# Se crea un 'theme' personalizado:
customTheme <- shinyDashboardThemeDIY(

# General:
  appFontFamily = "Arial"
  ,appFontColor = "#F0F0F0"
  ,primaryFontColor = "#F0F0F0"
  ,infoFontColor = "#F0F0F0"
  ,successFontColor = "#F0F0F0"
  ,warningFontColor = "#FF7F7F"
  ,dangerFontColor = "#FF7878"
  ,bodyBackColor = "#172127"
  
# Header:
  ,logoBackColor = "#414A52"

  ,headerButtonBackColor = "#414A52"
  ,headerButtonIconColor = "#F0F0F0"
  ,headerButtonBackColorHover = "#FF6A39"
  ,headerButtonIconColorHover = "#F0F0F0"
  
  ,headerBackColor = "#414A52"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
# Sidebar:
  ,sidebarBackColor = "#32393F"
  ,sidebarPadding = "0"
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = "0"
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "#F0F0F0"
  
  ,sidebarSearchBackColor = "#F0F0F0"
  ,sidebarSearchIconColor = "#646464"
  ,sidebarSearchBorderColor = "#DCDCDC"
  
  ,sidebarTabTextColor = "#F0F0F0"
  ,sidebarTabTextSize = "14"
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = "0"
  
  ,sidebarTabBackColorSelected = "#E6E6E6"
  ,sidebarTabTextColorSelected = "#0F0F0F"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "#F5F5F5"
  ,sidebarTabTextColorHover = "#0F0F0F"
  ,sidebarTabBorderStyleHover = "none solid none none"
  ,sidebarTabBorderColorHover = "#C8C8C8"
  ,sidebarTabBorderWidthHover = "4"
  ,sidebarTabRadiusHover = "0px"
  
# Boxes:
  ,boxBackColor = "#172127"
  ,boxBorderRadius = "5"
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = "18"
  ,boxDefaultColor = "#F0F0F0"
  ,boxPrimaryColor = "#5F9BD5"
  ,boxInfoColor = "#B4B4B4"
  ,boxSuccessColor = "#70AD47"
  ,boxWarningColor = "#ED7D31"
  ,boxDangerColor = "#E84C22"
  
  ,tabBoxTabColor = "#F0F0F0"
  ,tabBoxTabTextSize = "14"
  ,tabBoxTabTextColor = "#F0F0F0"
  ,tabBoxTabTextColorSelected = "#2D2D2D"
  ,tabBoxBackColor = "#32393F"
  ,tabBoxHighlightColor = "#FF6A39"
  ,tabBoxBorderRadius = "5"
  
# Inputs:
  ,buttonBackColor = "#E2EAF0"
  ,buttonTextColor = "#2D2D2D"
  ,buttonBorderColor = "#2D2D2D"
  ,buttonBorderRadius = "5"
  
  ,buttonBackColorHover = "#8B9094"
  ,buttonTextColorHover = "#2D2D2D"
  ,buttonBorderColorHover = "#969696"
  
  ,textboxBackColor = "#B37359"
  ,textboxBorderColor = "#D2D7DB"
  ,textboxBorderRadius = "5"
  ,textboxBackColorSelect = "#2D2D2D"
  ,textboxBorderColorSelect = "#D2D7DB"
  
# Tables:
  ,tableBackColor = "#2D2D2D"
  ,tableBorderColor = "#3D3D3D"
  ,tableBorderTopSize = "1"
  ,tableBorderRowSize = "1"
)

# Se agrega el logo de BEDU:
logo <- tags$a(tags$img(src="bedulogo.png", height='40', width='140'))

ui <- 
  
  dashboardPage(title="Dashboard de match.games | BEDU",
                
                dashboardHeader(title=logo),
                
                dashboardSidebar(
                  
# Se crean botones personalizados para el "sidebar":
                  sidebarMenu(
                    menuItem("Gráfica goles local y visitante", tabName = "goles", icon=icon("futbol")),
                    menuItem("Gráficas postwork 3", tabName = "pwork3", icon = icon("chart-bar")),
                    menuItem("Tabla match.data", tabName = "dtable", icon = icon("table")),
                    menuItem("Gráficas factores de ganancia", tabName = "momios", icon = icon("money-bill-wave")),
                    menuItem("Serie de tiempo", tabName = "tseries", icon = icon("clock")),
                    menuItem("Tabla de predicciones", tabName = "pred", icon = icon("lightbulb"))
                  )
                  
                ),
                
                dashboardBody(
                  customTheme, # Se implementa un tema personalizado y se agrega el título.
                  
                  tags$head(tags$style(HTML(
                    '.myClass { 
                    font-size: 20px;
                    line-height: 50px;
                    text-align: left;
                    font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
                    padding: 0 15px;
                    overflow: hidden;
                    color: white;
                  }
                '))),
                  tags$script(HTML('
                  $(document).ready(function() {
                    $("header").find("nav").append(\'<span class="myClass"> Dashboard de match.games | Programación y estadística con R </span>\');
                  })
                 ')),
                  
                  tabItems(
                    
# Se crean los "tabs" para las gráficas que se incluirán dentro del "dashboard":
                    tabItem(tabName = "goles",
                            fluidRow(
                              titlePanel("Gráfica de barras de goles local y visitante"),
                              selectInput("score.sel", "Selecciona visitante o local", 
                                          c("home.score", "away.score")),
                              plotlyOutput("goles.plot", height=600, width=1000)
                              
                            )
                    ),
                    
                    tabItem(tabName = "pwork3",
                            fluidRow(
                              titlePanel("Gráficas postwork 3"),
                              box(plotlyOutput("pwork31")),
                              box(plotlyOutput("pwork32")),
                              box(plotlyOutput("pwork33"))
                              
                            )
                    ),
                    
                    tabItem(tabName = "dtable",
                            fluidRow(        
                              titlePanel(h3(" Tabla match.data")),
                              box(dataTableOutput ("data.table"))
                            )
                    ),
                    
                    tabItem(tabName = "momios",
                            fluidRow(
                              titlePanel(h3(" Gráficas momios máximos y promedios")),
                              box(plotlyOutput("mom.max.plot")),
                              box(plotlyOutput("mom.prom.plot"))
                            )
                    ),
                    
                    tabItem(tabName = "tseries",
                            fluidRow(
                              titlePanel("Serie de tiempo del promedio de goles mensuales"),
                              selectInput("ts.st.y", "Selecciona la fecha inicio serie de tiempo - Año, mes, día",
                                          c(2010:2020)),
                              selectInput("ts.st.m", "", c(1:12)),
                              selectInput("ts.st.d", "", c(1:31)),
                              selectInput("ts.ed.y", "Selecciona la fecha fin serie de tiempo - Año, mes, día",
                                          c(2010:2020)),
                              selectInput("ts.ed.m", "", c(1:12)),
                              selectInput("ts.ed.d", "", c(1:31)),
                              box(plotlyOutput("tsplot"))
                              
                            )
                    ),
                    
                    tabItem(tabName = "pred",
                            fluidRow(        
                              titlePanel("Tabla de predicciones"),
                              selectInput("pred.date", "Selecciona la fecha a predecir",
                                          fecha),
                              h5("Se usarán datos de los juegos anteriores a la fecha seleccionada"),
                              box(DT::dataTableOutput("pred.table"))
                            )
                    )
                  )
                )
  )

server <- function(input, output) {
# Gráfica de barras:
  output$goles.plot <- renderPlotly({
    goles.data <- mutate(match.data, RES=ifelse(home.score==away.score, "T", ifelse(home.score>away.score, "H", "A")))
    x <- goles.data[,input$score.sel]
    g <- ggplot(goles.data, aes(x, fill=RES)) +
      geom_bar() +
      facet_wrap("away.team") +
      labs(x=input$score.sel, y="Goles")
    
    ggplotly(g)
  })
  
# Gráficas:
  games.FTHG <- select(data.1720, FTHG)
  games.FTAG <- select(data.1720, FTAG)
  games.goals <- select(data.1720, FTHG, FTAG)
  
  num <- nrow(games.goals)
  
  FTHG.mprob <- round(table(games.FTHG) / num, 4)
  FTAG.mprob <- round(table(games.FTAG) / num, 4)
  games.cprob <- round(table(games.goals) / num, 4)
  
  FTAG.mprob.df <- as.data.frame(FTAG.mprob)
  FTHG.mprob.df <- as.data.frame(FTHG.mprob)
  games.cprob.df <- as.data.frame(games.cprob)
  
  names(FTAG.mprob.df) <- c("goles_afuera", "prob_marginal")
  names(FTHG.mprob.df) <- c("goles_casa", "prob_marginal")
  names(games.cprob.df) <- c("goles_casa", "goles_afuera", "prob_marginal")
  
  output$pwork31 <- renderPlotly({ 
    pwork1 <- ggplot(games.cprob.df, aes(x=goles_casa, y=goles_afuera, fill=prob_marginal)) + 
      geom_tile() +
      xlab("Goles en casa") +
      ylab("Goles de visitante") +
      labs(fill= "Probabilidad", 
           title = paste("Heatmap de la probabilidad conjunta de las anotaciones"))+
      scale_fill_continuous(name = "Prob. Conjunta")+
      scale_fill_gradient(low = "#ece7f2", high = "#2b8cbe")
    
    ggplotly(pwork1)
  })
  
  output$pwork32 <- renderPlotly({ 
    pwork2 <- ggplot(FTHG.mprob.df, aes(x=goles_casa, y=prob_marginal)) +
      geom_bar(stat="identity", color="#172127", fill="#172127") +
      xlab("Goles anotados") +
      ylab("Probabilidad marginal")+
      labs(title = paste("Probabilidad marginal - Anotaciones del equipo en casa"))
    
    ggplotly(pwork2)
  })
  
  output$pwork33 <- renderPlotly({ 
    pwork3 <- ggplot(FTAG.mprob.df, aes(x=goles_afuera, y=prob_marginal)) +
      geom_bar(stat="identity", color="#FF6A39", fill="#FF6A39") +
      xlab("Goles anotados") +
      ylab("Probabilidad marginal")+
      labs(title = paste("Probabilidad marginal - Anotaciones del equipo visitante"))
    
    ggplotly(pwork3)
  })
  
# Data table de 'match.games':
  output$data.table <- renderDataTable(match.data, options = list(aLengthMenu = c(5,25,50),
                                                                  iDisplayLength = 5))
  
# Gráfica de factores de ganancia máximos:
  output$mom.max.plot <- renderPlotly({ 
    p <- ggplot(mom.max, aes(x=Num_Ap, y=Capital)) + geom_line( color="purple") + geom_point() +
      labs(x = "Número de juego",
           y = "Capital",
           title = "Escenario con momios máximos") +
      theme(plot.title = element_text(size=12))  +
      theme(axis.text.x = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1),
            axis.text.y = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1))
    ggplotly(p)
  }) 
  
# Gráfica de factores de ganancia promedios:
  output$mom.prom.plot <- renderPlotly({ 
    pp <- ggplot(mom.prom, aes(x=Num_Ap, y=Capital)) + geom_line( color="purple") + geom_point() +
      labs(x = "Número de juego",
           y = "Capital",
           title = "Escenario con momios promedios") +
      theme(plot.title = element_text(size=12))  +
      theme(axis.text.x = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1),
            axis.text.y = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1))
    
    ggplotly(pp)
  })
  
# Serie de tiempo personalizable:
  output$tsplot <- renderPlotly({
    ts.data <- match.data
    ts.data[,"sumagoles"] <- ts.data[,5] + ts.data[,3]
    
    ts.data[,"anio"] <- as.numeric(format(ts.data$date, "%Y"))
    ts.data[,"mes"] <- as.numeric(format(ts.data$date, "%m"))
    
    promedios <- ts.data %>% group_by(anio, mes) %>% summarise(prom_gol_pmes = mean(sumagoles), .groups = "drop")
    promedios <- as.data.frame(promedios)
    
    (promedios <- unite(promedios, Fecha, c("anio","mes"), sep="-"))
    
    promedios[,2]
    prom.ts <- ts(promedios$prom_gol_pmes,
                  start = c(input$ts.st.y,input$ts.st.m,input$ts.st.d),
                  end = c(input$ts.ed.y,input$ts.ed.m,input$ts.ed.d),
                  frequency = 12)
    
    autoplotly(as.zoo(prom.ts))
  })
  
# Tabla de predicciones, también personalizable:
  output$pred.table <- DT::renderDataTable({
    anotaciones <- listasoccer$scores
    equipos <- listasoccer$teams
    
    input.date <- as.Date(input$pred.date)
    
    imaxd <- fecha[(which(fecha==input.date) - 1)]
    
    ranking <- rank.teams(scores=anotaciones, teams=equipos, max.date=imaxd, min.date="2017-08-18")
    
    pred <- predict(ranking, date=input.date)
    
    pred.df <- data.frame(Fecha=pred$scores$date, Eq.Local=pred$scores$home.team,
                          Eq.Visita=pred$scores$away.team, Prob.Gana.L=round(pred$scores$home.win,2),
                          Prob.Gana.V=round(pred$scores$away.win,2), Prob.Empate=round(pred$scores$tie,2),
                          Punt.L.Pred=round(pred$scores$pred.home.score,2), Punt.V.Pred=round(pred$scores$pred.away.score,2),
                          Punt.L.Real=pred$scores$home.score, Punt.V.Real=pred$scores$away.score)
    
    pred.df <- mutate(pred.df, Resultado.Real=ifelse(Punt.L.Real==Punt.V.Real, "T", ifelse(Punt.L.Real>Punt.V.Real, "H", "A")))
    
    DT::datatable(pred.df)
  })
}

shinyApp(ui, server)
