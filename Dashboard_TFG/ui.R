# Definición de la interfaz de usuario
ui <-  fluidPage(theme = shinytheme("flatly"),
                 
  # Dashboard página
  dashboardPage(
    # Header
    dashboardHeader(disable = TRUE),
    # SideBar
    dashboardSidebar(disable = TRUE),
    # Body
    dashboardBody(
      navbarPage("Sesgo de género en la participación de 'Comunica2'",
        tabPanel("General", icon = icon("house"),          
            column(width = 5,
              valueBoxOutput("nConf", width = NULL),
              fluidRow(
                tabBox(
                  title = "Número de preguntas o comentarios por año según género",
                  id = "tabset1",
                  tags$style(".nav-tabs-custom .nav-tabs li.active {border-top-color: ##FF911D;}"),
                  tabPanel("Total", plotlyOutput("grafAbsAnualMH")),
                  tabPanel("Proporción", plotlyOutput("grafRelAnualMH")),
                  width = 12
                )
              ),
              fluidRow(width = 5,
                valueBoxOutput("mediaDurHombres", width = 6),
                valueBoxOutput("mediaDurMujeres", width = 6)
              )
            ),
            column(width = 7, 
              fluidRow(
                valueBoxOutput("propPonenteH",width = 6),
                valueBoxOutput("propPonenteM",width = 6),
              ),
              fluidRow(
                valueBoxOutput("pregsHombresTotal", width = 6),
                valueBoxOutput("pregsMujeresTotal", width = 6)
              ),
              fluidRow(
                box(
                  title = "Duración de las preguntas (segundos) según género",
                  solidHeader = TRUE,
                  plotlyOutput("grafdurMHui",height = "442px"),
                  width = 7
                ),
                box(
                  title = "Proporción de siguientes preguntas hechas por mujeres dependiendo de quien realizó la primera",
                  solidHeader = TRUE,
                  plotlyOutput("grafSigPregsMui",height = "415px"),
                  width = 5
                )
              )
            )
        ),
        
        tabPanel("Por años", icon = icon("glyphicon glyphicon-calendar", lib = "glyphicon"),
          fluidRow(
            column(width = 4,
              box(
                title = "Medianas de las duraciones (segundos) por año según género",
                solidHeader = TRUE,
                plotlyOutput("grafDurMedMHui"),
                width = 12
              )
            ),
            column(width = 4,
              box(
                title = "Duración combinada total (segundos) por año según género",
                solidHeader = TRUE,
                plotlyOutput("grafDurCombMHui"),
                width = 12
              )
            ),
            column(width = 4,
              box(
                title = "Número de preguntas de más de 60 segundos por año según género",
                solidHeader = TRUE,
                plotlyOutput("grafDurMas60MHui"),
                width = 12
              )
            )
          ),
          fluidRow(
            column(width = 4,
              box(
                title = "Número de primeras preguntas por año según género",
                solidHeader = TRUE,
                plotlyOutput("grafPrimPregsui"),
                width = 12
              )
            ),
            column(width = 4,
              tabBox(
                title = "Género de segundas preguntas dependiendo de quien realizó la primera (por año)",
                id = "tabset2",
                tabPanel("Hombre", plotlyOutput("grafPrimSegHui", height = "345px")),
                tabPanel("Mujer", plotlyOutput("GrafPrimSegMui", height = "345px")),
                width = 12,
              )
            ),
            column(width = 4,
                   box(
                     title = "Proporción de siguientes preguntas hechas por mujeres dependiendo de quien realizó la primera (por año)",
                     solidHeader = TRUE,
                     plotlyOutput("grafSigPregsManualui"),
                     width = 12
                   )
            ),
          )
        ),
        
        tabPanel("Por género", icon = icon("person-half-dress"),
          box(solidHeader = TRUE, width = NULL,
            fluidRow(
              column(width = 6,
                selectInput("selectAño", label = h3("Selecciona el año "), 
                        choices = rev(años)),
                hr()
              ),
              valueBoxOutput("propDurCombM", width = 6)
            ),
            fluidRow(
              column(width = 4,
                     box(title = "Número de preguntas según género",
                       solidHeader = TRUE,
                       plotlyOutput("selectGrafAnualMH"),
                       width = 12)
              ),
              column(width = 4,
                     box(title = "Duración de las preguntas (segundos) según género",
                       solidHeader = TRUE,
                       plotlyOutput("selectGrafDurMH"),
                       width = 12)
              ),
              column(width = 4,
                     box(title = "Número de individuos que hablan más de 60 segundos",
                         solidHeader = TRUE,
                         plotlyOutput("selectGrafDurMas60MH"),
                         width = 12)
              )
            ),
            fluidRow(
              column(width = 4,
                     box(title = "Número de primeras preguntas realizadas según género",
                       solidHeader = TRUE,
                       plotlyOutput("selectGrafPrimPregs"),
                       width = 12)
                     ),
              column(width = 4,
                     box(title = "Número de segundas preguntas por género de quien realizó la primera pregunta",   
                       solidHeader = TRUE,
                       plotlyOutput("selectGrafPrimSeg"),
                       width = 12)
              ),
              column(width = 4,
                     box(title = "Proporción de siguientes preguntas hechas por mujeres dependiendo de quien realizó la primera",
                         solidHeader = TRUE,
                         plotlyOutput("selectGrafSigPregsM"),
                         width = 12)
              )
            )
          )
        ),
        
        tabPanel("Ponentes", icon = icon("chalkboard-user"), 
          fluidRow(    
            column(width = 6,
              fluidRow(
                valueBoxOutput("nponentesHui", width = 6),
                valueBoxOutput("nponentesMui", width = 6),
              ),
              fluidRow(
                box(title = "Número de conferencias según el género de los ponentes",
                    solidHeader = TRUE,
                    plotlyOutput("grafPonConfLollipop"),
                    width = 12
                )
              )
            ),
            column(width = 6,
              box(
                solidHeader = TRUE,
                radioButtons("radioB", label = h3("Temáticas según género del ponente"), 
                                   choices = list("Hombre" = 1, "Mujer" = 2, "Todas las temáticas" = 3),
                                   selected = 1, 
                                   inline = TRUE),
                hr(),
                conditionalPanel(condition = 'input.radioB == 1', plotOutput("wordcloudH", height = "435px")),
                conditionalPanel(condition = 'input.radioB == 2', plotOutput("wordcloudM", height = "435px")),
                conditionalPanel(condition = 'input.radioB == 3', plotOutput("wordcloudMH", height = "435px")),
                width = 12
              )
            )
          ),
          fluidRow(
            box(title = "Ponencias compuestas por: Mínimo un hombre VS. Mínimo una mujer",
                solidHeader = TRUE,
                plotlyOutput("grafNumConfMinMHuiP"),
                width = 4
            ),
            box(title = "Ponencias compuestas por: Exclusivamente hombres VS. Mínimo una mujer",
                solidHeader = TRUE,
                plotlyOutput("grafNumConfMHuiP"),
                width = 4
            ),
            box(title = "Participación dependiendo del género del ponente",
                solidHeader = TRUE,
                plotlyOutput("grafPropXponXuiP"),
                width = 4
            )
          )
        )
      ) 
    )  
  )
)

