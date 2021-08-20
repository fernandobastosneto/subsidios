#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 

library(barao)
library(comerciobr)

retirar <- c("Não Definido", "Provisão de Navios e Aeronaves", "Bancos Centrais",
                          "A Designar", "Niue", "Cocos (Keeling), Ilhas", "Marianas do Norte, Ilhas",
                          "Wake, Ilha", "Sint Maarten", "Midway, Ilhas", "Guernsey", 
                          "Madeira, Ilha da", "Território Britânico do Oceano Índico", 
                          "São Pedro e Miquelon", "Svalbard e Jan Mayen", "Terras Austrais Francesas",
                          "Geórgia do Sul e Sandwich do Sul, Ilhas", "Heard e ilhas mcdonald, Ilha",
                          "Organizações Internacionais", "Mayotte", "Guam", "São Bartolomeu",
                          "Lebuan, Ilhas", "Bouvet, Ilha", "Brasil", "Antártica", "Madagascar")

comtrade_files <- fs::dir_ls(here::here("data", "relatorios_comerciomundo")) %>% 
  stringr::str_remove(paste0(here::here("data", "relatorios_comerciomundo/"), "comerciomundo_")) %>%
  stringr::str_remove(".pdf$")
mdic_files <- fs::dir_ls(here::here("data", "relatorios_comerciobr")) %>%
  stringr::str_remove(paste0(here::here("data", "relatorios_comerciobr/"), "comerciobr_")) %>%
  stringr::str_remove(".pdf$")
paises_bancomundial <- readr::read_csv2(here::here("data/Paises_BancoMundial.csv"))

relatorio_bancomundial <- function(pais) {
  paises_bancomundialfiltrado <- paises_bancomundial %>% 
    dplyr::filter(NO_PAIS == pais)
  filtro <- paises_bancomundialfiltrado$NO_PAIS
  filtro_bancomundial <- paises_bancomundialfiltrado$CO_PAIS_ISOA3
  rmarkdown::render(here::here("inst/rmd", "notebook_bancomundial.Rmd"))
}

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    # fluidPage(
    #   tags$head(
    #     tags$style(
    #       HTML(".shiny-notification {
    #          position:fixed;
    #          top: calc(50%);
    #          left: calc(30%);
    #          }"
    #       )
    #     )
    #   ),
    #   tags$img(src = "https://cordilheira.itamaraty.gov.br/vendor/mre-layout/skins/login/assets/img/logo-palacio.svg",
    #            align = "center"),
    #   h3("Secretaria de Comércio Exterior e Assuntos Econômicos"),
    #   h4("Subsídios Econômicos e Comerciais"),
    #   tabsetPanel(
    #     tabPanel("Dados Comerciais",
    #              fluidRow(
    #                column(2, wellPanel(selectInput("mdic", "Brasil-País", mdic_files), 
    #                                    downloadButton("mdic_report", "Download"), align = "left"))),
    #              fluidRow(
    #                column(2, wellPanel(selectInput("comtrade", "País-Mundo", stringr::str_remove(comtrade_files, ".pdf")),
    #                                    downloadButton("comtrade_report", "Download"), align = "left")))
    #     ),
    #     tabPanel("Dados Econômicos",
    #              fluidRow(
    #                column(2, wellPanel(selectInput("dataset3", "Escolha um país", paises_bancomundial$NO_PAIS),
    #                                    downloadButton("report3", "Download"), align = "left")))
    #     )
    #   )
    # )
    
    shinydashboard::dashboardPage(
      skin = "black",
      shinydashboard::dashboardHeader(
        title = "Subsídios Econômicos",
        titleWidth = 250),
      shinydashboard::dashboardSidebar(
        width = 250,
        shinydashboard::sidebarMenu(
          id = "tabs",
            selectInput(
              "pais", "Países",
              choices = comerciobr::dic_paises$no_pais[!comerciobr::dic_paises$no_pais %in% retirar],
              selected = "China"),
            shinydashboard::menuItem("Comércio", icon = icon("bar-chart-o"),
              shinydashboard::menuSubItem("Comércio Brasil-País", tabName = "comerciobr"),
              shinydashboard::menuSubItem("Comércio País-Mundo", tabName = "comerciomundo"))
        )
      ),
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem("dashboard", "Dashboard tab content"),
          shinydashboard::tabItem("investimentos",
                                  h2("Dados de Investimentos teste"),
                                  fluidRow(
                                    shinydashboard::box(title = "Dados de Estoque",
                                                        echarts4r::echarts4rOutput("plot1", height = 300)),
                                    shinydashboard::box(title = "Dados de Fluxo",
                                                        echarts4r::echarts4rOutput("plot2", height = 300))
                                  )
                                  # fluidRow(
                                  # shinydashboard::box(title = "BNDES",
                                  # echarts4r::echarts4rOutput("plot3", height = 300)),
                                  # shinydashboard::box(title = "BNDES-2",
                                  # echarts4r::echarts4rOutput("plot4", height = 300))
                                  # )
          ),
          shinydashboard::tabItem("comerciobr",
                                  h2("Dados de Comércio Brasil-País"),
                                  downloadButton("mdic_report", "Baixar Relatório em PDF"),
                                  fluidRow(
                                    shinydashboard::box(title = "Fluxo de Comércio", echarts4r::echarts4rOutput("corrente", height = 300)),
                                    shinydashboard::tabBox(title = "Composição",
                                                           tabPanel("exp", echarts4r::echarts4rOutput("fator_exp", height = 300)),
                                                           tabPanel("imp", echarts4r::echarts4rOutput("fator_imp", height = 300))
                                    )
                                  ),
                                  fluidRow(
                                    shinydashboard::tabBox(title = "Produtos",
                                                           tabPanel("exp", echarts4r::echarts4rOutput("produtos_exp", height = 300)),
                                                           tabPanel("imp", echarts4r::echarts4rOutput("produtos_imp", height = 300))
                                    ),
                                    shinydashboard::tabBox(title = "Países",
                                                           tabPanel("exp", echarts4r::echarts4rOutput("paises_exp", height = 300)),
                                                           tabPanel("imp", echarts4r::echarts4rOutput("paises_imp", height = 300))
                                    )
                                  )
          ),
          shinydashboard::tabItem("comerciomundo",
                                  h2("Dados de Comércio País-Mundo"),
                                  downloadButton("comtrade_report", "Baixar Relatório em PDF"),
                                  fluidRow(
                                    shinydashboard::box(title = "Fluxo de Comércio", echarts4r::echarts4rOutput("correntemundo", height = 300)),
                                    shinydashboard::box(title = "Saldos Comerciais", tmap::tmapOutput("saldomundo", height = 400))
                                  ),
                                  fluidRow(
                                    shinydashboard::tabBox(title = "Produtos",
                                                           tabPanel("exp", echarts4r::echarts4rOutput("produtosmundo_exp", height = 300)),
                                                           tabPanel("imp", echarts4r::echarts4rOutput("produtosmundo_imp", height = 400))
                                    ),
                                    shinydashboard::tabBox(title = "Países",
                                                           tabPanel("exp", echarts4r::echarts4rOutput("parceirosmundo_exp", height = 300)),
                                                           tabPanel("imp", echarts4r::echarts4rOutput("parceirosmundo_imp", height = 300))
                                    )
                                  ),
                                  fluidRow(
                                    shinydashboard::tabBox(title = "Destinos e origens dos principais produtos",
                                                           tabPanel("exp", echarts4r::echarts4rOutput("prodpaismundo_exp", height = 500)),
                                                           tabPanel("imp", echarts4r::echarts4rOutput("prodpaismundo_imp", height = 500))
                                    ),
                                    shinydashboard::tabBox(title = "Produtos dos principais países",
                                                           tabPanel("exp", echarts4r::echarts4rOutput("paisprodmundo_exp", height = 500)),
                                                           tabPanel("imp", echarts4r::echarts4rOutput("paisprodmundo_imp", height = 500))
                                    )
                                  )
                                  
          ))
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'subsidios'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
  
}

