#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 

library(barao)

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
    fluidPage(
      tags$head(
        tags$style(
          HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(30%);
             }"
          )
        )
      ),
      h4("Subsídios Econômicos e Comerciais"),
      tabsetPanel(
        tabPanel("Dados Comerciais",
                 fluidRow(
                   column(2, wellPanel(selectInput("mdic", "Brasil-País", mdic_files), 
                                       downloadButton("mdic_report", "Download"), align = "left"))),
                 fluidRow(
                   column(2, wellPanel(selectInput("comtrade", "País-Mundo", stringr::str_remove(comtrade_files, ".pdf")),
                                       downloadButton("comtrade_report", "Download"), align = "left")))
        ),
        tabPanel("Dados Econômicos",
                 fluidRow(
                   column(2, wellPanel(selectInput("dataset3", "Escolha um país", paises_bancomundial$NO_PAIS),
                                       downloadButton("report3", "Download"), align = "left")))
        )
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

