#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  arqs <- fs::dir_ls(here::here("data", "relatorios_comerciobr")) %>% 
    tibble::as_tibble()
  
  arqs_comtrade <- fs::dir_ls(here::here("data", "relatorios_comerciomundo")) %>% 
    tibble::as_tibble()
  
  output$mdic_report <- downloadHandler(
    filename = function() {paste0(input$mdic, " - Comércio Bilateral - ", format(Sys.time(), '%B %Y'), ".pdf")},
    content = function(file) {
      
      id <- showNotification("Criando relatório.. (o processo pode demorar de 1 a 2 minutos)", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      # arquivo <- input$dataset
      arquivo <- arqs %>% 
        dplyr::filter(stringr::str_detect(value, input$mdic)) %>% 
        dplyr::pull(value)
      file.copy(arquivo, file)
    }
  )
  
  
  output$comtrade_report <- downloadHandler(
    filename = function() {paste0(input$comtrade, " - Comércio Total - ", format(Sys.time(), '%B %Y'), ".pdf")},
    content = function(file) {
      
      id <- showNotification("Criando relatório.. (o processo pode demorar de 1 a 2 minutos)", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      # arquivo <- input$dataset
      arquivo <- arqs_comtrade %>% 
        dplyr::filter(stringr::str_detect(value, input$comtrade)) %>% 
        dplyr::pull(value)
      file.copy(arquivo, file)
    }
  )
  
  output$report3 <- downloadHandler(
    filename = function() {paste0(input$dataset3, " - Indicadores Econômicos - ", format(Sys.time(), '%B %Y'), ".pdf")},
    content = function(file) {
      filtro <- input$dataset3
      
      id <- showNotification("Rendering report...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      
      arquivo <- relatorio_bancomundial(filtro)
      file.copy(arquivo, file)
    }
  )
  
  
  
}

