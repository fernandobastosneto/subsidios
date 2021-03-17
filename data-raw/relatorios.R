retirar_lista_paises <- c("Não Definido", "Provisão de Navios e Aeronaves", "Bancos Centrais",
                          "A Designar", "Niue", "Cocos (Keeling), Ilhas", "Marianas do Norte, Ilhas",
                          "Wake, Ilha", "Sint Maarten", "Midway, Ilhas", "Guernsey", 
                          "Madeira, Ilha da", "Território Britânico do Oceano Índico", 
                          "São Pedro e Miquelon", "Svalbard e Jan Mayen", "Terras Austrais Francesas",
                          "Geórgia do Sul e Sandwich do Sul, Ilhas", "Heard e ilhas mcdonald, Ilha",
                          "Organizações Internacionais", "Mayotte", "Guam", "São Bartolomeu",
                          "Lebuan, Ilhas", "Bouvet, Ilha", "Brasil", "Antártica", "Madagascar")

lista_paises <- unique(comerciobr::sh1_df$no_pais)
lista_paises_filtrada <- lista_paises[! lista_paises %in% retirar_lista_paises]


# barao::relatorio_brasil_pais("China")

purrr::walk(lista_paises_filtrada, barao::relatorio_brasil_pais)
# 
# # base de dados comtrade
# 
# ## Identificando os nomes dos arquivos do banco de dados COMTRADE
# 
# paths_arquivos_filtrados <- list.files("input/Comtrade/arquivos_filtrados", pattern = "*.csv", full.names = FALSE)
# 
# # Lendo a base de dados COMTRADE
# 
lista_paises_comtrade <- comerciomundo::dic_comtrade_mdic

# exp_imp_sh2 <- map_df(here("input", "Comtrade", "arquivos_filtrados", paths_arquivos_filtrados), read_csv)
# 
# # Lista de países sem dados suficientes no Comtrade  
# 
retirar_lista_paises_comtrade <- c("Areas, nes", "Chad", "Cuba", "Dem. People's Rep. of Korea", "Dem. Rep. of the Congo",
                                   "Djibouti", "Equatorial Guinea", "Eritrea", "Faeroe Isds", "Free Zones", "Gabon",
                                   "Gibraltar", "Grenada", "Guinea-Bissau",
                                   "Haiti", "Holy See (Vatican City State)", "LAIA, nes", "Liberia", "Libya",
                                   "Marshall Isds", "Mayotte", "Nauru", "Neth. Antilles", "Oceania, nes", "Other Africa, nes",
                                   "Other Asia, nes", "Br. Antarctic Terr.", "Other Europe, nes", "Antarctica",
                                   "North America and Central America, nes", "San Marino", "Sao Tome and Principe", "Somalia",
                                   "Tajikistan", "Turkmenistan", "Tuvalu", "Wallis and Futuna Isds", "Norfolk Isds",
                                   "Anguilla", "Br. Indian Ocean Terr.", "Cocos Isds", "Falkland Isds (Malvinas)", "Niue",
                                   "Saint Helena", "Tokelau", "Christmas Isds", "American Samoa", "Fr. South Antarctic Terr.",
                                   "Guam", "Saint Pierre and Miquelon", "United States Minor Outlying Islands", "South Georgia and the South Sandwich Islands",
                                   "N. Mariana Isd", "Heard Island and McDonald Islands", "Pitcairn", "Special Categories", "Bouvet Island",
                                   "Neutral Zone", "Saint Maarten", "N. Mariana Isds", "Western Sahara", "Bunkers", "Curaçao", "Serbia and Montenegro",
                                   "Bonaire", "South Sudan", "Syria", "Saint Barthélemy", "Br. Virgin Isds", "Cayman Isds", "Fmr Panama-Canal-Zone",
                                   "Czechoslovakia", "Bhutan", "Cook Isds", "Lao People's Dem. Rep.", "Dominica",
                                   "Guadeloupe", "French Guiana", "Fmr Dem. Yemen", "Martinique", "FS Micronesia", "Papua New Guinea",
                                   "Réunion", "Turks and Caicos Isds", "Vanuatu", "Venezuela", "US Virgin Isds")

lista_paises_comtrade_filtrada <- lista_paises_comtrade %>% 
  dplyr::filter(! text %in% retirar_lista_paises_comtrade) %>% 
  dplyr::filter(stringr::str_detect(text, ", nes$", negate = T)) %>% 
  dplyr::pull(text)

purrr::walk(lista_paises_comtrade_filtrada, barao::relatorio_pais_mundo)

