
# Pacotes -----------------------------------------------------------------

library(sidrar)
library(dplyr)
library(lubridate)
library(ggplot2)
library(GetBCBData)



# Coleta de dados ---------------------------------------------------------

dados_brutos <- sidrar::get_sidra(api = "/t/1737/n1/all/v/2265/p/all/d/v2265%202")

dados_brutos_desemprego <- sidrar::get_sidra(api = "/t/6381/n1/all/v/4099/p/all/d/v4099%201")

dados_brutos_sgs <- GetBCBData::gbcbd_get_series(
  id = 3695,
  first.date = "2000-01-01",
  last.date = Sys.Date()
)


# Tratamentos dos Dados ---------------------------------------------------

dados_tratados <- dados_brutos |> 
  dplyr::mutate(
    data = lubridate::ym(`Mês (Código)`),
    ipca = Valor,
    .keep = "none"
  ) |> 
  dplyr::filter(data >= "2000-01-01") |> 
  dplyr::as_tibble()

dados_tratados_desemprego <- dados_brutos_desemprego |> 
  dplyr::mutate(
    data = lubridate::ym(`Trimestre Móvel (Código)`),
    desemprego = Valor,
    .keep = "none"
  ) |>  
  dplyr::as_tibble()


dados_cruzados <- dplyr::inner_join(
  x = dados_tratados,
  y = dados_tratados_desemprego,
  by = "data"
)


dados_tratados_sgs <- dados_brutos_sgs |> 
  dplyr::mutate(
    data = ref.date,
    cambio = value,
    .keep = "none"
  ) |> 
  dplyr::filter(data >= "2000-01-01") |> 
  dplyr::as_tibble()

dados_cruzados_ipca_sgs <- dplyr::inner_join(
  x = dados_tratados,
  y = dados_tratados_sgs,
  by = "data"
)



# Análise dos Dados -------------------------------------------------------

  #Como a inflação se comportou no periodo?

ggplot2::ggplot(dados_tratados) +
    ggplot2::aes(x = data, y = ipca) +
    ggplot2::geom_line()
  
  
  #Quais são as menores e maiores variações no periodo?

dados_tratados |> 
  dplyr::arrange(ipca) |> 
  dplyr::slice(c(1,nrow(dados_tratados)))
  

  #Qual é o valor médio e como é a distribuição dos seus valores?

summary(dados_tratados$ipca)

ggplot2::ggplot(dados_tratados) +
  ggplot2::aes(x = ipca) +
  ggplot2::geom_histogram()


  #Qual é a relação do IPCA com outras variaveis?

#grafico de distribuição

ggplot2::ggplot(dados_cruzados) + 
    ggplot2::aes(x = desemprego, y = ipca) +
    ggplot2::geom_point()


#grafico de linhas

ggplot2::ggplot(dados_cruzados) +
  ggplot2::geom_line(ggplot2::aes(x = data, y = ipca,
                                  color = "IPCA - 12 meses"),
                     linewidth = 1) +
  ggplot2::geom_line(ggplot2::aes(x = data, y = desemprego,
                                  color = "Taxa de Desemprego"),
                     linewidth = 1)+
  scale_color_manual(values = c("#282f6b", "#b22200"))

 #Modelo de regressão desemprego

modelo <- lm(ipca ~ desemprego, data = dados_cruzados)
summary(modelo)

ggplot2::ggplot(dados_cruzados) + 
  ggplot2::aes(x = desemprego, y = ipca) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm")




  #Buscando dados do Dólar no (sgs/BCB) para comparação

ggplot2::ggplot(dados_cruzados_ipca_sgs) +
  ggplot2::geom_line(ggplot2::aes(x = data, y = ipca,
                                  color = "IPCA - 12 meses"),
                     linewidth = 1) +
  ggplot2::geom_line(ggplot2::aes(x = data, y = cambio,
                                  color = "CAMBIO - 12 meses"),
                     linewidth = 1)+
  scale_color_manual(values = c("#282f6b", "#b22200"))



#Modelo de regressão cambio

modelo <- lm(ipca ~ cambio, data = dados_cruzados_ipca_sgs)
summary(modelo)

ggplot2::ggplot(dados_cruzados_ipca_sgs) + 
  ggplot2::aes(x = cambio, y = ipca) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm")



#Correlações

dados_combinados1 <- merge(dados_tratados, dados_tratados_desemprego, by = "data")
correlacao1 <- cor(dados_combinados$ipca, dados_combinados$desemprego, method = "pearson")
print(correlacao)



dados_combinados2 <- merge(dados_tratados, dados_tratados_sgs, by = "data")
correlacao2 <- cor(dados_combinados2$ipca, dados_combinados2$cambio, method = "pearson")
print(correlacao2)







  