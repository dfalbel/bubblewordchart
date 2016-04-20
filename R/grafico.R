#'  df_bubble_prop
#'  
#'  função que separa os pontos de um círculo de acordo com a proporção
#'  definida pelo parametro p
df_bubble_prop <- function(r, p, cx = 0, cy = 0, n = 100000){
  df <- data.frame(
    x = cx + r*cos(seq(0, 2*pi, length.out = n)),
    y = cy + r*sin(seq(0, 2*pi, length.out = n))
  )
  df$cor <- ifelse(df$x <= cx - r + (1-p)*2*r, "azul", "vermelho")
  return(df)
}

#'  transformar_em_df
#'  
#'  função que une vários círculos em um único dataset.
#'  ela precisa dos raios, proporções e centros dos círculos
transformar_em_df <- function(df){
  l <- lapply(1:nrow(df), function(i){
    df2 <- df_bubble_prop(r = df$raios[i], p = df$props[i], cx = df$cx[i], cy = df$cy[i])
    df2$palavra <- as.character(df$palavra[i])
    df2
  })
  bind_rows(l)
}

#'  escala
#'
#'  função apenas para definir a escala dos raios. Eles serão sempre um
#'  número entre 10 e 110.
escala <- function(x, f = sqrt, minimo = 10, maximo = 100) {
  y <- f(x)
  y <- (y - min(y))/max(y)*maximo + minimo
  return(y)
}

#'  pontos_borda
#'  
#'  função que dado o centro e o raio de um círculo, retorna os pontos que estão
#'  em sua borda.
#'  o n define a quantidade de pontos da borda.
pontos_borda <- function(cx, cy, r, n = 100000){
  dplyr::data_frame(
    x = cx + r*cos(seq(0, 2*pi, length.out = n)),
    y = cy + r*sin(seq(0, 2*pi, length.out = n))
  )
}

#' retirar_pontos_circulo
#' 
#' função que dada uma lista de pontos e um círculo (definido pelo seu centro
#' e raio) retira os pontos da lista que estão dentro deste círculo.
retirar_pontos_circulo <- function(df, cx, cy, r){
  df[(df$x - cx)^2 + (df$y - cy)^2 >= r^2, ]
}

#' menor_distancia
#' 
#' dada uma lista de pontos e um ponto, esta função encontra o ponto da lista
#' que possui a menor distância do ponto dado
menor_distancia <- function(pontos, ponto, px = 1, py = 3){
  pontos$distancia <- px*(pontos$x - ponto[1])^2 + py*(pontos$y - ponto[2])^2
  pontos <- pontos[pontos$distancia == min(pontos$distancia), c(1,2)]
  pontos[1,]
}

#' gerar_centros
#' 
#' função gerada p/ criar os centros das bolhas, de forma que elas não tenham
#' intersecção e que se posicionem de acordo com a proporção ente qt1 e qt2.
gerar_centros <- function(palavra, qt1, qt2, tamanho = 1000, espacamento = 1){
  
  df <- data_frame(
    palavra = palavra,
    qt1 = qt1,
    qt2 = qt2,
    raios = escala(qt1 + qt2),
    props = qt1/(qt1 + qt2),
    props2 = 100*props + ((tamanho - 100)/2)
  ) %>%
    arrange(abs(props - 0.5))
  
  df$cx[1] <- df$props2[1]
  df$cy[1] <- tamanho/2
  
  
  for (i in 2:nrow(df)){
    
    # criar pontos das bordas + espacamento
    pontos <- lapply(1:(i - 1), function(j){
      pontos_borda(df$cx[j], df$cy[j], df$raios[i] + df$raios[j] + espacamento)
    }) %>% bind_rows()
    # retirando pontos que já estão dentro de algum círculo
    for(j in 1:(i-1)){
      pontos <- retirar_pontos_circulo(pontos, df$cx[j], df$cy[j], df$raios[i] + df$raios[j] + espacamento)
    }
    # obtendo o ponto com mínima proximidade do meu centro preferido
    centro <- menor_distancia(pontos, c(df$props2[i], tamanho/2))
    df$cx[i] <- centro$x[1]
    df$cy[i] <- centro$y[1]
  }
  df
}

#' bubblewordchart
#' 
#' @param df data.frame
#' @param palavra nome da coluna do data.frame que contem as palavras
#' @param qt1 nome da coluna do data.frame que contém a frequência da palavra no sentimento 1
#' @param qt2 nome da coluna do data.frame que contém a frequência da palavra no sentimento 2
#' @param cor vetor com nome das cores do gráfico (sentimento 1, sentimento 2)
#' @param transparencia transparencia das cores
#' 
#' @export
bubblewordchart <- function(df, palavra, qt1, qt2, cor = c("blue", "red"), transparencia = 0.4){
  x <- df
  x$palavra <- df[[palavra]]
  x$qt1 <- df[[qt1]]
  x$qt2 <- df[[qt2]]
  x$qtds <- paste(x$qt2, "-", x$qt1)
  df <- gerar_centros(df$palavra, df$qt1, df$qt2)
  aux <- transformar_em_df(df)
  df <- dplyr::left_join(df, x %>% dplyr::select(palavra, qtds), by =  "palavra")
  tema_em_branco <- ggplot2::theme(axis.line=ggplot2::element_blank(),axis.text.x=ggplot2::element_blank(),
                          axis.text.y=ggplot2::element_blank(),axis.ticks=ggplot2::element_blank(),
                          axis.title.x=ggplot2::element_blank(),
                          axis.title.y=ggplot2::element_blank(),legend.position="none",
                          panel.background=ggplot2::element_blank(),panel.border=ggplot2::element_blank(),panel.grid.major=ggplot2::element_blank(),
                          panel.grid.minor=ggplot2::element_blank(),plot.background=ggplot2::element_blank())
  ggplot2::ggplot(aux %>% dplyr::filter(cor == "azul"), ggplot2::aes(x = x, y = y, group = palavra)) +
    ggplot2::geom_polygon(fill = cor[1], alpha = transparencia) +
    ggplot2::geom_polygon(data = aux %>% filter(cor == "vermelho"), fill = cor[2], alpha = transparencia) +
    ggplot2::geom_text(data = df, ggplot2::aes(x = cx, y = cy, label = palavra), vjust = -1) +
    ggplot2::geom_text(data = df, ggplot2::aes(x = cx, y = cy, label = qtds), vjust = 1) +
    tema_em_branco
}