## Importando dados de artistas
subset_unique_artists <- read.csv("~/ufcg/analise-de-dados-2/analise-de-dados-2-p2/subset_unique_artists.csv")
## Importando dados de musicas
songs <- read.csv("~/ufcg/analise-de-dados-2/analise-de-dados-2-p2/songs.csv")
## Removendo dados com anos invalidos
songs <- subset(songs, songs$year > 0)
## Importando dados de termos
artists.terms <- read.csv("~/ufcg/analise-de-dados-2/analise-de-dados-2-p2/artist_term.csv")
## Merge de dados de musicas e termos
songs <- merge(songs,artists.terms,by="artist_id")
## Selecionando os ritmos 
songs <- subset(songs, songs$term == "rock" | songs$term == "jazz" | songs$term == "pop" | songs$term == "electronic" | songs$term == "alternative rock")
## Retirando colunas indesejadas
songs$artist_familiarity <- NULL
songs$artist_hotttnesss <- NULL
songs$duration <- NULL
songs$artist_name <- NULL
songs$artist_id <- NULL
songs$track_id <- NULL
songs$title <- NULL
songs$song_id <- NULL
songs$release <- NULL
songs$artist_mbid <- NULL
## Agrupando por decadas
toDecada <- function(x) {
  output <- x
  for(i in 1:nrow(x)) {
    row <- x[i,]
    if(row$year > 1919 && row$year < 1930) {
      output[i,]$year <- 1920
    } else if(row$year > 1929 && row$year < 1940) {
      output[i,]$year <- 1930
    } else if(row$year > 1939 && row$year < 1950) {
      output[i,]$year <- 1940
    } else if(row$year > 1949 && row$year < 1960) {
      output[i,]$year <- 1950
    } else if(row$year > 1959 && row$year < 1970) {
      output[i,]$year <- 1960
    } else if(row$year > 1969 && row$year < 1980) {
      output[i,]$year <- 1970
    } else if(row$year > 1979 && row$year < 1990) {
      output[i,]$year <- 1980
    } else if(row$year > 1989 && row$year < 2000) {
      output[i,]$year <- 1990
    } else if(row$year > 1999 && row$year < 2010) {
      output[i,]$year <- 2000
    } else if(row$year > 2009) {
      output[i,]$year <- 2010
    }
    print(output[i,]$year)
  }
  return(output)
}
songs <- toDecada(songs)
#Contando a frequencia de musicas por decada e termo
songs <- count(songs, c("term","year"))
#Criando escadas
escala <- c(1,5,10,30,50,100,200,300,400,500,1000,1500)
escala1 <- c(1920,1930,1940,1950,1960,1970,1980,1990,2000,2010)
#plote do grafico
ggplot(data=songs, aes(x=year, y=freq, fill=term)) + 
  geom_bar(colour= "black",stat="identity", position=position_dodge(), size=0.5) + # Thinner lines
  scale_fill_hue(name="Ritmos") +      # Set legend title
  scale_y_log10(breaks = escala) + # Set  y scale
  scale_x_log10(breaks = escala1) + # Set x scale
  xlab("Decada") +
  ylab("Quantidade de músicas") +
  ggtitle("Quantidade de músicas por década") +  # Set title
  theme_bw()