subset_unique_artists <- read.csv("~/ufcg/analise-de-dados-2/analise-de-dados-2-p2/subset_unique_artists.csv")
View(subset_unique_artists)

id.artista.local <- read.csv("~/ufcg/analise-de-dados-2/analise-de-dados-2-p2/id.artista.local.csv")
View(id.artista.local)

songs <- read.csv("~/ufcg/analise-de-dados-2/analise-de-dados-2-p2/songs.csv")
View(songs)

artists.terms <- read.csv("~/ufcg/analise-de-dados-2/analise-de-dados-2-p2/artist_term.csv")
summary(artists.terms) 

summary(songs)

artistas.id.mid <- merge(id.artista.local,subset_unique_artists,by="artist_id")
artistas.id.mid$track_id <- NULL
artistas.id.mid$artist_name.y <- NULL

View(artistas.id.mid)

artistas.pop <- aggregate(. ~ artist_id, data=songs, FUN=mean)

artistas.id.mid.hot <- merge(artistas.pop,artistas.id.mid,by="artist_id")

View(artistas.id.mid.hot)
summary(artistas.id.mid.hot)

artistas.id.mid.hot$title <- NULL
artistas.id.mid.hot$song_id <- NULL
artistas.id.mid.hot$release <- NULL
artistas.id.mid.hot$artist_mbid.x <- NULL
artistas.id.mid.hot$artist_name <- NULL
artistas.id.mid.hot$duration <- NULL
artistas.id.mid.hot$year <- NULL
artistas.id.mid.hot$track_id <- NULL

View(artistas.id.mid.hot)
summary(artistas.id.mid.hot)

artistas.id.mid.hot.term <- merge(artistas.id.mid.hot,artists.terms,by="artist_id")
artistas.interessados <- subset(artistas.id.mid.hot.term, artistas.id.mid.hot.term$term == "rock" | artistas.id.mid.hot.term$term == "jazz" | artistas.id.mid.hot.term$term == "pop" | artistas.id.mid.hot.term$term == "electronic" | artistas.id.mid.hot.term$term == "alternative rock")

View(artistas.interessados)
summary(artistas.interessados)

## dados rock ###
artistas.rock <- subset(artistas.id.mid.hot.term, artistas.id.mid.hot.term$term == "rock")
artistas.rock.pais <- aggregate(. ~ artist_country, data=artistas.rock, FUN=mean)
View(artistas.rock.pais)
artistas.rock.pais$artist_id <- NULL
artistas.rock.pais$artist_mbid.x <- NULL
artistas.rock.pais$artist_name.x <- NULL
artistas.rock.pais$artist_mbid.y <- NULL
artistas.rock.pais$term <- NULL
View(artistas.rock.pais)
artistas.rock.pais <- subset(artistas.rock.pais, artistas.rock.pais$artist_country == "United States" | artistas.rock.pais$artist_country == "United Kingdom" | artistas.rock.pais$artist_country == "Canada" | artistas.rock.pais$artist_country == "France" | artistas.rock.pais$artist_country == "Jamaica")
View(artistas.rock.pais)

## dados jazz ###
artistas.jazz <- subset(artistas.id.mid.hot.term, artistas.id.mid.hot.term$term == "jazz")
artistas.jazz.pais <- aggregate(. ~ artist_country, data=artistas.jazz, FUN=mean)
View(artistas.jazz.pais)
artistas.jazz.pais$artist_id <- NULL
artistas.jazz.pais$artist_mbid.x <- NULL
artistas.jazz.pais$artist_name.x <- NULL
artistas.jazz.pais$artist_mbid.y <- NULL
artistas.jazz.pais$term <- NULL
artistas.jazz.pais <- subset(artistas.jazz.pais, artistas.jazz.pais$artist_country == "United States" | artistas.jazz.pais$artist_country == "United Kingdom" | artistas.jazz.pais$artist_country == "Canada" | artistas.jazz.pais$artist_country == "France" | artistas.jazz.pais$artist_country == "Jamaica")

## dados pop ###
artistas.pop <- subset(artistas.id.mid.hot.term, artistas.id.mid.hot.term$term == "pop")
artistas.pop.pais <- aggregate(. ~ artist_country, data=artistas.pop, FUN=mean)
View(artistas.pop.pais)
artistas.pop.pais$artist_id <- NULL
artistas.pop.pais$artist_mbid.x <- NULL
artistas.pop.pais$artist_name.x <- NULL
artistas.pop.pais$artist_mbid.y <- NULL
artistas.pop.pais$term <- NULL
artistas.pop.pais <- subset(artistas.pop.pais, artistas.pop.pais$artist_country == "United States" | artistas.pop.pais$artist_country == "United Kingdom" | artistas.pop.pais$artist_country == "Canada" | artistas.pop.pais$artist_country == "France" | artistas.pop.pais$artist_country == "Jamaica")

## dados eletronic ###
artistas.electronic <- subset(artistas.id.mid.hot.term, artistas.id.mid.hot.term$term == "electronic")
artistas.electronic.pais <- aggregate(. ~ artist_country, data=artistas.electronic, FUN=mean)
View(artistas.electronic.pais)
artistas.electronic.pais$artist_id <- NULL
artistas.electronic.pais$artist_mbid.x <- NULL
artistas.electronic.pais$artist_name.x <- NULL
artistas.electronic.pais$artist_mbid.y <- NULL
artistas.electronic.pais$term <- NULL
artistas.electronic.pais <- subset(artistas.electronic.pais, artistas.electronic.pais$artist_country == "United States" | artistas.electronic.pais$artist_country == "United Kingdom" | artistas.electronic.pais$artist_country == "Canada" | artistas.electronic.pais$artist_country == "France" | artistas.electronic.pais$artist_country == "Jamaica")

## dados alternative rock ###
artistas.alternative.rock <- subset(artistas.id.mid.hot.term, artistas.id.mid.hot.term$term == "alternative rock")
artistas.alternative.rock.pais <- aggregate(. ~ artist_country, data=artistas.alternative.rock, FUN=mean)
View(artistas.alternative.rock.pais)
artistas.alternative.rock.pais$artist_id <- NULL
artistas.alternative.rock.pais$artist_mbid.x <- NULL
artistas.alternative.rock.pais$artist_name.x <- NULL
artistas.alternative.rock.pais$artist_mbid.y <- NULL
artistas.alternative.rock.pais$term <- NULL
artistas.alternative.rock.pais <- subset(artistas.alternative.rock.pais, artistas.alternative.rock.pais$artist_country == "United States" | artistas.alternative.rock.pais$artist_country == "United Kingdom" | artistas.alternative.rock.pais$artist_country == "Canada" | artistas.alternative.rock.pais$artist_country == "France" | artistas.alternative.rock.pais$artist_country == "Jamaica")
View(artistas.alternative.rock.pais)

## artistas.id.mid.hot.term2 <- aggregate(. ~ artist_id, data=artistas.id.mid.hot.term, FUN=mode)

## View(artistas.id.mid.hot.term2)

library(psych)

spider(y=term,x=artist_country,data=artistas.id.mid.hot.term)

stars(artistas.id.mid.hot.term, full = TRUE, scale = TRUE, radius = TRUE, artistas.id.mid.hot.term$artist_country, nrow = NULL, ncol = NULL)


op <- par(mfrow=c(3,2))
spider(y=1,x=2:9,data=Thurstone,connect=FALSE) #a radar plot
spider(y=1,x=2:9,data=Thurstone) #same plot as a spider plot
spider(y=1:3,x=4:9,data=Thurstone,overlay=TRUE)
#make a somewhat oversized plot
spider(y=26:28,x=1:25,data=cor(bfi,use="pairwise"),fill=TRUE,scale=2) 
par(op)


require(grDevices)
stars(mtcars[, 1:7], key.loc = c(14, 2), main = "Motor Trend Cars : stars(*, full = F)", full = FALSE)

stars(mtcars[, 1:7], key.loc = c(14, 1.5),
      main = "Motor Trend Cars : full stars()", flip.labels = FALSE)

## 'Spider' or 'Radar' plot:
stars(mtcars[, 1:7], locations = c(0, 0), radius = FALSE,
      key.loc = c(0, 0), main = "Motor Trend Cars", lty = 2)

## Segment Diagrams:
palette(rainbow(12, s = 0.6, v = 0.75))
stars(mtcars[, 1:7], len = 0.8, key.loc = c(12, 1.5),
      main = "Motor Trend Cars", draw.segments = TRUE)
stars(mtcars[, 1:7], len = 0.6, key.loc = c(1.5, 0),
      main = "Motor Trend Cars", draw.segments = TRUE,
      frame.plot = TRUE, nrow = 4, cex = .7)


## scale linearly (not affinely) to [0, 1]
USJudge <- apply(USJudgeRatings, 2, function(x) x/max(x))
Jnam <- row.names(USJudgeRatings)
Snam <- abbreviate(substring(Jnam, 1, regexpr("[,.]",Jnam) - 1), 7)
stars(USJudge, labels = Jnam, scale = FALSE,
      key.loc = c(13, 1.5), main = "Judge not ...", len = 0.8)
stars(USJudge, labels = Snam, scale = FALSE,
      key.loc = c(13, 1.5), radius = FALSE)

loc <- stars(USJudge, labels = NULL, scale = FALSE,
             radius = FALSE, frame.plot = TRUE,
             key.loc = c(13, 1.5), main = "Judge not ...", len = 1.2)
text(loc, Snam, col = "blue", cex = 0.8, xpd = TRUE)

## 'Segments':
stars(USJudge, draw.segments = TRUE, scale = FALSE, key.loc = c(13,1.5))

## 'Spider':
stars(USJudgeRatings, locations = c(0, 0), scale = FALSE, radius  =  FALSE,
      col.stars = 1:10, key.loc = c(0, 0), main = "US Judges rated")
## Same as above, but with colored lines instead of filled polygons.
stars(USJudgeRatings, locations = c(0, 0), scale = FALSE, radius  =  FALSE,
      col.lines = 1:10, key.loc = c(0, 0), main = "US Judges rated")
## 'Radar-Segments'
stars(USJudgeRatings[1:10,], locations = 0:1, scale = FALSE,
      draw.segments = TRUE, col.segments = 0, col.stars = 1:10, key.loc =  0:1,
      main = "US Judges 1-10 ")
palette("default")
stars(cbind(1:16, 10*(16:1)), draw.segments = TRUE,
      main = "A Joke -- do *not* use symbols on 2D data!")


webplot = function(data, data.row = NULL, y.cols = NULL, main = NULL, add = F, 
                   col = "red", lty = 1, scale = T) {
  if (!is.matrix(data) & !is.data.frame(data)) 
    stop("Requires matrix or data.frame")
  if (is.null(y.cols)) 
    y.cols = colnames(data)[sapply(data, is.numeric)]
  if (sum(!sapply(data[, y.cols], is.numeric)) > 0) {
    out = paste0("\"", colnames(data)[!sapply(data, is.numeric)], "\"", 
                 collapse = ", ")
    stop(paste0("All y.cols must be numeric\n", out, " are not numeric"))
  }
  if (is.null(data.row)) 
    data.row = 1
  if (is.character(data.row)) 
    if (data.row %in% rownames(data)) {
      data.row = which(rownames(data) == data.row)
    } else {
      stop("Invalid value for data.row:\nMust be a valid rownames(data) or row-index value")
    }
  if (is.null(main)) 
    main = rownames(data)[data.row]
  if (scale == T) {
    data = scale(data[, y.cols])
    data = apply(data, 2, function(x) x/max(abs(x)))
  }
  data = as.data.frame(data)
  n.y = length(y.cols)
  min.rad = 360/n.y
  polar.vals = (90 + seq(0, 360, length.out = n.y + 1)) * pi/180
  
  # 
  if (add == F) {
    plot(0, xlim = c(-2.2, 2.2), ylim = c(-2.2, 2.2), type = "n", axes = F, 
         xlab = "", ylab = "")
    title(main)
    lapply(polar.vals, function(x) lines(c(0, 2 * cos(x)), c(0, 2 * sin(x))))
    lapply(1:n.y, function(x) text(2.15 * cos(polar.vals[x]), 2.15 * sin(polar.vals[x]), 
                                   y.cols[x], cex = 0.8))
    
    lapply(seq(0.5, 2, 0.5), function(x) lines(x * cos(seq(0, 2 * pi, length.out = 100)), 
                                               x * sin(seq(0, 2 * pi, length.out = 100)), lwd = 0.5, lty = 2, col = "gray60"))
    lines(cos(seq(0, 2 * pi, length.out = 100)), sin(seq(0, 2 * pi, length.out = 100)), 
          lwd = 1.2, col = "gray50")
  }
  
  
  r = 1 + data[data.row, y.cols]
  xs = r * cos(polar.vals)
  ys = r * sin(polar.vals)
  xs = c(xs, xs[1])
  ys = c(ys, ys[1])
  
  lines(xs, ys, col = col, lwd = 2, lty = lty)
  
}

familiaridade_pais <- read.csv("~/Documentos/ufcg/analise-de-dados-2/analise-de-dados-2-p2/familiaridade_pais.csv")
popularidade_pais <- read.csv("~/Documentos/ufcg/analise-de-dados-2/analise-de-dados-2-p2/popularidade_pais.csv")

webplot(familiaridade_pais,1, main = "Familiaridade", scale = T)
par(new = T)
webplot(familiaridade_pais,2, col = "blue", main = "", scale = T)
par(new = T)
webplot(familiaridade_pais,3, col = "green", main = "", scale = T)
par(new = T)
webplot(familiaridade_pais,4, col = "yellow", main = "", scale = T)
par(new = T)
webplot(familiaridade_pais,5, col = "orange", main = "", scale = T)
legend("bottomright", lwd = 5, col = c("red", "blue", "green","yellow","orange"), c("Canada","France", "Jamaica", "United Kingdom", "United States"), bty = "n")

webplot(popularidade_pais,1, main = "Popularidade")
par(new = T)
webplot(popularidade_pais,2, col = "blue", main = "")
par(new = T)
webplot(popularidade_pais,3, col = "green", main = "")
par(new = T)
webplot(popularidade_pais,4, col = "yellow", main = "")
par(new = T)
webplot(popularidade_pais,5, col = "orange", main = "")
legend("bottomright", lwd = 5, col = c("red", "blue", "green","yellow","orange"), c("Canada","France", "Jamaica", "United Kingdom", "United States"), bty = "n")

## 'Spider' or 'Radar' plot:
stars(mtcars[, 1:7], locations = c(0, 0), radius = FALSE, key.loc = c(0, 0), main = "Motor Trend Cars", lty = 2)

stars(familiaridade_pais, locations = c(0, 0), col.stars = 1:10 , main = "Familiaridade", key.loc = c(0, 0))

## 'Spider':
stars(USJudgeRatings, locations = c(0, 0), scale = FALSE, radius  =  FALSE, col.stars = 1:10, key.loc = c(0, 0), main = "US Judges rated")

## Same as above, but with colored lines instead of filled polygons.
stars(USJudgeRatings, locations = c(0, 0), scale = FALSE, radius  =  FALSE, col.lines = 1:10, key.loc = c(0, 0), main = "US Judges rated")


View(Thurstone)
spider(y=1,x=2:9,data=Thurstone,connect=FALSE) #a radar plot
spider(y=1,x=2:9,data=Thurstone) #same plot as a spider plot
spider(y=26:28,x=1:25,data=cor(bfi,use="pairwise"),fill=TRUE,scale=2) 

stars(aa[, 1:7], key.loc = c(14, 2), main = "Motor Trend Cars : stars(*, full = F)", full = FALSE)

## Segment Diagrams:
require(grDevices)
palette(rainbow(12, s = 0.6, v = 0.75))
stars(familiaridade_pais[, 2:6], key.loc = c(5, 2),len = 1, main = "Familiaridade", draw.segments = TRUE, full =FALSE, labels = c("Canada","France","Jamaica","United Kingdom","United States"), scale = TRUE)


stars(popularidade_pais[, 2:6], key.loc = c(5, 2), main = "Popularidade", draw.segments = TRUE, full = FALSE, labels = NULL, scale = TRUE)

View(mtcars)

require(grDevices)
stars(mtcars[, 1:7], key.loc = c(14, 2), main = "Motor Trend Cars : stars(*, full = F)", full = FALSE)
stars(mtcars[, 1:7], key.loc = c(14, 1.5),
      main = "Motor Trend Cars : full stars()", flip.labels = FALSE)

library("psych")

summary(artistas.id.mid.hot.term)
write.csv(artistas.id.mid.hot.term, file = "~/Documentos/ufcg/analise-de-dados-2/analise-de-dados-2-p2/artistas.id.mid.hot.term.csv")

as.data.frame(table(numbers))
  				
terms <- artistas.id.mid.hot.term
terms$artist_id <- NULL
terms$artist_familiarity <- NULL
terms$artist_hotttnesss <- NULL
terms$artist_name.x <- NULL
terms$artist_country <- NULL
terms$artist_mbid.y <- NULL

terms.count <- as.data.frame(sort(table(terms),decreasing= TRUE))

artistas.id.mid.hot.term <- subset

artistas.id.mid.hot.term2 <- subset(artistas.id.mid.hot.term, artistas.id.mid.hot.term$term == "rock" | artistas.id.mid.hot.term$term == "jazz" | artistas.id.mid.hot.term$term == "pop" | artistas.id.mid.hot.term$term == "electronic")

"rock" | "pop" | "electronic" | "jazz" | "united states" | "alternative rock"

library(fmsb)
radarchart(familiaridade_pais, axistype=2, maxmin = FALSE)
aaa <- familiaridade_pais
aaa$row.names <- NULL

install.packages("ggplot2")
library(ggplot2)
p <- ggplot(mtcars, aes( x = factor(gear)))
p + 
  geom_bar(width = 1, colour = "black") + 
  coord_polar(theta = "x")

library(ggplot2)

# A bar graph
ggplot(data=df1, aes(x=time, y=total_bill, fill=sex)) + 
  geom_bar(colour="black", stat="identity",
           position=position_dodge(),
           size=.3) +                        # Thinner lines
  scale_fill_hue(name="Sex of payer") +      # Set legend title
  xlab("Time of day") + ylab("Total bill") + # Set axis labels
  ggtitle("Average bill for 2 people") +  # Set title
  theme_bw()

df1 <- data.frame(sex       = factor(c("Female","Female","Male","Male","Male","Female")),
                  time       = factor(c("Lunch","Dinner","Lunch","Dinner","Tarde", "Tarde"), levels=c("Lunch","Dinner")),
                  total_bill = c(13.53, 16.81, 16.24, 17.42,15.57,18.2))
