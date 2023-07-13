library(shiny)
library(shinydashboard)
library(rworldmap)
library(dplyr)
library(xlsx) #Read xlsx
zomato = read.csv("zomato.csv")
cn <- read.xlsx2("Country-Code.xlsx", sheetIndex = 1, header = TRUE, stringsAsFactors =
                   FALSE)
for (dfindex in 1:nrow(zomato)) {
  i <- 1
  while(zomato$Country.Code[dfindex]!= cn$Country.Code[i]){
    i = i+1 }
  zomato$country.Name[dfindex] <-cn$Country[i]
}
ui <- dashboardPage(dashboardHeader(title="Zomato"),
                    dashboardSidebar(
                      sidebarMenu(menuItem("Location of All Restaurants",tabName = "map"), menuItem("Aggregate
Rating", tabName = "dash1"), menuItem("Price Range", tabName = "dash2"),
                                  menuItem("Excellent Restaurants", tabName = "exe"),
                                  menuItem("Analysis of India", menuSubItem("Restaurant Ratings", tabName = "mapind"),
                                           menuSubItem("Average Cost for 2", tabName = "avg_c"), menuSubItem("Price Range", tabName
                                                                                                             = "price_ind"), menuSubItem("Has Table Booking", tabName = "tb"), menuSubItem("Has Online
Booking", tabName = "ob"), menuSubItem("Text Data Analysis of Most Popular Cuisines",
                                       tabName = "cuis")),
                                  menuItem("About Us", tabName = "abt")
                      )),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "map", fluidRow(box(plotOutput("mapp")),
                                                          box(plotOutput("locbar")))),
                        tabItem(tabName = "dash1", fluidRow(box(plotOutput("histogram")),
                                                            box(plotOutput("bar"))), fluidRow(box(sliderInput("bins","Number of breaks",1,100,50)))),
                        tabItem(tabName = "dash2", fluidRow(box(plotOutput("line")),
                                                            box(plotOutput("bars")), box(plotOutput("boxp")))),
                        tabItem(tabName = "exe", fluidRow(box(plotOutput("exew")))),
                        tabItem(tabName = "abt", fluidRow(textOutput("text"))),
                        tabItem(tabName = "mapind", fluidRow(box(plotOutput("maprevind")))),
                        tabItem(tabName = "invest", fluidRow(box(plotOutput("inv")))),
                        tabItem(tabName = "tb", fluidRow(box(plotOutput("table")))),
                        tabItem(tabName = "ob", fluidRow(box(plotOutput("online")))),
                        tabItem(tabName = "avg_c", fluidRow(box(plotOutput("averagecost")))),
                        tabItem(tabName = "price_ind", fluidRow(box(plotOutput("priceindia")))),
                        tabItem(tabName = "cuis",fluidRow(box(plotOutput("cuisine")))))))
server <- function(input, output) {
  library('rworldxtra')
  output$mapp <- renderPlot({newmap <- getMap(resolution = "high")
  plot(newmap, xlim = range(zomato$Longitude),ylim = range(zomato$Latitude), asp = 1,
       main="Distribution of All the Restaurants")
  points(zomato$Longitude, zomato$Latitude, col = "red", cex = 2)})
  output$locbar <- renderPlot({
    countryfreq<- table(zomato$country.Name)
    colFunc <- colorRampPalette(c("red","blue", "gold"))
    my.cols <- colFunc(length(countryfreq))
    barplot(countryfreq, col=my.cols,
            las = 1,
            horiz = TRUE,
            border = "tan",
            cex.axis = 0.7,
            cex.names = 0.6,
            xlab = "Frequency",
            xlim = c(0, max(countryfreq)),
            main="Restaurant Distribution accross Countries"
    )})
  output$histogram <- renderPlot({hist(zomato$Aggregate.rating, main="Aggregate Rating",
                                       breaks = input$bins)})
  output$bar <- renderPlot({plot(zomato$Aggregate.rating, col="blue", main="Aggregate Rating",
                                 xlab="Aggregate Rating of restaurants", cex=.4)})
  output$line <- renderPlot({plot(zomato$Restaurant.ID,zomato$Price.range,col="red",
                                  main="Price Range", ylab="Price Range of Restaurants")})
  output$boxp <- renderPlot({
    boxplot(zomato$Price.range,
            xlab = "Price Range",
            main = "Distribution of Price Range",
            pch="$")
    abline(h = median(zomato$Price.range), lty = 2, col = "red")
  })
  output$bars <- renderPlot({price_tab<-table(zomato$Price.range)
  barplot(price_tab, main = "Price Range of the Restaurants", ylab="No of Restaurants",
          xlab="Price Range")})
  output$exew <- renderPlot({zomatoE <- filter(zomato,zomato$Rating.text == "Excellent")
  newmap <- getMap(resolution = "low")
  plot(newmap, xlim = range(zomatoE$Longitude),ylim = range(zomatoE$Latitude), asp =
         1,
       main = "Distribution of the Restaurants Rated Excellent")
  points(zomatoE$Longitude, zomatoE$Latitude, col = "orange", cex = 2)})
  output$maprevind <- renderPlot({z2<- filter(zomato,zomato$country.Name=="India")
  library(RColorBrewer)
  pal2 <- colorRampPalette(c("green","red"))
  colg <- pal2(43)
  rating<-aggregate(z2$Aggregate.rating, list(z2$City),mean)
  colnames(rating) <- c("city","rating")
  pr<- aggregate(z2$Price.range,list(z2$City),mean)
  colnames(pr)<- c("city","price.range")
  library(plyr)
  ratprt<- merge(pr,rating, by.x = "city", by.y = "city")
  ratprt <- ratprt[order(ratprt[,3], decreasing = TRUE),]
  x<- barplot(ratprt[,3],names.arg=ratprt[,1], las =2,
              ylab = "Average Rating",
              ylim=c(0,5),
              xlab = "city",
              cex.axis = 0.5,
              cex.names = 0.5,
              main = "Restaurant Ratings across Cities in India",
              bty="n",
              col=colg,)
  lines(x=x, y=ratprt[,2])})
  output$inv <- renderPlot({
    ## Adding Population ###
    fnamep <- paste0("C:/Users/aksha/Documents/R Dataset/population.csv")
    pop <- read.csv(fnamep, stringsAsFactors = FALSE, header = TRUE)
    z2<- zomato[zomato$country.Name=="India",]
    z3 <-merge(pop,z2, by.x= "City", by.y = "City")
    ## Calculating top 5 cities to invest using weights
    z3<-z3[,c("City","Population","Votes","Average.Cost.for.two","Aggregate.rating")]
    z3[,c("Population","Votes","Average.Cost.for.two","Aggregate.rating")]<-
      scale(z3[,c("Population","Votes","Average.Cost.for.two","Aggregate.rating")])
    z3$scores<-
      round(z3$Population*0.2+z3$Votes*0.2+z3$Average.Cost.for.two*0.4+z3$Aggregate.rating*0.4)
    scores<- tapply(z3$scores, list(z3$City), mean)
    scores2<- sort(scores, decreasing = TRUE)
    library(RColorBrewer)
    pal2 <- colorRampPalette(c("green","red"))
    colg <- pal2(43)
    barplot(scores2[1:5],
            las=2,
            ylab = "Scores",
            xlab = "city",
            ylim = c(0,max(scores2)),
            cex.axis = 0.5,
            cex.names = 0.7,
            main = "Top 5 Cities to Invest",
            bty="n",
            col = colg)})
  output$table <- renderPlot({
    z2<- zomato[zomato$country.Name=="India",]
    d1 <- aggregate(z2$Votes, list(z2$Has.Table.booking, z2$City), mean)
    names1 <- d1$Group.2[d1$Group.1=="Yes"]
    z<- d1[d1$Group.2 %in% names1,]
    library(ggplot2)
    ggplot(z, aes(fill=Group.1, y=x, x=Group.2)) +
      geom_bar(position="dodge", stat="identity") +
      theme(axis.text.x = element_text(angle = 90)) +
      ggtitle("Restaurant Ratings across cities") +
      scale_fill_discrete(name = "Has Table Booking")})
  output$online <- renderPlot({
    z2<- zomato[zomato$country.Name=="India",]
    d2 <- aggregate(z2$Votes, list(z2$Has.Online.delivery, z2$City), mean)
    names2 <- d2$Group.2[d2$Group.1=="Yes"]
    z<- d2[d2$Group.2 %in% names2,]
    library(ggplot2)
    ggplot(z, aes(fill=Group.1, y=x, x=Group.2)) +
      geom_bar(position="dodge", stat="identity") +
      theme(axis.text.x = element_text(angle = 90)) +
      ggtitle("Restaurant Ratings across cities") +
      scale_fill_discrete(name = "Has Online Booking")})
  output$averagecost <- renderPlot({
    z2<- zomato[zomato$country.Name=="India",]
    e <- tapply(z2$Average.Cost.for.two, list(z2$City), mean)
    cost<- sort(e,decreasing = TRUE)
    library(RColorBrewer)
    pal <- colorRampPalette(c("red", "green"))
    colred <- pal(43)
    barplot(cost,
            las=2,
            ylab = "Average Cost for 2",
            xlab = "Cities",
            cex.axis = 0.5,
            cex.names = 0.6,
            main = "Average Cost for 2 People across Cities",
            bty="n",
            col = colred)})
  output$priceindia <- renderPlot({
    z2<- zomato[zomato$country.Name=="India",]
    boxplot(z2$Price.range,
            xlab = "Price Range",
            main = "Distribution of Price Range",
            pch="$")})
  output$cuisine <- renderPlot({
    library('tm')
    library('textstem')
    library('qdap')
    z2<- zomato[zomato$country.Name=="India",]
    cF1<- Corpus(VectorSource(z2$Cuisines))
    cF1<- tm_map(cF1,tolower)
    cF1 <- tm_map(cF1, removePunctuation)
    cF1 <- tm_map(cF1, removeWords, stopwords("english"))
    cF1 <- tm_map(cF1, removeNumbers)
    stem_word1 <- lemmatize_words(cF1, dictionary = lexicon::hash_lemmas)
    freq_terms(stem_word1, 20)
    tdm<- TermDocumentMatrix(stem_word1)
    tdmatrix<-as.matrix(tdm)
    colT<- apply(tdmatrix, 2, sum)
    tdmclean<- tdmatrix[,colT>0]
    tdfreq<- rowSums(tdmclean)
    tdfreq<- sort(tdfreq,decreasing = T)
    tdfreq[1:20]
    wordfreq<- data.frame(term=names(tdfreq),num=tdfreq)
    library('wordcloud')
    wordcloud(wordfreq$term, wordfreq$num, max.words=50,
              colors=c("aquamarine","darkgoldenrod","tomato"))})
  output$text<-renderText("\t\t\t\n\n\n Data visualisation Project by group : --> \n\t Sankalp :
19BCE2675 \n\t , Aman Singh : 20BCE2916 \n\t , Atul Koirala : 19BCE2666")
}
shinyApp(ui, server)


