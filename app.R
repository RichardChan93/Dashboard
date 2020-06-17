#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(extrafont)
library(wordcloud)

Visual.Resume <- function(left.titles = c("Richard Chan", "Data Scientist", "Bewerbung als Data Analyst im Migros-Genossenschafts-Bund"),
                          right.titles = c("chan.richard@hotmail.com", "+41 78 910 18 68"),
                          year.range = c(2013, 2021), 
                          graph.labels = c("Ausbildung", "Erfahrung"), 
                          data = rbind(c("Universität Zürich", "Bachelor of Arts in Ökonomie", "2013.5", "2016.5", 65, "top", 2013.8, "right",75),
                                       c("Universität Zürich", "Master of Arts in Ökonomie und Data Science", "2016.5", "2019.8", 65, "top", 2016.8, "right",83),
                                       c("Customer Retention Agent", "Coople (Schweiz) AG", "2015.5", "2017", 35, "bottom", 2015.8, "right",29),
                                       c("Application Support Specialist", "Coople (Schweiz) AG", "2017", "2018.8", 35, "bottom", 2017.3, "right",20),
                                       c("Data Scientist", "Coople (Schweiz) AG", "2018.8", "2020", 35, "bottom", 2019.3, "right",13)
                            ),
                          transparency = 1,
                          publications = c("aa",
                                           "bb"), 
                          words = c("R", "Python"), 
                          words.freq = c(10, 20, 5, 5, 5), 
                          links = c("LinkedIn: https://www.linkedin.com/in/richard-chan-879030197/",  "Github: XX"),
                          pallete = "Info.Pal",
                          cloud.size.max = 2,
                          milestones = c("a, 2015", "b, 2016"),
                          font.family = "Helvetica",
                          bottom.titles = c("Interests", "Links")) {
  
  
  
  

  
  top.graph.label <- graph.labels[1]  
  bottom.graph.label <- graph.labels[2] 
  
  left.titles <- unlist(strsplit(left.titles, ","))
  
  left.title <- left.titles[1]
  left.subtitle <- left.titles[2]
  left.subsubtitle <- left.titles[3]
  
  right.titles <- unlist(strsplit(right.titles, ","))
  
  right.title <- right.titles[1]
  right.subtitle <- right.titles[2]
  
  bottom.titles <- unlist(strsplit(bottom.titles, ","))  
  
  
  event.n <- nrow(data)
  event.title <- data[,1]
  event.subtitle <- data[,2]
  start.years <- as.numeric(data[,3])
  end.years <- as.numeric(data[,4])
  poly.heights <- as.numeric(data[,5])
  direction.vec <- data[,6]
  point.starts.x <- as.numeric(data[,7])
  poly.label.direction <- data[,8]
  label.heights <- as.numeric(data[,9])
  
  
  
  ### START FUN
  
  
  
  # ---- Color Palletes
  {
    
    # Espresso.Pal, taken from series of espresso cups in ARC lab
    
    Espresso.Pal <- data.frame(
      "purple" = rgb(39, 27, 48, maxColorValue = 255),
      "blue" = rgb(35, 102, 192, maxColorValue = 255),
      "yellow" = rgb(233, 215, 56, maxColorValue = 255),
      "red" = rgb(185, 18, 38, maxColorValue = 255),
      "green" = rgb(163, 218, 75, maxColorValue = 255),
      "orange" = rgb(255, 100, 53, maxColorValue = 255)
    )
    
    
    iPod.Pal <- data.frame(
      "lightgray" = rgb(215, 215, 215, maxColorValue = 255),
      "red" = rgb(243, 174, 175, maxColorValue = 255),
      "darkgray" = rgb(174, 173, 176, maxColorValue = 255),
      "green" = rgb(158, 217, 191, maxColorValue = 255),
      "blue" = rgb(92, 203, 235, maxColorValue = 255),
      "yellow" = rgb(222, 235, 97, maxColorValue = 255),
      "background" = rgb(242, 242, 242, maxColorValue = 255),
      stringsAsFactors = F
    )
    
    
    Info.Pal <- data.frame(
      "red" = rgb(231, 105, 93, maxColorValue = 255),
      "darkblue" = rgb(107, 137, 147, maxColorValue = 255),
      "creme" = rgb(246, 240, 212, maxColorValue = 255),
      "green" = rgb(149, 206, 138, maxColorValue = 255),
      "lightgray" = rgb(210, 210, 210, maxColorValue = 255),
      "lightblue" = rgb(148, 212, 212, maxColorValue = 255),
      "lightgray" = rgb(150, 150, 150, maxColorValue = 255),
      "background" = rgb(241, 243, 232, maxColorValue = 255),
      "brown" = rgb(136, 119, 95, maxColorValue = 255),
      stringsAsFactors = F
    )
    
    
    Info2.Pal <- data.frame(
      "darkblue" = rgb(0, 106, 64, maxColorValue = 255),
      "pink" = rgb(240, 136, 146, maxColorValue = 255),
      "lightgreen" = rgb(117, 180, 30, maxColorValue = 255),
      "lightgray" = rgb(149, 130, 141, maxColorValue = 255),
      "grayblue" = rgb(112, 140, 152, maxColorValue = 255),
      "lightblue" = rgb(138, 184, 207, maxColorValue = 255),
      "turquoise" = rgb(0, 126, 127, maxColorValue = 255),
      "green" = rgb(53, 131, 89, maxColorValue = 255),
      "paleblue" = rgb(139, 161, 188, maxColorValue = 255),
      "purple" = rgb(90, 88, 149, maxColorValue = 255),
      "orange" = rgb(242, 153, 12, maxColorValue = 255),
      "purple" = rgb(90, 88, 149, maxColorValue = 255),
      "paleorange" = rgb(229, 186, 58, maxColorValue = 255),
      "salmon" = rgb(216, 108, 79, maxColorValue = 255),
      stringsAsFactors = F
    )
    
  }
  # -----------
  
  
  if(pallete %in% c("Espresso.Pal", "iPod.Pal", "Info.Pal", "Info2.Pal")) {color.vec <- as.character(get(pallete))}  
  
  if(length(color.vec) < length(data)) {color.vec <- rep(color.vec, length.out = length(data))}
  
  layout(
    matrix(c(1, 1, 1, 2, 2, 2, 3, 4, 5), nrow = 3, ncol = 3, byrow = T),
    widths = c(2.5, 2.5, 6),
    heights = c(1.5, 5, 2)
  )
  
  
  ## Header
  
  par(mar = c(0, 0, 0, 0))
  
  plot(1, xlim = c(0, 1) ,ylim = c(0, 1), bty = "n", type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "",  family = font.family)
  
  segments(c(.02, .02), c(.8, 0), c(.98, .98), c(.8, 0), lwd = .8, col = "darkgray")
  
  text(.02, .57, left.title, adj = 0, cex = 3)
  text(.02, .3, left.subtitle, adj = 0, cex = 2.5)
  text(.02, .1, left.subsubtitle, cex = 2, adj = 0)
  text(.98, .6, right.title, adj = 1, cex = 3)
  text(.98, .3, right.subtitle, cex = 2.5, adj = 1)
  
  
  ### Body 1
  
  year.seq <- seq(year.range[1], year.range[2], 1)
  year.min <- min(year.seq)
  year.max <- max(year.seq)
  
  
  
  
  par(mai = c(0, 0, 0, 0))
  
  plot(1, xlim = c(year.min, year.max), ylim = c(0, 100), type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "",  family = font.family, bty = "n")
  
  
  ## Background ribbons
  
  rect(seq(year.min, year.max - 1),
       rep(0, year.max - year.min - 1),
       seq(year.min + 1, year.max),
       rep(100, year.max - year.min),
       col = rep(gray(c(.97, .99)), times = floor(year.max - year.min / 2)),
       border = rep(NA, times = floor(year.max - year.min / 2))
  )
  
  
  ## Year labels
  
  segments(year.min, 55, year.max, 55, col = "lightgray")
  segments(year.min, 45, year.max, 45, col = "lightgray")
  
  segments(year.min:year.max, 
           rep(45, length(year.seq)), 
           year.min:year.max, 
           rep(55, length(year.seq)), 
           col = "lightgray"
  )
  
  text(x = (year.min + .5):(year.max - .5), 
       y = rep(50, length(year.seq) - 1), 
       labels = year.seq[-(length(year.seq))],
       cex = 1.8
  )
  
  
  mtext(top.graph.label, 3, cex = 2, adj = .05, line = -3.5)
  mtext(bottom.graph.label, 1, cex = 2, adj = .06, line = -2.7)
  
  
  
  #---------------- Symbol legend
  
  # points(year.min + .3, 87, pch = 22, cex = 3.5, col = "black", bg = "white")
  # text(year.min + .5, 87, symbol.descriptions[1], adj = 0, cex = 1.5)
  # 
  # points(year.min + .3, 80, pch = 23, cex = 3.5, col = "black", bg = "white")
  # text(year.min + .5, 80, symbol.descriptions[2], adj = 0, cex = 1.5)
  
  
  
  #--------------- Plot Polygons
  
  Transparent <- function(orig.col = "red", trans.val = 1, maxColorValue = 255) {
    
    if(length(orig.col) == 1) {orig.col <- col2rgb(orig.col)}
    if(!(length(orig.col) %in% c(1, 3))) {return(paste("length of original color must be 1 or 3!"))}
    
    final.col <- rgb(orig.col[1], orig.col[2], orig.col[3], alpha = trans.val * 255, maxColorValue = maxColorValue)
    
    return(final.col)
  }
  
  for (i in 1:event.n) {
    
    if(substr(direction.vec[i], 1, 1) == "t") {start.pos <- 55}
    if(substr(direction.vec[i], 1, 1) == "b") {start.pos <- 45}
    
    polygon(c(start.years[i], start.years[i], end.years[i], end.years[i]), 
            c(start.pos, poly.heights[i], poly.heights[i], start.pos), 
            col = Transparent(color.vec[i], transparency), border = "black", lwd = .4)
    
  }
  
  #---------------- Add labels
  
  point.starts.y <- rep(NA, event.n)
  point.starts.y[substr(direction.vec, 1, 1) == "t"] <- 60
  point.starts.y[substr(direction.vec, 1, 1) == "b"] <- 40
  
  
  points(point.starts.x, point.starts.y, pch = 21, cex = 3, 
         col = "black", bg = "white")
  
  
  text.adj.vec <- rep(0, event.n)
  text.adj.vec[substr(poly.label.direction, 1, 1) == "l"] <- 1
  
  elbow.vec <- rep(.2, event.n)
  elbow.vec[substr(poly.label.direction, 1, 1) == "l"] <- -.2
  
  segments(c(point.starts.x, point.starts.x),
           c(point.starts.y, label.heights),
           c(point.starts.x, point.starts.x + elbow.vec),
           c(label.heights, label.heights),
           col = gray(.5), lwd = 1, lty = 2)
  
  
  
  for(i in 1:event.n) {
    
    text(point.starts.x[i] + elbow.vec[i], label.heights[i], event.title[i], cex = 1.8, adj = text.adj.vec[i])
    text(point.starts.x[i] + elbow.vec[i], label.heights[i] - 4, event.subtitle[i], cex = 1.4, adj = text.adj.vec[i])
    
  }
  
  # -- Upper milestones
  
  #milestones <- "2005,BA,sub\n2006,MA,subma"
  #milestones <- unlist(strsplit(milestones, "\n"))
  
  milestone.years <- sapply(1:length(milestones), function(x) {
    
    output <- as.numeric(unlist(strsplit(milestones[x], ","))[1])
  })
  milestone.titles <- sapply(1:length(milestones), function(x) {
    
    output <- unlist(strsplit(milestones[x], ","))[2]
    
  })
  milestone.subtitles <- sapply(1:length(milestones), function(x) {
    
    output <- unlist(strsplit(milestones[x], ","))[3]
    
  })
  
  
  
  text(milestone.years, 
       rep(90, length(milestones)), 
       milestone.titles, cex = 2, adj = 1
  )
  
  text(milestone.years, 
       rep(85, length(milestones)),
       milestone.subtitles, 
       cex = 2, adj = 1)
  
  
  # ---- Add publication symbols
  
  
  publications <- unlist(strsplit(publications, "\n"))
  
  
  n.pubs <- length(publications)
  if(n.pubs > 4) {publications <- publications[1:4]}
  
  
  find.year <- function(publication) {
    
    log.vec <- sapply(1900:3000, function(year) {grepl(year, publication)})
    
    year <- (1900:3000)[which(log.vec)]
    
  }
  
  publication.years <- sapply(1:n.pubs, function(x) {find.year(publications[x])})
  
  points(publication.years, rep(60, length(publication.years)), pch = 23, col = "black", bg = "white", cex = 4)
  text(publication.years, rep(60, length(publication.years)), 1:length(publication.years), cex = 1.5)
  
  
  # --- Bottom left graph
  par(mar = c(0, 1, 0, 0))
  if(mean(is.na(words)) != 1) {
    
    wordcloud(words, 
              words.freq, 
              scale = c(1, cloud.size.max), 
              colors = color.vec,
              random.color = T
    )
    
    text(.5, .99, bottom.titles[1], cex = 2, adj = .5)
    
  }
  
  # ---- Bottom middle graph
  
  n.links <- length(links)
  if(length(links) > 4) {links <- links[1:4]}
  
  plot(1, xlim = c(0, 1), ylim = c(0, 1), ylab = "", xlab = "", bty = "n", type = "n", yaxt = "n", xaxt = "n")
  
  text(.5, .99, bottom.titles[2], adj = .5, cex = 2)
  
  text(rep(0, n.links), seq(.8, to = .8 - .2 * (n.links - 1), by = -.2), links, adj = 0, cex = 1.7)
  
  
  
  
}

Visual.Resume()
# Define UI for application that draws a histogram
ui <- shiny::fluidPage(
   
   # Application title
   titlePanel("Dashboard of Richard Chan"),
   
   # Sidebar with a slider input for number of bins 
   sidebarPanel(
     
     # Input: Slider for the number of bins ----
     sliderInput("slider2", label = h3("Year range"), min = 2012, 
                 max = 2022, value = c(2013, 2021))
     
   ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$range <- renderPrint({ input$slider2 })
   
   output$distPlot <- renderPlot({
     Visual.Resume(year.range=c(input$slider2,input$slider2))
   })
}


# Run the application 
shinyApp(ui = ui, server = server)

# Load application to server
#library(rsconnect)

#rsconnect::deployApp('/Users/richardchan/Documents/shiny/Dashboard_RichardChan')
