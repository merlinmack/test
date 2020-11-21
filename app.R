#Shiny App test
library(shiny)
library(dplyr)
library(MASS)
library(plotrix)
ui <- fluidPage(titlePanel("Supply and Demand"),
                sidebarLayout(
                  sidebarPanel(
                    #code to calculate window size (this is independent of output display)
                    tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
                    
                    h4("Click the button to start or get new equations"),
                    actionButton("run", "Get Equations"),
                    checkboxInput("inverse", label="See Inverse Equations", value=F),
                    checkboxInput("equil", label="See Equilibrium", value=F),
                    checkboxInput("graph", label="See Graph", value=F),
                    h4("Choose the Limit"),
                    radioButtons("limit", "Limit", c("10"="ten", "100"="hundred", "1000"="thousand"))

                  ),#end sidebarpanel
                  
                  mainPanel(
                    #verbatimTextOutput("dimension_display"),
                    h4("Normal Demand and Supply Equations"),
                    verbatimTextOutput("n.eq"),
                    h4("Inverse Demand and Supply Equations"),
                    verbatimTextOutput("i.eq"),
                    h4("Equilibrium"),
                    verbatimTextOutput("equil"),
                    h4("Graph"),
                    plotOutput("sd.graph"),
                    
                    
                  )#end mainpanel
                )#end sidebarlayout
)#end fluidPage 

server <- function(input, output, session){
  #this will display the window dimensions...I have this turned off in the display for now
  output$dimension_display <- renderText({
    paste(input$dimension[1], input$dimension[2], input$dimension[2]/input$dimension[1])
  })
  
  #set graph size
  g.s <- reactive(
    ifelse(input$dimension[1] < 500, "auto", 500)
  )
  
  #code that generates reactive (when the button is clicked) equation values
  vals <- reactiveValues()
  vals$g.n <- 0

    observeEvent(input$run,{
      #reset the checkboxes
      updatedValue = FALSE
      updateCheckboxInput(session =  session, inputId = "inverse", value = updatedValue)    
      updateCheckboxInput(session =  session, inputId = "equil", value = updatedValue)    
      updateCheckboxInput(session =  session, inputId = "graph", value = updatedValue)    
      
      #get limit value from radio button
      if(input$limit == "ten"){
          vals$limit <- 10
      }else if(input$limit == "hundred"){
          vals$limit <- 100
      }else if (input$limit == "thousand"){
          vals$limit <- 1000
      }
      #build slope matrix
      q <- matrix(0,10,10)
      i <- seq(1,1000,1)
      #turn on the normal equations
      vals$g.n <- 1
      for(a in 1:10){
        #run across columns
        for(b in 1:10){
          q[a,b] <- i[a]/i[b]
        }
      }
      #loop to find whole-number equilibrium
      test=1
      count=1
      while(test==1){
        #set range of graph
        vals$range.1 <- seq(vals$limit/2, vals$limit, 1)
        #get parameter values for inverse demand and supply
        vals$a <- sample(vals$range.1,1)
        vals$b <- sample(q,1)
        vals$c <- sample(0:(vals$limit/2 - 1),1)
        vals$d <- sample(q,1)
        #calculate equilibrium
        vals$q1 <- (vals$a-vals$c)/(vals$d+vals$b)
        vals$p1 <- vals$a-vals$b*vals$q1
        #create rounded equilibrium values to test actual values to
        q.r <- round(vals$q1)
        p.r <- round(vals$p1)
        #test to see if rounded values equal actual values
        test <- ifelse(vals$q1 == q.r & vals$p1 == p.r, 2, 1)
        count=count+1
    }#end loop
  })#end observeevent

  #send output back to UI
  #normal demand and supply
  output$n.eq <- renderText(if(vals$g.n == 1){paste(" Demand: Q = ", fractions(vals$a/vals$b), " - ", fractions(1/vals$b), "P \n",  
                                  "Supply: Q = ", fractions(1/vals$d), "P", " - ", fractions(vals$c/vals$d))})
  #inverse demand and supply
  output$i.eq <- renderText(if(input$inverse == T){paste(" Demand: P = ", fractions(vals$a), " - ", fractions(vals$b), "Q \n",
                                  "Supply: P = ", fractions(vals$c), " + ", fractions(vals$d), "Q")})
  #equilibrium values
  output$equil <- renderText(if(input$equil == T){paste("Q* = ", vals$q1, ", P* = ", vals$p1)})
  
  #create graph figures 
  output$sd.graph <- renderPlot(expr={
    if(input$graph == T){
      #set range of x
      range.x <- ifelse(vals$limit > vals$q1, vals$limit,  round((vals$q1 + .25 * vals$q1)/10)*10)
      #set range of y
      range.y <- ifelse(vals$limit > vals$p1, vals$limit,  round((vals$p1 + .25 * vals$p1)/10)*10)
      #start plot
      plot(0, xlim =c(0,range.x), ylim = c(0,range.y), type="l", ylab="", xlab="", axes=F, las=1)#, asp=.5) 
      #add demand
      ablineclip(vals$a, -vals$b, x1=0, y1=0)
      #add supply
      ablineclip(vals$c, vals$d, x1=0)
      #add axes
      axis(1, pos=0, at=seq(0,range.x,(range.x/10)), cex.axis=.9)
      axis(2, pos=0, at=seq(0,range.y,(range.y/10)), cex.axis=.9)
      #add equilibrium lines
      ablineclip(h=vals$p1, x1=0, x2=vals$q1, lty="dashed", col="grey")
      ablineclip(v=vals$q1, y1=0, y2=vals$p1, lty="dashed", col="grey")
      #set positioning for S and D labels in graph
      x.end <- min(range.x-.1, (range.x-vals$c)/vals$d-.11)
      text(x=x.end, y=(vals$c + vals$d * x.end), labels="S", cex=1, pos=3)
      x.end <- min(range.x-.1, vals$a/vals$b-.1)
      text(x=x.end, y=(vals$a - vals$b * x.end), labels="D", cex=1, pos=3)
      #set axis labels and equilibrium labels on axes
      mtext(text=paste("Q*=", vals$q1), side=1, at=vals$q1, line=1.5)
      mtext(text=paste("P*=", vals$p1), side=2, at=vals$p1, line=1, las=1)
      mtext(text="P", side=2, at=range.y, line=3, las=1, cex=1.4)
      mtext(text="Q", side=1, at=range.x, line=3, las=1, cex=1.4)
      }#end of code that builds the plot
    }, width = g.s, height = g.s
    )#end renderPlot
  
  
}#end server function
shinyApp(ui = ui, server = server)



