library(shiny)
library(bslib)
library("lubridate")
library(shinycssloaders)
library(future.apply)

create_graph <- function(){
  dfGraph <- read.csv2("https://bergplace.org/share/out.radoslaw_email_email", skip = 2, sep = " ")[, 1:2]
  colnames(dfGraph) <- c("from", "to")
  
  #counting weihts
  cntij_table <- table(dfGraph$from, dfGraph$to)
  cntij <- as.data.frame(as.table(cntij_table))
  colnames(cntij) <- c("from","to","cntij")
  cntij <- cntij[cntij$cntij>0, ]
  
  cnti <- tapply(cntij$cntij, cntij$from, sum)
  cntij$weight <- cntij$cntij / cnti[cntij$from]
  
  library(igraph)
  g <- graph_from_data_frame(cntij, directed=TRUE)
  E(g)$weight <- cntij$weight
  
  g <- simplify(g, remove.multiple=TRUE, remove.loops=TRUE)
  cat("Liczba węzłów:", vcount(g), "\n")
  cat("Liczba krawędzi:", ecount(g), "\n")
  
  neighbor_list <- neighborhood(g, order = 1, nodes = V(g), mode = "out")
  
  return(list(graph = g, neighbors = neighbor_list))
  
}

select_initial <- function(g, method, perc=0.05){
  n_start <- ceiling(vcount(g)*perc)
  
  neighbors_outdegree <- function(g) {
    deg <- degree(g, mode="out")
    sapply(V(g), function(v){
      sum(deg[neighbors(g, v, mode="out")])
    })
  }
  
  if(method == "outdegree"){
    deg <- degree(g, mode="out")
    return(order(deg, decreasing=TRUE)[1:n_start])
  } else if(method == "betweenness"){
    b <- betweenness(g)
    return(order(b, decreasing=TRUE)[1:n_start])
  } else if(method == "closeness"){
    c <- closeness(g)
    return(order(c, decreasing=TRUE)[1:n_start])
  } else if(method == "random"){
    return(sample(vcount(g), n_start))
  } else if(method == "neighbors_outdegree"){ # nodes with best neighbors (with more out connection)
    nout <- neighbors_outdegree(g)
    return(order(nout, decreasing=TRUE)[1:n_start])
  } else{
    stop("Unknown method")
  }
}

run_ic <- function(g, neighbour_list, initial_nodes, iter_max=10, wij_scale=1){
  
  V(g)$activated <- FALSE
  
  V(g)[initial_nodes]$activated <- TRUE
  activated_now <- initial_nodes
  all_activated <- activated_now
  
  edge_weights <- E(g)$weight
  
  iter <- 1
  history <- c(length(activated_now))
  
  while(iter <= iter_max & length(activated_now) > 0){
    new_activated <- c()
    
    for(v in activated_now){
      neighbors <- neighbor_list[[v]]
      for(n in neighbors){
        if(!V(g)[n]$activated){
          eid <- get_edge_ids(g, c(v, n))
          p <- edge_weights[eid] *wij_scale
          p <- min(p, 1)
          if(runif(1) < p){
            new_activated <- c(new_activated, n)
          }
        }
      }
    }
    
    V(g)[new_activated]$activated <- TRUE
    all_activated <- unique(c(all_activated, new_activated))
    activated_now <- unique(new_activated)
    
    history <- c(history, length(new_activated))
    iter <- iter + 1
  }
  
  return(history)
}


# Define UI for app that draws a history of AC in a flat ----
ui <- page_sidebar(
  title = "Symulacja dyfuzji informacji",
  sidebar = sidebar(
    sliderInput("wij", "Skalowanie prawdopodobieństwa aktywacji (%)",
                min=10, max=200, value=100),
    sliderInput("iter", "Maksymalna liczba iteracji",
                min=1, max=50, value=10),
    actionButton("run", "URUCHOM SYMULACJĘ", class="btn-primary")
  ),
  withSpinner(plotOutput("icPlot"), type=4, color="#0d6efd")
)

methods <- c("outdegree", "betweenness", "closeness", "random", "neighbors_outdegree")
set.seed(123)

graph_data <- create_graph()
g <- graph_data$graph
neighbor_list <- graph_data$neighbors


# Define server logic required to draw a plot ----
server <- function(input, output) {

 sim_results <- eventReactive(input$run, {
     set.seed(123)
     plan(multisession) #for parallel calculations
   
    all_histories <- list()
    iters <- input$iter
    pMultiplier <- input$wij/100
    for(m in methods){
      histories <- list()
      cat("Running sim for:",m, "\n")
      histories <- future_lapply(1:100, function(rep){
        initial <- select_initial(g, m)
        run_ic(g, neighbor_list, initial, iter_max=iters, wij_scale=pMultiplier)
      }, future.seed = TRUE)
      all_histories[[m]] <- histories
    }
    
    ##Setting results - it could end on different iteration 
    max_len <- max(unlist(lapply(all_histories, function(h) sapply(h, length))))
    results <- list()
    for(m in methods){
      histories <- all_histories[[m]]
      
      histories_matrix <- sapply(histories, function(x){
        c(x, rep(0, max_len - length(x)))
      })
      
      results[[m]] <- rowMeans(histories_matrix)
    }
    return(results)
  })
 output$icPlot <- renderPlot({  
    req(sim_results())
    
    results <- sim_results()
    results_mat <- do.call(cbind, results)
    n_iter <- nrow(results_mat)
    colors <- c("red", "blue", "green", "orange", "purple")
    matplot(1:n_iter,results_mat, type="l", lty=1, col=colors,
            xlab="Iteracja", ylab="Średnia liczba aktywowanych węzłów",
            main="Proces dyfuzji informacji dla różnych zestawów węzłów początkowych")
    legend("topright", legend=names(results), col=colors, lty=1)
  })
  
}

shinyApp(ui = ui, server = server)