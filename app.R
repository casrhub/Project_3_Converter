library(shiny)
library(igraph)

# Define the UI
ui <- fluidPage(
    titlePanel("Regular Grammar to Finite Automaton Converter"),
    sidebarLayout(
        sidebarPanel(
            textAreaInput("grammar", "Enter Regular Grammar", 
                          rows = 10, cols = 40),
            helpText("Format: X -> Y, separate rules by newlines.")
        ),
        mainPanel(
            plotOutput("automatonPlot")
        )
    )
)

# Define Server Logic
server <- function(input, output) {
    output$automatonPlot <- renderPlot({
        # Process the input to create transitions
        input_text <- input$grammar
        rules <- strsplit(input_text, "\n")[[1]]
        transitions <- lapply(rules, function(x) strsplit(x, "->")[[1]])
        
        # Initialize an empty graph
        g <- make_empty_graph(n = 0, directed = TRUE)
        
        edge_curvatures <- data.frame(from = character(), to = character(), curvature = numeric())

    for (rule in transitions) {
        if (length(rule) == 2) {
            antecedent <- trimws(rule[1])
            consequent <- trimws(rule[2])
            
            symbol <- gsub("[A-Z]", "", consequent)
            state <- gsub("[a-z]", "", consequent)
            
            if (!antecedent %in% V(g)$name) {
                g <- add_vertices(g, 1, name = antecedent)
            }
            
            if (!state %in% V(g)$name && nchar(state) > 0) {
                g <- add_vertices(g, 1, name = state)
            }
            
            if (nchar(state) > 0) {
                g <- add_edges(g, c(antecedent, state))
                E(g)[length(E(g))]$label <- symbol
                
                # Calculate curvature for each new edge
                existing_edge <- which(edge_curvatures$from == antecedent & edge_curvatures$to == state)
                if (length(existing_edge) == 0) {
                    new_curvature <- 0.2  # Start with some curvature
                    edge_curvatures <- rbind(edge_curvatures, data.frame(from = antecedent, to = state, curvature = new_curvature))
                } else {
                    # Increase curvature slightly to separate multiple edges between the same nodes
                    edge_curvatures$curvature[existing_edge] <- edge_curvatures$curvature[existing_edge] + 0.9
                }
                
                # Assign curvature to the edge
                E(g)[length(E(g))]$curved <- edge_curvatures$curvature[which(edge_curvatures$from == antecedent & edge_curvatures$to == state)]
            } else {
                g <- add_vertices(g, 1, name = "Z")
                g <- add_edges(g, c(antecedent, "Z"))
                E(g)[length(E(g))]$label <- symbol
                E(g)[length(E(g))]$curved <- 0.2
            }
        }
    }
        # Color initial and final states
        V(g)$color <- ifelse(V(g)$name == "S", "green", ifelse(grepl("Z", V(g)$name), "red", "gray"))
        
        # Plot the graph
        plot(g, vertex.size = 30, vertex.label.cex = 1.2, vertex.label.color = "black",
             vertex.color = V(g)$color, edge.label = E(g)$label, layout = layout_nicely(g))
    })
}

shinyApp(ui = ui, server = server)