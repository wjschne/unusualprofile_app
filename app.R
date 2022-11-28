#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(unusualprofile)
library(ragg)
library(bslib)
library(showtext)

library(thematic)
my_font <- "Roboto Condensed"
my_size <- 20

my_theme <- bs_theme(bootswatch = "simplex",
                     base_font = font_google(my_font))

# Options
options(shiny.usecairo = T, # use Cairo device for better antialiasing
        scipen = 999 # Do not use scientific notation
)


ggtext_size <- function(base_size, ratio = 0.8) {
    ratio * base_size / 2.845276
}


prob_label <- function(p,
                       accuracy = 0.01,
                       digits = NULL,
                       max_digits = NULL,
                       remove_leading_zero = TRUE,
                       round_zero_one = TRUE) {
    if (is.null(digits)) {
        l <- scales::number(p, accuracy = accuracy)
    } else {
        sig_digits <- abs(ceiling(log10(p + p / 1000000000)) - digits)
        pgt99 <- p > 0.99
        sig_digits[pgt99] <- abs(ceiling(log10(1 - p[pgt99])) - digits + 1)
        sig_digits[ceiling(log10(p)) == log10(p) & (-log10(p) >= digits)] <- sig_digits[ceiling(log10(p)) == log10(p) & (-log10(p) >= digits)] - 1
        sig_digits[is.infinite(sig_digits)] <- 0
        l <- purrr::map2_chr(p,
                             sig_digits,
                             formatC,
                             format = "f",
                             flag = "#")
        
    }
    if (remove_leading_zero) l <- sub("^-0","-", sub("^0","", l))
    
    if (round_zero_one) {
        l[p == 0] <- "0"
        l[p == 1] <- "1"
        l[p == -1] <- "-1"
    }
    
    if (!is.null(max_digits)) {
        if (round_zero_one) {
            l[round(p, digits = max_digits) == 0] <- "0"
            l[round(p, digits = max_digits) == 1] <- "1"
            l[round(p, digits = max_digits) == -1] <- "-1"
        } else {
            l[round(p, digits = max_digits) == 0] <- paste0(".", paste0(rep("0", max_digits), collapse = ""))
            l[round(p, digits = max_digits) == 1] <- paste0("1.", paste0(rep("0", max_digits), collapse = ""))
            l[round(p, digits = max_digits) == -1] <- paste0("-1.", paste0(rep("0", max_digits), collapse = ""))
        }
    }
    
    l <- sub(pattern = "-", replacement = "−", x = l)
    Encoding(l) <- "UTF-8"
    
    dim(l) <- dim(p)
    l
}






# UI ----
ui <- fluidPage(
    theme = my_theme,
    shiny::sidebarLayout(
sidebarPanel(titlePanel("UnusualProfile"),
             h3("Mahalanobis Distances Applied to Conditional Distributions"),
             downloadButton("downloadData", "Download Data Template"),
             fileInput("datainput", "Import Data", accept = ".xlsx"),
             uiOutput("distances")),    
    mainPanel(
        h1("This app works, but is in a preliminary state. More functions to come."),
        tabsetPanel(id = "Main",
            tabPanel("Paste Data",  
                     textAreaInput("caption","Paste Data from Excel Here",
                                                  "
Tests	Scores	Means	SDs	Distance Immediate predicts Delayed	Distance Visual predicts Verbal	Immediate Verbal Memory	Delayed Verbal Memory	Immediate Visual Memory	Delayed Visual Memory
Immediate Verbal Memory	120	100	15	2	1				
Delayed Verbal Memory	120	100	15	1	1	0.8			
Immediate Visual Memory	80	100	15	2	2	0.7	0.6		
Delayed Visual Memory	80	100	15	1	2	0.6	0.7	0.8
",

width = "1000px", height = "400px")),
tabPanel("View Data",
         h2("Scores"),
         tableOutput("tab_scores"),
         h2("Correlations"),
         plotOutput("plot_R")
        ),
tabPanel("Distances",
         plotOutput("customdistances")

         
         )))))

server <- function(input, output) {
    d <- reactive(readr::read_tsv(input$caption))
    v_tests <- reactive(pull(d(),Tests))
    d_scores <- reactive({
        d() |> select(Tests, Scores) |> 
            pivot_wider(names_from = Tests, values_from = Scores)
    })
    R <- reactive({
        R <- d() |> select( all_of(v_tests())) |> as.matrix()
        R[is.na(R)] <- 0
        R <- R + t(R)
        diag(R) <- 1
        dimnames(R) <- list(v_tests(), v_tests())
        R
        
    })
    
    d_Distance <- reactive({
        d() |>
            select(Tests, contains("Distance")) |>
            pivot_longer(-Tests, values_to = "Role", 
                         names_to = "Distance", 
                         names_prefix = "Distance ") |>
            mutate(v_dep = ifelse(Role == 1, Tests, NA_character_),
                   v_ind = ifelse(Role == 2, Tests, NA_character_),
                   v_ind_composite = ifelse(Role == 3, Tests, NA_character_)) |>
            group_by(Distance) |>
            summarise(v_dep = list(v_dep) |> map(\(x) {l = x[!is.na(x)]}),
                      v_ind = list(v_ind) |> map(\(x) {l = x[!is.na(x)]}),
                      v_ind_composite = list(v_ind_composite) |> map(\(x) {l = x[!is.na(x)]}), .groups = "drop")
        
    })
        
    output$tab_scores <- renderTable({
        d() |> 
            select(Tests, Scores, Means, SDs) |>
            mutate(SS = 15 * (Scores - Means) / SDs + 100)
    })
    output$plot_R <- renderPlot({

        as.data.frame(R()) |> 
            `rownames<-`(v_tests()) |> 
            tibble::rownames_to_column("Tests") |> 
            # rename_with(\(x) str_wrap(x, width = 10)) |> 
            pivot_longer(-Tests, names_to = "test_x", values_to = "r") |> 
            mutate(Tests = fct_inorder(Tests) |> fct_rev()) |> 
            mutate(test_x = fct_inorder(test_x)) |> 
            ggplot(aes(test_x, Tests)) +
            geom_tile(aes(fill = r)) + 
            geom_text(aes(label = prob_label(r)), family = my_font, size = ggtext_size(my_size)) +
            coord_equal() + 
            scale_x_discrete(NULL) + 
            scale_y_discrete(NULL) + 
            scale_fill_gradient2(limits = c(-1,1), low = "firebrick", high = "royalblue", mid = "white", guide = guide_colourbar(title.hjust = 1, title.vjust = 1), labels = prob_label) +
            theme(axis.text.x = element_text(angle = -90, vjust = .5), 
                  legend.position = "top", legend.key.width = unit(20, "mm"))
        
        
        
    }, height = 800, width = 800)
    
    output$distances <- renderUI({
        v_distance <- d_Distance() |> pull(Distance)
        
        shiny::selectInput("inputdistance", "Select Distance", choices = v_distance, size = length(v_distance), selectize = FALSE)
    })
    
    selectedDistance <- reactiveVal()
    
    observeEvent(input$inputdistance, {
        # req(input$inputdistance, input$inputdistance != "NA")
        selectedDistance(input$inputdistance)
        updateTabsetPanel(inputId = "Main", selected = "Distances")
    }, ignoreInit = TRUE)
    
    output$downloadData <- downloadHandler(
        filename = "unusualprofiletemplate.xlsx",
        content = function(file) {
            file.copy("unusualprofiletemplate.xlsx", file)
        }
    )

    
    output$customdistances <- renderPlot({
        # print(selectedDistance())

        dd <-  d_Distance() |> filter(Distance == selectedDistance())
        v_all <- c(dd$v_dep[[1]],dd$v_ind[[1]], dd$v_ind_composite[[1]])
        
        if (length(dd$v_ind[[1]]) == 0) dd$v_ind <- NULL
        if (length(dd$v_ind_composite[[1]]) == 0) dd$v_ind_composite <- NULL
        
        
        # print(dd$v_ind[[1]])
        # print(dd$v_dep[[1]])
        cond_maha(
            data = d_scores(),
            R = R()[v_all, v_all],
            v_dep = dd$v_dep[[1]],
            v_ind = dd$v_ind[[1]],
            v_ind_composites = dd$v_ind_composite[[1]],
            mu = d() |> select(Tests, Means) |> filter(Tests %in% v_all) |> deframe(),
            sigma = d() |> select(Tests, SDs) |> filter(Tests %in% v_all) |> deframe()
        ) |> 
            plot(family = my_font) +
            theme(strip.text.x = element_text(my_font, size = my_size * .4),
                  axis.text.x = element_text(my_font, size = my_size * .4),
                  axis.text.y = element_text(my_font, size = my_size * .4),
                  plot.subtitle = element_blank()) +
            scale_x_discrete(NULL, labels = \(x) str_wrap(x, 10))
    }, height = 700, res = 150)
    

        
    
    
    
    
}







ggplot2::theme_set(ggplot2::theme_minimal(base_size = my_size, base_family = my_font))

thematic_shiny(font = font_spec(families = my_font, install = T, update = T))



shinyApp(ui, server)

# d <- "
# Tests	Scores	Means	SDs	Distance 1	Distance 2	Immediate Verbal Memory	Delayed Verbal Memory	Immediate Visual Memory	Delayed Visual Memory
# Immediate Verbal Memory	120	100	15	2	1				
# Delayed Verbal Memory	120	100	15	1	1	0.8			
# Immediate Visual Memory	80	100	15	2	2	0.7	0.6		
# Delayed Visual Memory	80	100	15	1	2	0.6	0.7	0.8
# " |> 
#     read_tsv()
# 
# 
# 
# 
# 
# mu <- d |> pull(Means)
# sigma <- d |> pull(SDs)
# 


                     