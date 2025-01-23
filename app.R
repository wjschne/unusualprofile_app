#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Setup ----
library(shiny)
library(tidyverse)
library(unusualprofile)
library(ragg)
library(bslib)
library(scales)
# library(showtext)

library(thematic)
my_font <- "Roboto Condensed"
my_size <- 20

my_theme <- bs_theme(bootswatch = "simplex",
                     base_font = font_google(my_font))

# Options
options(shiny.useragg = TRUE, shiny.usecairo = FALSE,
        scipen = 999)


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
    sig_digits[pgt99] <-
      abs(ceiling(log10(1 - p[pgt99])) - digits + 1)
    sig_digits[ceiling(log10(p)) == log10(p) &
                 (-log10(p) >= digits)] <-
      sig_digits[ceiling(log10(p)) == log10(p) &
                   (-log10(p) >= digits)] - 1
    sig_digits[is.infinite(sig_digits)] <- 0
    l <- purrr::map2_chr(p,
                         sig_digits,
                         formatC,
                         format = "f",
                         flag = "#")
    
  }
  if (remove_leading_zero)
    l <- sub("^-0", "-", sub("^0", "", l))
  
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
      l[round(p, digits = max_digits) == 0] <-
        paste0(".", paste0(rep("0", max_digits), collapse = ""))
      l[round(p, digits = max_digits) == 1] <-
        paste0("1.", paste0(rep("0", max_digits), collapse = ""))
      l[round(p, digits = max_digits) == -1] <-
        paste0("-1.", paste0(rep("0", max_digits), collapse = ""))
    }
  }
  
  l <- sub(pattern = "-",
           replacement = "−",
           x = l)
  Encoding(l) <- "UTF-8"
  
  dim(l) <- dim(p)
  l
}






# UI ----
ui <- fluidPage(theme = my_theme,
                shiny::sidebarLayout(
                  sidebarPanel(
                    titlePanel("UnusualProfile"),
                    h4("Mahalanobis Distances Applied to Conditional Distributions"),
                    shiny::wellPanel(fileInput(inputId = "importdata", 
                                               label = "Import Data", 
                                               accept = ".xlsx", 
                                               placeholder = "unusualprofiletemplate.xlsx"),
                                     downloadButton(outputId = "downloadData", label = "Download Data Template")),
                    shiny::wellPanel(
                    uiOutput("person"),
                    uiOutput("distances"),
                    shiny::fixedRow(checkboxInput("sort", value = FALSE, label = "Sort by Score"),
                                    checkboxInput("color", value = FALSE, label = "Sort by Color"))
                    ),
                    p("Created by ",
                    tags$a(target = "_blank",
                           href = "https://wjschne.github.io/",
                           "W. Joel Schneider"), "to accompany", tags$br(), 
                    "Schneider, W. J., & Ji, F. (2023). ", 
                    tags$a(target = "_blank",
                           href = "https://doi.org/10.1007/s40817-022-00137-x",
                           "Detecting unusual score patterns in the context of relevant predictors."), 
                    " ",
                    tags$em("Journal of Pediatric Neuropsychology, 9,"),
                    " 1–17."
                    ), width = 3

                    
                  ),
                  mainPanel(
                    tabsetPanel(
                      id = "Main",
                      tabPanel("Distance Plot", plotOutput("condplot")),
                      tabPanel("Distance Tables", 
                               h3("Test Statistics"),
                               tableOutput("distance_table"),
                               h3("Distance Statistics"),
                               tableOutput("distance_stats")),
                      tabPanel("Scores", tableOutput("tab_scores")),
                      tabPanel(
                        "Correlations",
                        plotOutput("plot_R")
                      ),
                      tabPanel(
                        "Instructions",
                        h2("Complete Template"),
                        h4("Download Excel Template"),
                        tags$ul(
                          tags$li("Click the “Download Data Template” button at left."),
                          tags$li("Save the file to a location you can easily find again."),
                          tags$li("Open and edit the file in Excel.")
                          ),
                               h4("Test Characteristics"),
                               p("Click the “Tests\" tab. In each row, enter test score names, means, and standard deviations."),
                               img(src = "tests.png", alt = "Test Characteristics", title = "Test Characteristics"), 
                               h4("Test Correlations"),
                               p("Click the \"Correlations\" tab. Enter test correlations in each cell."),
                               img(src = "correlations.png", alt = "Test Correlations", title = "Test Correlations"),
                               h4("Enter Test Scores"),
                               p("Click the “Scores\" tab. In each row, enter a person's test scores."),
                               img(src = "scores.png", alt = "Test Scores", title = "Test Scores"),
                               h4("Create Custom Composite Scores (Optional)"),
                               p("Click the “Make Composites\" tab. In each row, enter the composite name, mean, and standard deviation. For equally weighted composites, enter a 1 into each test's column you wish to include. Blank cells are treated as 0. You can enter any other value for weighted composites, including negative numbers to create difference scores."),
                               img(src = "composites.png", alt = "Test Composites", title = "Test Composites"),
                               h4("Create Distances"),
                               p("Click the “Distances\" table. Enter new distance names in each row. Specify each score's role with numeras follows:"),
                        tags$ul(
                          tags$li("0 (or blank): Omit from distance"),
                          tags$li("1: Dependent/Outcome variable"),
                          tags$li("2: Independent/Predictor variable"),
                          tags$li("3: Composite Predictor. That is, the composite score is a linear combination of 2 or more dependent variables."),
                          ),
                               img(src = "distances.png", alt = "Distances", title = "Distances"),
                        h4("Save and Upload the New File"),
                        tags$ul(tags$li("Save the file, giving it an informative name."),
                                tags$li("Upload file using the \"Browse\" button at left.")),
                        h2("Interpret Distances")
                               )
                    )
                  , width = 9)
                ))

server <- function(input, output) {
  # File ----
  fp <- reactiveVal(value = "unusualprofile_sample.xlsx")

  # Raw Data ----
  R_subtests <- reactive({
      readxl::read_xlsx(path = fp(), sheet = "cormatrix") |> 
      tibble::column_to_rownames("test") |> 
      as.matrix()
    })

  d_subtest <- reactive(readxl::read_xlsx(path = fp(), sheet = "tests"))
  d_subtestscores <- reactive({
      readxl::read_xlsx(path = fp(), sheet = "scores")
    })


  d_Distance <- reactive({
    readxl::read_xlsx(path = fp(), sheet = "distances") |>
      rename(Distance = `Distance Name`) |> 
      mutate(Distance = fct_inorder(Distance)) |> 
      pivot_longer(
        cols = -Distance,
        names_to = "Tests",
        values_to = "Role"
      ) |>
      mutate(
        v_dep = ifelse(Role == 1, Tests, NA_character_),
        v_ind = ifelse(Role == 2, Tests, NA_character_),
        v_ind_composites = ifelse(Role == 3, Tests, NA_character_)
      ) |>
      group_by(Distance) |>
      summarise(
        v_dep = list(v_dep) |> map(\(x) {
          l = x[!is.na(x)]
        }),
        v_ind = list(v_ind) |> map(\(x) {
          l = x[!is.na(x)]
        }),
        v_ind_composites = list(v_ind_composites) |> map(\(x) {
          l = x[!is.na(x)]
        }),
        .groups = "drop"
      )
  })
  
  d_composite <- reactive(readxl::read_xlsx(path = fp(), 
                                            sheet = "composites"))
  
  d_test <- reactive({
    bind_rows(d_subtest(), d_composite() |> 
                rename(Test = "Composite Name") |> 
                select(Test, Mean, SD))
    
  })
  
  # Colors
  d_color <- reactive(readxl::read_xlsx(path = fp(), 
                                        sheet = "colors") |> 
                        rename(Distance = `Distance Name`) |> 
                        mutate(Distance = fct_inorder(Distance)) |> 
                        pivot_longer(
                          cols = -Distance,
                          names_to = "Test",
                          values_to = "Color"))
  
  # Scores ---

  d_compositescore <- reactive(
    d_composite() |>
      rename(
        Composite = `Composite Name`,
        composite_mean = Mean,
        composite_sd = SD
      ) |>
      pivot_longer(
        cols = -c(Composite, composite_mean, composite_sd),
        names_to = "Test",
        values_to = "weight"
      ) |>
      left_join(d_subtest(), by = "Test") |>
      left_join(
        d_subtestscores() |>
          pivot_longer(-`Person ID`,
                       names_to = "Test",
                       values_to = "Score"),
        by = "Test",
        multiple = "all",
        relationship = "many-to-many"
      ) |>
      group_by(`Person ID`, Composite, composite_mean, composite_sd) |>
      nest() |>
      mutate(compositescore = pmap_dbl(
        list(
          d = data,
          composite_mean = composite_mean,
          composite_sd = composite_sd
        ),
        \(d, composite_mean, composite_sd) {
          d <- d |> filter(weight != 0)
          x <- d$Score |> `names<-`(d$Test)
          s <- d$SD |> `names<-`(d$Test)
          w <- d$weight |> `names<-`(d$Test)
          mu <- d$Mean |> `names<-`(d$Test)
          R_all <- R_subtests()[d$Test, d$Test]
          diag_w <- diag(s * w)
          composite_sd * (sum(w * (x - mu)) / sqrt(sum(diag_w %*% R_all %*% diag_w))) + composite_mean
        }
      )) |>
      ungroup()
  )
  
  d_scores <- reactive({
    dc <- d_compositescore() |>
      select(`Person ID`, Composite, compositescore) |>
      pivot_wider(names_from = Composite, values_from = compositescore)
    
    d_subtestscores() |>
      left_join(dc, by = "Person ID")
  })
  
  d_person <- reactive({
    req(input$inputperson)
    filter(d_scores(), `Person ID` == input$inputperson) |>
      pivot_longer(-`Person ID`,
                   values_to = "Score",
                   names_to = "Test") |>
      left_join(d_test(), by = "Test") |>
      select(Test, Score, Mean, SD) |>
      mutate(SS = 15 * (Score - Mean) / SD + 100) 
      
  })
  
  
  
  R <- reactive({
    cw <- d_composite() |> 
      select(-Mean, -SD) |> 
      tibble::column_to_rownames("Composite Name") |> 
      as.matrix() |> 
      t()
    
    sw <- diag(nrow(d_subtest())) |> 
      `dimnames<-`(list(d_subtest()$Test, d_subtest()$Test))
    
    w <- cbind(sw,cw)
    
    cov2cor(t(w) %*% R_subtests() %*% w)
  })
  
  
  
  # Score Table ----
  output$tab_scores <- renderTable({
    if (input$sort) {
      d_person() |> 
        arrange(SS)
    } else {
      d_person()
    }
      
  }, striped = TRUE, hover = TRUE, digits = 0)
  
  # Correlation plot ----
  output$plot_R <- renderPlot({
    as.data.frame(R()) |>
      # `rownames<-`(pull(d_test(), Test)) |>
      tibble::rownames_to_column("test") |>
      # rename_with(\(x) str_wrap(x, width = 10)) |>
      pivot_longer(-test, names_to = "test_x", values_to = "r") |>
      mutate(test = fct_inorder(test) |> fct_rev()) |>
      mutate(test_x = fct_inorder(test_x)) |>
      ggplot(aes(test_x, test)) +
      geom_tile(aes(fill = r)) +
      geom_text(aes(label = prob_label(r)),
                family = my_font,
                size = ggtext_size(my_size)) +
      coord_equal() +
      scale_x_discrete(NULL) +
      scale_y_discrete(NULL) +
      scale_fill_gradient2(
        limits = c(-1, 1),
        low = "firebrick",
        high = "royalblue",
        mid = "white",
        guide = guide_colourbar(title.hjust = 1, title.vjust = 1),
        labels = prob_label
      ) +
      theme(
        axis.text.x = element_text(angle = -90, vjust = .5, hjust = 0),
        legend.position = "top",
        legend.key.width = unit(20, "mm")
      )



  }, height = 800, width = 800)
  
  # ListBox Selectors ----
  output$distances <- renderUI({
    v_distance <- d_Distance() |> pull(Distance)

    shiny::selectInput(
      "inputdistance",
      "Select Distance",
      choices = v_distance,
      size = length(v_distance),
      selectize = FALSE
    )
  })

  output$person <- renderUI({
    v_person <- d_scores() |> pull(`Person ID`)

    shiny::selectInput(
      "inputperson",
      "Select Person",
      choices = v_person,
      size = length(v_person),
      selectize = FALSE
    )
  })

  selectedDistance <- reactiveVal()


  # Observed Events ----
  observeEvent(input$importdata,{
    req(input$importdata)
      fp(input$importdata$datapath)
  })

  observeEvent(input$inputdistance, {
    req(input$inputdistance)
    selectedDistance(input$inputdistance)
  }, ignoreInit = TRUE)

  output$downloadData <- downloadHandler(
    filename = "unusualprofiletemplate.xlsx",
    content = function(file) {
      file.copy("unusualprofiletemplate.xlsx", file)
    }
  )

  # Distances ----
  mycondmaha <- reactive({
    dd <- d_Distance() |> filter(Distance == selectedDistance())


    v_ind_composites <- NULL
    v_ind <- NULL
    v_dep <- NULL

    if (!is.null(dd["v_ind_composites"])) {
      if (length(dd["v_ind_composites"][[1]]) > 0) {
        v_ind_composites <- dd$v_ind_composites[[1]]
      }
      
    }
    if (!is.null(dd["v_ind"])) {
    if (length(dd["v_ind"][[1]]) > 0) {
      v_ind <- dd$v_ind[[1]]
      }}
    
    if (!is.null(dd["v_dep"])) {
    if (length(dd["v_dep"][[1]]) > 0) {
      v_dep <- dd$v_dep[[1]]
    }}
    
    if (!is.null(dd["v_ind"])) {
    if (length(dd["v_ind"][[1]]) == 0) {
      dd$v_ind <- NULL
    }}
    
    if (!is.null(dd["v_ind_composites"])) {
    if (length(dd["v_ind_composites"][[1]]) == 0) {
      dd$v_ind_composites <- NULL
    }}
    
    v_all <- unique(
      c(v_dep, 
        v_ind, 
        v_ind_composites))
    
    d_personscore <- d_person() |> 
      filter(Test %in% v_all) 
    
    mu <- d_personscore |> 
      select(Test, Mean) |> 
      deframe()
    
    sigma <- d_personscore |> 
      select(Test, SD) |> 
      deframe()
    
    d_person_score <- d_person() |> 
      select(Test, Score) |> 
      pivot_wider(names_from = Test, values_from = Score) |> 
      select(all_of(v_all))

    if (length(v_ind_composites) == 0) {
      v_ind_composites <- NULL
    }
    
    if (length(v_ind) == 0) {
      v_ind <- NULL
    }
    if (length(v_dep) == 0) {
      v_dep <- NULL
    }
    
    if (is.null(v_dep)) {
      return(NULL)
    }


    
    
    dcm <- cond_maha(
      data = d_person_score,
      R = R()[v_all, v_all],
      v_dep = v_dep,
      v_ind = v_ind,
      v_ind_composites = v_ind_composites,
      mu = mu[v_all],
      sigma = sigma[v_all]
    ) 
    dcm
  })

# Distance Plot ----
output$condplot <- renderPlot({
  req(input$inputdistance)
  x <- mycondmaha()
  if (is.null(x)) return(NULL)
  
  p_tail <- .05
  score_digits <- ifelse(min(x$sigma) >= 3, 0, 2)
  
  d_mycolors <- d_color() |> 
    filter(Distance == selectedDistance()) |> 
    rename(Variable = Test) 
  
  break_width <- max(x$sigma)
  break_mu <- max(x$mu)
  minor_break_width <- ifelse(break_width %% 3 == 0,
                              1 / 3,
                              1 / 2)
  major_breaks <- seq(-10, 10, 1)
  minor_breaks <- seq(-10, 10, minor_break_width)
  
  if ("maha" %in% class(x)) {

    
    d_stats <-
      tibble::tibble(Variable = x$v_dep,
                     mu = x$mu,
                     sigma = x$sigma) |> 
      left_join(d_mycolors, by = "Variable") |> 
      mutate(Color = ifelse(Color == 0, NA, Color) |> factor(),
             fcol = scales::dscale(Color, scales::viridis_pal(option = "D", begin = 0.2, end = .9))) |> 
      mutate(fcol = ifelse(is.na(Color), "#bbbbbb", fcol)) 
    
    
    
    d <- x$d_dep %>%
      tibble::rownames_to_column("id") %>%
      dplyr::mutate(id = factor(id)) %>%
      tidyr::pivot_longer(-"id",
                          names_to = "Variable",
                          values_to = "Score") %>%
      dplyr::left_join(d_stats, by = "Variable") %>%
      dplyr::mutate(Variable = forcats::fct_inorder(Variable)) %>% 
      dplyr::mutate(
        z = (Score - mu) / sigma,
        z_p = stats::pnorm(z),
        in_tail = z_p < p_tail / 2 |
          z_p > (1 - p_tail / 2)
      )
    
    if (input$sort) {
      if (input$color) {
        d <- d |> 
          group_by(Color) |> 
          arrange(Color, desc(z_p), Variable) |> 
          ungroup() |> 
          mutate(Variable = fct_inorder(Variable))
        
      } else {
        d <- d |> 
          arrange(desc(z_p), Variable) |> 
          mutate(Variable = fct_inorder(Variable))
        
      }
      
      

    } else {
      if (input$color) {
        d <- d |>
          group_by(Color) |>
          arrange(Color) |>
          ungroup() |>
          mutate(Variable = fct_inorder(Variable))
      } else {
        d |>
          mutate(Variable = fct_inorder(Variable))
      }

    }
      ggplot(d, aes(Variable, z)) +
      ggnormalviolin::geom_normalviolin(
        data = d_stats,
        aes(
          x = Variable,
          mu = 0,
          sigma = 1,
          fill = tinter::lighten(fcol, .7),
        ),
        inherit.aes = FALSE,
        p_tail = p_tail
      ) +
      geom_point(mapping = aes(shape = in_tail)) +
      geom_text(
        mapping = aes(
          label = formatC(Score, score_digits, format = "f"),
          color = id
        ),
        vjust = -0.5,
        size = ggtext_size(my_size) * .8,
        family = my_font
      ) +
      geom_text(
        mapping = aes(label = paste0(
          "italic(p)=='",
          proportion_round(z_p),
          "'"
        )),
        vjust = 1.3,
        size = ggtext_size(my_size) * .6,
        parse = TRUE,
        family = my_font
      ) +
      theme_light(base_family = my_font) +
      theme(legend.position = "none",
            axis.text.x = element_text(my_font, size = my_size * .4),
            axis.text.y = element_text(my_font, size = my_size * .4))  +
      scale_color_grey() +
      scale_fill_identity() +
      scale_shape_manual(values = c(16, 0)) +
      scale_y_continuous("Standard Scores",
                         breaks = major_breaks,
                         minor_breaks = minor_breaks, 
                         labels = \(x) signs::signs(x * break_width + break_mu), 
                         sec.axis = sec_axis(name = "Standard Deviations", 
                                             transform = \(x) x, 
                                             breaks = major_breaks, 
                                             labels = signs::signs)) +
        scale_x_discrete(NULL,
                         expand = expansion(add = 1), labels = \(x) str_wrap(x, 10, whitespace_only = FALSE)) +
      labs(title = bquote(list(
        Mahalanobis~Distance == .(formatC(x$dM_dep, 2, format = "f")),
        italic(p) == .(proportion_round(x$dM_dep_p))
      )),
      caption = expression(list(italic(p) == "Population proportion")))
    
    

  } else {

    
    label_independent <- paste0(
      "list(Independent~italic(d[M])==\"",
      formatC(x$dM_ind,
              digits = 2,
              format = "f"),
      "\",italic(p)==\"",
      proportion_round(x$dM_ind_p),
      "\")"
    )
    
    
    label_dependent <- paste0(
      "list(Dependent~italic(d[M])==\"",
      formatC(x$dM_dep,
              digits = 2,
              format = "f"),
      "\",italic(p)==\"",
      proportion_round(x$dM_dep_p),
      "\")"
    )
    
    
    
    
    
    
    
    
    d <- x$d_score %>%
      mutate(
        SD = ifelse(
          test = is.na(zSEE),
          yes = 1,
          no = zSEE * 1
        ),
        z = (Score - mu) / sigma,
        yhat = ifelse(is.na(Predicted), 0, (Predicted - mu) / sigma),
        id = factor(id),
        Role = factor(
          Role,
          levels = c("Independent", "Dependent"),
          labels = c(label_independent, label_dependent)
        )
      ) |> 
      left_join(d_mycolors, by = "Variable") |> 
      mutate(Color = ifelse(Color == 0, NA, Color) |> factor(),
             fcol = scales::dscale(Color, scales::viridis_pal(option = "D", begin = 0.2, end = .9))) |> 
      mutate(fcol = ifelse(is.na(Color), "#bbbbbb", fcol))
    
    if (input$sort) {
      if (input$color) {
        d <- d |> 
          group_by(Color) |> 
          arrange(Color, desc(p), Variable) |> 
          ungroup() |> 
          mutate(Variable = fct_inorder(Variable))
        
      } else {
        d <- d |> 
          arrange(desc(p), Variable) |> 
          mutate(Variable = fct_inorder(Variable))
      }
      

    } else {
      if (input$color) {
        d <- d |>
          group_by(Color) |>
          arrange(Color) |>
          ungroup() |>
          mutate(Variable = fct_inorder(Variable))
      } else {
        d <- d |>
          mutate(Variable = fct_inorder(Variable))
        
      }

    }
    
    
    
    
    
    
    
    d |> 
      ggplot(aes(
        x = Variable,
        y = z
        # fill = Role
      )) +
      facet_grid(
        cols = vars(!!quote(Role)),
        scales = "free",
        space = "free",
        labeller = label_parsed
      ) +
      ggnormalviolin::geom_normalviolin(
        mapping = aes(
          mu = 0,
          sigma = 1,
          fill = tinter::lighten(fcol, .7),
          face_right = Role != label_dependent,
          face_left = TRUE
        ),
        p_tail = p_tail,
        # fill = "gray68",
        width = 0.85
      ) +
      ggnormalviolin::geom_normalviolin(
        mapping = aes(
          mu = yhat,
          sigma = SD,
          fill = tinter::lighten(fcol, .3),
          face_right = Role == label_dependent,
          face_left = FALSE
        ),
        # fill = "gray88",
        width = 0.85,
        p_tail = p_tail
      ) +
      geom_point(mapping = aes(color = id)) +
      geom_text(
        mapping = aes(
          label = formatC(Score, score_digits, format = "f"),
          color = id
        ),
        vjust = -0.5,
        family = my_font
      ) +
      geom_text(
        mapping = aes(
          color = id,
          label = if_else(
            Role == label_independent,
            "",
            paste0("italic(c*p)=='",
                   proportion_round(cp),
                   "'")
          )
        ),
        vjust = 2.3,
        size = 3,
        parse = TRUE,
        family = my_font
      ) +
      geom_text(
        mapping = aes(
          color = id,
          label = paste0("italic(phantom(c)*p)=='",
                         proportion_round(p),
                         "'")
        ),
        vjust = 1.3,
        size = 3,
        parse = TRUE,
        family = my_font
      ) +
      scale_fill_identity() +
      scale_y_continuous("Standard Scores",
                         breaks = major_breaks,
                         minor_breaks = minor_breaks, 
                         labels = \(x) signs::signs(x * break_width + break_mu), 
                         sec.axis = sec_axis(name = "Standard Deviations", 
                                             transform = \(x) x, 
                                             breaks = major_breaks, 
                                             labels = signs::signs)) +
      scale_x_discrete(NULL,
                       expand = expansion(add = 1), labels = \(x) str_wrap(x, 10, whitespace_only = FALSE)) +
      labs(title = bquote(list(
        Conditional ~ Mahalanobis ~ Distance ~ (italic(d[CM])) == .(formatC(
          x$dCM,
          digits = 2,
          format = "f"
        )),
        italic(p) == .(proportion_round(x$dCM_p))
      )),
      # subtitle = bquote(list(
      #   Mahalanobis ~ Distance ~ Reduction == .(paste0(
      #     round(100 * x$distance_reduction),
      #     "%")),
      #   Euclidean ~ Distance ~ Reduction == .(paste0(
      #     round(100 * x$variability_reduction),
      #     "%"))
      # )),
      caption = expression(
        list(
          italic(p) == "Population proportion",
          italic(c * p) == "Conditional proportion",
          italic(d[M]) == "Mahalanobis Distance"
        )
      )) +
      theme_light(base_family = my_font) +
      theme(legend.position = "none",
            strip.text.x = element_text(my_font, size = my_size * .4),
            axis.text.x = element_text(my_font, size = my_size * .4),
            axis.text.y = element_text(my_font, size = my_size * .4)) +
      scale_color_grey()
    
    
  }

  
  
  
  

  


}, height = 700, res = 150)

  # Distance Tables ----
output$distance_table <- renderTable({
  
  x <- mycondmaha()
  
  if ("maha" %in% class(x)) {
    
    # M <- list(
    #   dM_dep = dM_dep,
    #   dM_dep_df = k_dep,
    #   dM_dep_p = dM_dep_p,
    #   d_dep = tibble::as_tibble(d_dep),
    #   v_dep = v_dep,
    #   data = tibble::as_tibble(data),
    #   mu = mu[v_dep, 1],
    #   sigma = sigma[v_dep, 1],
    #   label = label
    # )
    
    d_distance_table <- x$d_dep |> 
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Score") |> 
      mutate(Mean = x$mu,
             SD = x$sigma,
             p = pnorm((Score - Mean) / SD)
             )
    
    if (input$sort) {
      d_distance_table |> 
        arrange(desc(p))
    } else {
      d_distance_table
    }
  } else {
    d_distance_table <- x$d_score |> 
      select(-id) |> 
      rename(Mean = mu, SD = sigma) |> 
      mutate(SEE = SEE * SD) |> 
      select(Role, Variable, Mean, SD, Score, p, cp, Predicted, SEE, R2)
    
    if (input$sort) {
      d_distance_table |> 
        arrange(Role, desc(p))
    } else {
      d_distance_table
    }
  }
  

  
  
}, striped = TRUE, hover = TRUE, na = "")

output$distance_stats <- renderTable({
  x <- mycondmaha()
  
  if ("maha" %in% class(x)) {
    # M <- list(
    #   dM_dep = dM_dep,
    #   dM_dep_df = k_dep,
    #   dM_dep_p = dM_dep_p,
    #   d_dep = tibble::as_tibble(d_dep),
    #   v_dep = v_dep,
    #   data = tibble::as_tibble(data),
    #   mu = mu[v_dep, 1],
    #   sigma = sigma[v_dep, 1],
    #   label = label
    # )
    tibble(Role = "Dependent Only",
           Distance = x$dM_dep,
           df = x$dM_dep_df,
           p = x$dM_dep_p)
    
    
  } else {
    x$d_person |> 
      rename(dM_CM_Distance = dCM,
             dM_CM_df = dCM_df,
             dM_CM_p = dCM_p,
             dM_dep_Distance = dM_dep,
             dM_ind_Distance = dM_ind) |> 
      pivot_longer(-id) |> 
      separate(name, c(NA, "Role", "Stat")) |> 
      pivot_wider(names_from = Stat, values_from = value) |> 
      select(-id) |> 
      mutate(Role = factor(Role, 
                           levels = c("CM", "ind", "dep"), 
                           labels = c("Conditional", "Independent Only", "Dependent Only")))
  }
}, striped = TRUE, hover = TRUE, na = "")

}





theme_set(theme_minimal(base_size = my_size, base_family = my_font))

thematic_shiny(font = font_spec(
  families = my_font,
  install = T,
  update = T
))



shinyApp(ui, server)




