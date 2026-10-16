#PLOT FUNCTIONS

#*******************************
#* PLOT FORECAST FUNCTIONS
#*******************************

#PLOT

library(ggplot2); library(dplyr); library(scales)

PLOT_WEEKLY_CASES <- function(
    data_mpox,
    list_jur,
    title_plot = "Mpox Weekly Reported Cases. January 2023 - November 2024",
    n_col_plot = 2,
    n_row_plot = 3,
    two_figures = TRUE,
    n_per_figure = n_col_plot * n_row_plot,   # 6 per page by default
    point_size = 1.6,
    line_size = 0.8,
    break_every = 8                           # label every 8th week (was 4)
) {
  
  library(dplyr)
  library(ggplot2)
  library(ggh4x)
  
  jur_labels <- GET_JUR_LABELS()
  
  df_plot <- data_mpox %>%
    filter(Jurisdiction %in% list_jur) %>%
    mutate(
      date_week_start   = as.Date(date_week_start),
      Jurisdiction_orig = Jurisdiction,
      Jurisdiction      = factor(jur_labels[Jurisdiction],
                                 levels = jur_labels[list_jur])
    ) %>%
    arrange(Jurisdiction, date_week_start)
  
  #-----------------------------
  # X AXIS (shared across all pages)
  #-----------------------------
  date_seq <- df_plot %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  
  idx <- seq(1, length(date_seq), by = break_every)
  x_breaks <- date_seq[idx]
  
  # Add the final date only if it won't crowd the previous label
  if (length(date_seq) - tail(idx, 1) >= break_every / 2) {
    x_breaks <- c(x_breaks, tail(date_seq, 1))
  }
  
  #-----------------------------
  # PLOT FUNCTION
  #-----------------------------
  plot_subset <- function(subset_jur, page_title) {
    
    d <- df_plot %>% filter(Jurisdiction_orig %in% subset_jur)
    d$Jurisdiction <- droplevels(d$Jurisdiction)
    
    ggplot(d, aes(x = date_week_start, y = Cases)) +
      
      geom_line(colour = "black", linewidth = line_size) +
      geom_point(colour = "black", shape = 16, size = point_size) +
      
      scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
      
      facet_wrap2(~Jurisdiction, scales = "free_y",
                  axes = "all", ncol = n_col_plot, nrow = n_row_plot) +
      
      labs(x = "Date (week start date of reported cases)",
           y = "Case counts",
           title = page_title) +
      
      theme_minimal(base_size = 17) +
      
      theme(
        plot.title       = element_text(size = 22, hjust = 0.5),
        axis.text.x      = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y      = element_text(size = 12),
        axis.title.x     = element_text(size = 18),
        axis.title.y     = element_text(size = 18),
        strip.text       = element_text(size = 17),
        panel.grid.minor = element_blank(),
        panel.spacing    = unit(1, "lines")
      )
  }
  
  #-----------------------------
  # PLOT (printed inside the function)
  #-----------------------------
  if (!two_figures) {
    print(plot_subset(list_jur, title_plot))
  } else {
    jur_chunks <- split(list_jur,
                        ceiling(seq_along(list_jur) / n_per_figure))
    
    plots <- lapply(jur_chunks, function(ch) plot_subset(ch, title_plot))
    
    invisible(lapply(plots, print))
  }
  
  invisible(NULL)
}

PLOT_WEEKLY_CASES_ONE_FIGURE <- function(
    data_mpox,
    list_jur,
    title_plot = "Mpox Weekly Reported Cases. January 2023 - November 2024",
    n_col_plot = 2,
    two_figures = FALSE,
    n_per_figure = 4,
    point_size = 1.6,
    line_size = 0.8,
    break_every = 4
) {
  
  jur_labels <- GET_JUR_LABELS()
  
  df_plot <- data_mpox %>%
    filter(Jurisdiction %in% list_jur) %>%
    mutate(
      date_week_start   = as.Date(date_week_start),
      Jurisdiction_orig = Jurisdiction,
      Jurisdiction      = factor(jur_labels[Jurisdiction],
                                 levels = jur_labels[list_jur])
    ) %>%
    arrange(Jurisdiction, date_week_start)
  
  #-----------------------------
  # X AXIS
  #-----------------------------
  date_seq <- df_plot %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  
  x_breaks <- date_seq[seq(1, length(date_seq), by = break_every)]
  if (tail(date_seq, 1) != tail(x_breaks, 1)) {
    x_breaks <- c(x_breaks, tail(date_seq, 1))
  }
  
  #-----------------------------
  # PLOT FUNCTION
  #-----------------------------
  plot_subset <- function(subset_jur) {
    
    d <- df_plot %>% filter(Jurisdiction_orig %in% subset_jur)
    
    ggplot(d, aes(x = date_week_start, y = Cases)) +
      
      geom_line(colour = "black", linewidth = line_size) +
      geom_point(colour = "black", shape = 16, size = point_size) +
      
      scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
      
      facet_wrap2(~Jurisdiction, scales = "free_y",
                  axes = "all", ncol = n_col_plot) +
      
      labs(x = "Date (week start date of reported cases)",
           y = "Case counts",
           title = title_plot) +
      
      theme_minimal(base_size = 17) +
      
      theme(
        plot.title       = element_text(size = 22, hjust = 0.5),
        axis.text.x      = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y      = element_text(size = 12),
        axis.title.x     = element_text(size = 18),
        axis.title.y     = element_text(size = 18),
        strip.text       = element_text(size = 17),
        panel.grid.minor = element_blank(),
        panel.spacing    = unit(1, "lines")
      )
  }
  
  #-----------------------------
  # RETURN PLOTS
  #-----------------------------
  if (!two_figures) {
    return(plot_subset(list_jur))
  } else {
    jur_chunks <- split(list_jur,
                        ceiling(seq_along(list_jur) / n_per_figure))
    plot_list <- lapply(jur_chunks, function(ch) print(plot_subset(ch)))
    invisible(plot_list)
  }
}
#*******************************
#* PLOT FORECAST FUNCTIONS
#*******************************


PLOT_FORECASTS_SINGLE_JURISDICITON <- function(
    df_preds_var,
    df_preds_ar,
    df_preds_arima,
    df_preds_nb,
    df_preds_naive,
    title_plot,
    point_size = 4.6,
    line_size = 1.6
) {
  
  #-----------------------------
  # FORMAT DATA
  #-----------------------------
  
  df_var <- df_preds_var %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "VAR-Lasso"
    ) %>%
    dplyr::select(Week_Number, date_week_start, Predicted, Actual, Method)
  
  df_ar <- df_preds_ar %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "AR-Lasso"
    ) %>%
    dplyr::select(Week_Number, date_week_start, Predicted, Actual, Method)
  
  df_arima <- df_preds_arima %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "ARIMA"
    ) %>%
    dplyr::select(Week_Number, date_week_start, Predicted, Actual, Method)
  
  df_nb <- df_preds_nb %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "Negative-Binomial-AR"
    ) %>%
    dplyr::select(Week_Number, date_week_start, Predicted, Actual, Method)
  
  df_naive <- df_preds_naive %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "Naive"
    ) %>%
    dplyr::select(Week_Number, date_week_start, Predicted, Actual, Method)
  
  
  #-----------------------------
  # COMBINE
  #-----------------------------
  
  df_all <- bind_rows(
    df_var,
    df_ar,
    df_arima,
    df_nb,
    df_naive
  )
  
  
  df_long <- df_all %>%
    pivot_longer(
      cols = c(Predicted, Actual),
      names_to = "Source",
      values_to = "Cases"
    ) %>%
    mutate(
      Legend = case_when(
        Source == "Actual" ~ "Reported Cases",
        TRUE ~ Method
      ),
      
      Legend = factor(
        Legend,
        levels = c(
          "Reported Cases",
          "VAR-Lasso",
          "AR-Lasso",
          "ARIMA",
          "Negative-Binomial-AR",
          "Naive"
        )
      )
    )
  
  
  #-----------------------------
  # X AXIS
  #-----------------------------
  
  date_seq <- df_long %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  
  if (tail(date_seq,1) != tail(x_breaks,1))
    x_breaks <- c(x_breaks, tail(date_seq,1))
  
  
  #-----------------------------
  # PLOT
  #-----------------------------
  
  ggplot(df_long,
         aes(
           x = date_week_start,
           y = Cases,
           colour = Legend
         )) +
    
    geom_line(
      aes(
        linetype = Legend,
        linewidth = Legend
      )
    ) +
    
    geom_point(
      aes(shape = Legend),
      size = point_size
    ) +
    
    scale_color_manual(
      name = "",
      values = c(
        "Reported Cases" = "black",
        "VAR-Lasso" = "#0072B2",
        "AR-Lasso" = "#E69F00",
        "ARIMA" = "#CC79A7",
        "Negative-Binomial-AR" = "#009E73",
        "Naive" = "#7F7F7F"
      )
    ) +
    
    scale_shape_manual(
      name = "",
      values = c(
        "Reported Cases" = 16,
        "VAR-Lasso" = 17,
        "AR-Lasso" = 15,
        "ARIMA" = 18,
        "Negative-Binomial-AR" = 16,
        "Naive" = 3
      )
    ) +
    
    scale_linetype_manual(
      name = "",
      values = c(
        "Reported Cases" = "solid",
        "VAR-Lasso" = "solid",
        "AR-Lasso" = "dashed",
        "ARIMA" = "dotdash",
        "Negative-Binomial-AR" = "twodash",
        "Naive" = "dotted"
      )
    ) +
    
    scale_linewidth_manual(
      values = c(
        "Reported Cases" = line_size,
        "VAR-Lasso" = line_size,
        "AR-Lasso" = line_size,
        "ARIMA" = line_size,
        "Negative-Binomial-AR" = line_size,
        "Naive" = line_size
      ),
      guide = "none"
    ) +
    
    guides(
      colour = guide_legend(
        override.aes = list(
          shape = c(16,17,15,18,16,3),
          linetype = c(
            "solid",
            "solid",
            "dashed",
            "dotdash",
            "twodash",
            "dotted"
          )
        )
      ),
      shape = "none",
      linetype = "none"
    ) +
    
    scale_x_date(
      breaks = x_breaks,
      date_labels = "%m/%d/%y"
    ) +
    
    labs(
      x = "Date (week start date of reported cases)",
      y = "Case counts",
      title = title_plot
    ) +
    
    theme_minimal(base_size = 17) +
    
    theme(
      plot.title = element_text(size = 25, hjust = 0.5),
      legend.position = "bottom",
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 17),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
      axis.text.y = element_text(size = 18),
      axis.title.y = element_text(size = 19),
      axis.title.x = element_text(size = 19),
      legend.key.size = unit(1.2, "lines"),
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(0.8, "cm")
    )
}


#*************************
#* PLOT CASES
#* ************************

PLOT_FORECASTS <- function(
    df_preds_var,
    df_preds_ar,
    df_preds_arima,
    df_preds_nb,
    df_preds_naive,
    list_ordered_jur,
    title_plot,
    n_col_plot = 2,
    two_figures = TRUE,
    point_size = 3.8,
    line_size = 1.45
) {
  
  jur_labels <- GET_JUR_LABELS()
  
  #-----------------------------
  # FORMAT PREDICTIONS
  #-----------------------------
  
  df_preds_var <- df_preds_var %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "VAR-Lasso"
    )
  
  df_preds_ar <- df_preds_ar %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "AR-Lasso"
    )
  
  df_preds_arima <- df_preds_arima %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "ARIMA"
    )
  
  df_preds_nb <- df_preds_nb %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "Negative-Binomial-AR"
    )
  
  df_preds_naive <- df_preds_naive %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "Naive"
    )
  
  
  #-----------------------------
  # TRUE DATA
  # (only need one copy)
  #-----------------------------
  
  df_true <- df_preds_var %>%
    dplyr::select(
      Week_Number,
      date_week_start,
      Jurisdiction,
      Actual
    ) %>%
    distinct() %>%
    rename(Cases = Actual) %>%
    mutate(Source = "True")
  
  
  #-----------------------------
  # ALL FORECASTS
  #-----------------------------
  
  df_preds_all <- bind_rows(
    df_preds_var,
    df_preds_ar,
    df_preds_arima,
    df_preds_nb,
    df_preds_naive
  ) %>%
    dplyr::select(
      Week_Number,
      date_week_start,
      Jurisdiction,
      Predicted,
      Method
    ) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  
  #-----------------------------
  # COMBINE DATA
  #-----------------------------
  
  df_plot <- bind_rows(
    df_true,
    df_preds_all
  ) %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      
      Source = factor(
        Source,
        levels = c("True", "Predicted")
      ),
      
      Jurisdiction_orig = Jurisdiction,
      
      Jurisdiction = factor(
        jur_labels[Jurisdiction],
        levels = jur_labels[list_ordered_jur]
      ),
      
      Method = factor(
        Method,
        levels = c(
          "VAR-Lasso",
          "AR-Lasso",
          "ARIMA",
          "Negative-Binomial-AR",
          "Naive"
        )
      )
    )
  
  
  #-----------------------------
  # X AXIS
  #-----------------------------
  
  date_seq <- df_plot %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  
  if (tail(date_seq, 1) != tail(x_breaks, 1)) {
    x_breaks <- c(x_breaks, tail(date_seq, 1))
  }
  
  
  #-----------------------------
  # PLOT FUNCTION
  #-----------------------------
  
  plot_subset <- function(subset_jur) {
    
    ggplot() +
      
      # True data
      geom_line(
        data = df_plot %>%
          filter(
            Jurisdiction_orig %in% subset_jur,
            Source == "True"
          ),
        aes(
          x = date_week_start,
          y = Cases
        ),
        colour = "black",
        linewidth = line_size
      ) +
      
      geom_point(
        data = df_plot %>%
          filter(
            Jurisdiction_orig %in% subset_jur,
            Source == "True"
          ),
        aes(
          x = date_week_start,
          y = Cases
        ),
        colour = "black",
        shape = 16,
        size = point_size
      ) +
      
      # Forecasts
      geom_line(
        data = df_plot %>%
          filter(
            Jurisdiction_orig %in% subset_jur,
            Source == "Predicted"
          ),
        aes(
          x = date_week_start,
          y = Cases,
          colour = Method,
          linetype = Method
        ),
        linewidth = line_size
      ) +
      
      geom_point(
        data = df_plot %>%
          filter(
            Jurisdiction_orig %in% subset_jur,
            Source == "Predicted"
          ),
        aes(
          x = date_week_start,
          y = Cases,
          colour = Method,
          shape = Method
        ),
        size = point_size
      ) +
      
      scale_color_manual(
        name = "",
        values = c(
          "VAR-Lasso" = "#0072B2",
          "AR-Lasso" = "#E69F00",
          "ARIMA" = "#CC79A7",
          "Negative-Binomial-AR" = "#009E73",
          "Naive" = "#7F7F7F"
        )
      ) +
      
      scale_shape_manual(
        name = "",
        values = c(
          "VAR-Lasso" = 17,
          "AR-Lasso" = 15,
          "ARIMA" = 18,
          "Negative-Binomial-AR" = 16,
          "Naive" = 3
        ),
        guide = "none"
      ) +
      
      scale_linetype_manual(
        name = "",
        values = c(
          "VAR-Lasso" = "solid",
          "AR-Lasso" = "dashed",
          "ARIMA" = "dotdash",
          "Negative-Binomial-AR" = "twodash",
          "Naive" = "dotted"
        ),
        guide = "none"
      ) +
      
      guides(
        colour = guide_legend(
          override.aes = list(
            shape = c(17, 15, 18, 16, 3),
            linetype = c(
              "solid",
              "dashed",
              "dotdash",
              "twodash",
              "dotted"
            )
          )
        )
      ) +
      
      scale_x_date(
        breaks = x_breaks,
        date_labels = "%m/%d/%y"
      ) +
      
      facet_wrap2(
        ~Jurisdiction,
        scales = "free_y",
        axes = "all",
        ncol = n_col_plot
      ) +
      
      labs(
        x = "Month",
        y = "Case counts",
        title = title_plot
      ) +
      
      theme_minimal(base_size = 17) +
      
      # theme(
      #   plot.title = element_text(size = 22, hjust = 0.5),
      #   legend.position = "bottom",
      #   legend.text = element_text(size = 18),
      #   legend.title = element_text(size = 16),
      #   axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
      #   axis.text.y = element_text(size = 18),
      #   axis.title.x = element_text(size = 18),
      #   axis.title.y = element_text(size = 18),
      #   strip.text = element_text(size = 19),
      #   legend.key.size = unit(1.2, "lines"),
      #   legend.key.width = unit(2, "cm"),
      #   legend.key.height = unit(0.8, "cm")
      # )
    
      theme(
        plot.title = element_text(
          size = 22,
          hjust = 0.5
        ),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1, size = 16
        ), #axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        strip.text = element_text(size = 19),
        legend.key.size = unit(1.2, "lines"),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.8, "cm")
      )
  }
  
  
  #-----------------------------
  # RETURN PLOTS
  #-----------------------------
  
  if (!two_figures) {
    
    return(
      plot_subset(list_ordered_jur)
    )
    
  } else {
    
    jur_chunks <- split(
      list_ordered_jur,
      ceiling(seq_along(list_ordered_jur) / 4)
    )
    
    plot_list <- lapply(seq_along(jur_chunks), function(i) {
      
      p <- plot_subset(jur_chunks[[i]])
      
      print(p)
      
    })
    
    invisible(plot_list)
  }
}

GET_JUR_LABELS <- function(){
  
  jur_labels <- c(
    "NewYorkCity" = "New York City",
    "Texas"      = "Texas",
    "LA"         = "Los Angeles County",
    "Florida"    = "Florida",
    "Illinois"   = "Illinois",
    "Georgia"    = "Georgia",
    "SanDiego"   = "San Diego County",
    "Colorado"   = "Colorado",
    "Washington" = "Washington State",
    "NewJersey"  = "New Jersey",
    "Philadelphia" = "Philadelphia",
    "NewYork" = "New York State",
    "Massachusetts" = "Massachusetts",
    "NorthCarolina" = "North Carolina",
    "Arizona" = "Arizona",
    "Virginia" = "Virginia",
    "Philadelphia" = "Philadelphia",
    "DistrictofColumbia" = "District of Columbia",
    "Maryland"  =  "Maryland",
    "Ohio" =  "Ohio"
  )
  
  return(jur_labels)
  
}

# The five models must be scored on the same forecast weeks
check_same_weeks <- function(forecast_list) {
  n_rows <- sapply(forecast_list, nrow)
  if (length(unique(n_rows)) > 1) {
    warning("Different numbers of forecast rows: ", paste(names(n_rows), n_rows, collapse = ", "))
  }
  if (all(sapply(forecast_list, function(x) "Week_Number" %in% names(x)))) {
    wk <- lapply(forecast_list, function(x) sort(unique(x$Week_Number)))
    if (!all(sapply(wk, identical, wk[[1]]))) warning("The models forecast different weeks.")
  }
  invisible(n_rows)
}

#FINAL PLOT
Plot_final_models <- function(var_output, nb_output, ar_output, arima_output, naive_output, title_plot,
                                jurisdiction_interest = "SanDiego") {

  #uf <- lapply(univariate_results$forecasts, function(x) x %>% filter(Jurisdiction == jurisdiction_interest))
  check_same_weeks(list(VAR = var_output$forecasts, AR = ar_output$forecasts$AR_Lasso, ARIMA = arima_output$forecasts$ARIMA,
                        NB = nb_output$forecasts, Naive = naive_output$forecasts$Naive))
  PLOT_FORECASTS_SINGLE_JURISDICITON(var_output$forecasts, ar_output$forecasts$AR_Lasso, arima_output$forecasts$ARIMA,
                                     nb_output$forecasts, naive_output$forecasts$Naive, title_plot)
}

# Print the train and test windows -- compare with the methods text
print_split_check <- function(data_mpox, TRAIN_WEEK, list_jur) {
  d <- data_mpox %>% filter(Jurisdiction %in% list_jur)
  split <- get_train_test_data(d, TRAIN_WEEK)
  tr <- GET_TS_DATA(split$train_data)
  fu <- GET_TS_DATA(split$future_data)
  cat("Train:", format(min(as.Date(tr$date_week_start))), "to", format(max(as.Date(tr$date_week_start))),
      "(", nrow(tr), "weeks )  |  Test:", format(min(as.Date(fu$date_week_start))), "to",
      format(max(as.Date(fu$date_week_start))), "(", nrow(fu), "weeks )\n")
}