#*****************************************
# PLOT FUNCTIONS
#
# Purpose
#   Plotting helpers for the Mpox forecasting analysis: weekly reported cases by
#   jurisdiction, the two-week-ahead forecast figure for San Diego County
#   (SDC), and the population-adjusted (rate-based) forecast figure.
#
# Layout of this file
#   1. Helpers
#        GET_JUR_LABELS()    : jurisdiction codes to display names
#        check_same_weeks()  : warns if the models were scored on different weeks
#        print_split_check() : prints the train and test windows, to compare
#                              with the Methods text
#   2. Weekly reported cases (Figure 1)
#        PLOT_WEEKLY_CASES()
#   3. Forecast figures
#        PLOT_FORECASTS()                  : five-model forecasts, one panel per
#                                            jurisdiction (NOT used for the
#                                            paper; from the multi-target
#                                            analysis)
#        Plot_final_models()               : checks the five models cover the
#                                            same weeks, then draws the main
#                                            SDC forecast figure (Figure 5)
#   4. Population-adjusted figure (Supplementary Figure S2)
#        PLOT_FORECASTS_SUBSET_POPULATION()  (alias: PLOT_FORECASTS_SUBSET)
#
# Defined elsewhere (not in this file)
#   PLOT_FORECASTS_SINGLE_JURISDICITON() : the SDC forecast plot called by
#                                          Plot_final_models() and RUN_VAR()
#
# Needs
#   Packages: ggplot2, dplyr, tidyr, scales, ggh4x
#   Functions defined elsewhere: get_train_test_data(), GET_TS_DATA()
#*****************************************


#*****************************************
# 1. HELPERS
#*****************************************

GET_JUR_LABELS <- function(){
  
  'Named character vector that maps jurisdiction codes used in the data to the
  display names shown on the plots, for example SanDiego to San Diego County.
  Panel order is set by the list of jurisdictions passed to each plot function,
  not by the order here. A code missing from this vector gives an NA panel
  label, so add any new jurisdiction here.
  
  Returns a named character vector.'
  
  jur_labels = c(
    "NewYorkCity" = "New York City", "LA" = "Los Angeles County",
    "Illinois" = "Illinois", "Georgia" = "Georgia",
    "SanDiego" = "San Diego County", "Colorado" = "Colorado", 
    "NewJersey" = "New Jersey", "Philadelphia" = "Philadelphia",
    "NewYork" = "New York State",
    "Ohio" = "Ohio", "Massachusetts" = "Massachusetts"   # CHANGED: added; removed trailing comma
  )
  
  return(jur_labels)
  
}


check_same_weeks <- function(forecast_list) {
  
  'Check that several models were scored on the same forecast weeks.
  
  Args:
    forecast_list : named list of forecast data frames, one per model. Each
                    should have a Week_Number column.
  
  Warns if the data frames have different numbers of rows, or if they cover
  different sets of weeks. Does nothing else.
  
  Returns the number of rows of each data frame, invisibly.'
  
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


print_split_check <- function(data_mpox, TRAIN_WEEK, list_jur) {
  
  'Print the training and test windows so they can be compared with the
  Methods text (training January to December 2023, testing 2024).
  
  Args:
    data_mpox  : long data frame, one row per jurisdiction-week.
    TRAIN_WEEK : index passed to get_train_test_data().
    list_jur   : jurisdictions to include.
  
  Prints the first and last week and the number of weeks in each window.
  Returns nothing useful.'
  
  d <- data_mpox %>% filter(Jurisdiction %in% list_jur)
  split <- get_train_test_data(d, TRAIN_WEEK)
  tr <- GET_TS_DATA(split$train_data)
  fu <- GET_TS_DATA(split$future_data)
  cat("Train:", format(min(as.Date(tr$date_week_start))), "to", format(max(as.Date(tr$date_week_start))),
      "(", nrow(tr), "weeks )  |  Test:", format(min(as.Date(fu$date_week_start))), "to",
      format(max(as.Date(fu$date_week_start))), "(", nrow(fu), "weeks )\n")
}


#*****************************************
# 2. WEEKLY REPORTED CASES
#*****************************************

PLOT_WEEKLY_CASES <- function(
    data_mpox, list_jur,
    title_plot = "Mpox Weekly Reported Cases. January 2023 - November 2024",
    n_col_plot = 2, n_row_plot = 3, two_figures = TRUE,
    n_per_figure = n_col_plot * n_row_plot,   # 6 per page by default
    point_size = 1.6, line_size = 0.8, break_every = 8) {
  
  'Plot weekly reported cases (observed counts only), one panel per
  jurisdiction with its own y-axis scale.
  
  Args:
    data_mpox    : long data frame with columns Jurisdiction, date_week_start
                   and Cases.
    list_jur     : jurisdictions to plot, in panel order.
    title_plot   : plot title.
    n_col_plot, n_row_plot : panel grid per figure.
    two_figures  : TRUE splits the jurisdictions over several figures of
                   n_per_figure panels; FALSE draws one figure.
    n_per_figure : panels per figure when two_figures is TRUE.
    point_size, line_size : marker and line sizes.
    break_every  : an x-axis label every this many weeks. The last week is
                   added only if it does not crowd the previous label.
  
  Prints each figure. Returns NULL invisibly.'
  
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


#*****************************************
# 3. FORECAST FIGURES
#*****************************************

PLOT_FORECASTS <- function(df_preds_var, df_preds_ar, df_preds_arima, df_preds_nb,
                           df_preds_naive, list_ordered_jur, title_plot,
                           n_col_plot = 2, two_figures = TRUE,
                           point_size = 3.8, line_size = 1.45) {
  
  'Plot observed weekly cases (black) and the forecasts of the five models,
  one panel per jurisdiction. Not used for the paper; kept from the
  multi-target analysis.
  
  Args:
    df_preds_var, df_preds_ar, df_preds_arima, df_preds_nb, df_preds_naive :
                       forecast data frames for the VAR-Lasso, AR-Lasso,
                       ARIMA, negative binomial AR-Lasso and naive models, each
                       with columns Week_Number, date_week_start, Jurisdiction,
                       Predicted and Actual.
    list_ordered_jur : jurisdictions to plot, in panel order.
    title_plot       : plot title.
    n_col_plot       : number of panel columns.
    two_figures      : TRUE prints the jurisdictions in groups of four, one
                       figure per group; FALSE returns a single plot.
    point_size, line_size : marker and line sizes.
  
  Observed counts are taken from the Actual column of df_preds_var. Method
  colours, shapes and line types are fixed (see the methods vector below).
  
  Returns a ggplot if two_figures is FALSE. Otherwise prints each figure and
  returns the list of plots invisibly.'
  
  jur_labels <- GET_JUR_LABELS()
  
  # Method styles, defined once and used for the data, scales and legend
  methods <- c("VAR-Lasso", "AR-Lasso", "ARIMA", "Negative-Binomial-AR", "Naive")
  cols    <- setNames(c("#0072B2", "#E69F00", "#CC79A7", "#009E73", "#7F7F7F"), methods)
  shapes  <- setNames(c(17, 15, 18, 16, 3), methods)
  ltypes  <- setNames(c("solid", "dashed", "dotdash", "twodash", "dotted"), methods)
  
  # Label each forecast data frame with its method and stack them
  preds <- list(df_preds_var, df_preds_ar, df_preds_arima, df_preds_nb, df_preds_naive)
  df_preds_all <- bind_rows(lapply(seq_along(preds), function(i)
    preds[[i]] %>% mutate(date_week_start = as.Date(date_week_start), Method = methods[i]))) %>%
    dplyr::select(Week_Number, date_week_start, Jurisdiction, Predicted, Method) %>%
    rename(Cases = Predicted) %>% mutate(Source = "Predicted")
  
  # Observed data (one copy, taken from the VAR-Lasso data frame)
  df_true <- df_preds_var %>%                                  # CHANGED: simpler, same result
    mutate(date_week_start = as.Date(date_week_start)) %>%
    dplyr::select(Week_Number, date_week_start, Jurisdiction, Actual) %>%
    distinct() %>% rename(Cases = Actual) %>% mutate(Source = "True")
  
  df_plot <- bind_rows(df_true, df_preds_all) %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(Source = factor(Source, levels = c("True", "Predicted")),
           Jurisdiction_orig = Jurisdiction,
           Jurisdiction = factor(jur_labels[Jurisdiction], levels = jur_labels[list_ordered_jur]),
           Method = factor(Method, levels = methods))
  
  # X axis: a label every fourth week, plus the last week
  date_seq <- sort(unique(df_plot$date_week_start))
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  if (tail(date_seq, 1) != tail(x_breaks, 1)) x_breaks <- c(x_breaks, tail(date_seq, 1))
  
  plot_subset <- function(subset_jur) {
    obs  <- df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Source == "True")
    pred <- df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Source == "Predicted")
    
    ggplot() +
      geom_line(data = obs, aes(date_week_start, Cases), colour = "black", linewidth = line_size) +
      geom_point(data = obs, aes(date_week_start, Cases), colour = "black", shape = 16, size = point_size) +
      geom_line(data = pred, aes(date_week_start, Cases, colour = Method, linetype = Method),
                linewidth = line_size) +
      geom_point(data = pred, aes(date_week_start, Cases, colour = Method, shape = Method),
                 size = point_size) +
      scale_color_manual(name = "", values = cols) +
      scale_shape_manual(name = "", values = shapes, guide = "none") +
      scale_linetype_manual(name = "", values = ltypes, guide = "none") +
      guides(colour = guide_legend(override.aes = list(shape = unname(shapes),
                                                       linetype = unname(ltypes)))) +
      scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
      facet_wrap2(~Jurisdiction, scales = "free_y", axes = "all", ncol = n_col_plot) +
      labs(x = "Month", y = "Case counts", title = title_plot) +
      theme_minimal(base_size = 17) +
      theme(plot.title = element_text(size = 22, hjust = 0.5),
            legend.position = "bottom",
            legend.text = element_text(size = 16), legend.title = element_text(size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
            axis.title.y = element_text(size = 18), strip.text = element_text(size = 19),
            legend.key.size = unit(1.2, "lines"), legend.key.width = unit(2, "cm"),
            legend.key.height = unit(0.8, "cm"))
  }
  
  if (!two_figures) return(plot_subset(list_ordered_jur))
  
  jur_chunks <- split(list_ordered_jur, ceiling(seq_along(list_ordered_jur) / 4))
  plot_list  <- lapply(jur_chunks, function(chunk) print(plot_subset(chunk)))
  invisible(plot_list)
}


Plot_final_models <- function(var_output, nb_output, ar_output, arima_output, naive_output, title_plot,
                              jurisdiction_interest = "SanDiego") {
  
  'Draw the main two-week-ahead forecast figure for San Diego County (Figure 5)
  from the five final model outputs.
  
  Args:
    var_output, nb_output, ar_output, arima_output, naive_output : outputs of
        the model wrappers (run_var_sd, run_nb_sd, run_AR_model,
        run_ARIMA_model, and the naive model). Each must contain a forecasts
        data frame with Week_Number, date_week_start, Jurisdiction, Predicted
        and Actual.
    title_plot            : plot title.
    jurisdiction_interest : not used here (each forecasts data frame is passed
                            on as it is). Kept so existing calls still work.
  
  First checks that the five models forecast the same weeks (warns if not),
  then calls PLOT_FORECASTS_SINGLE_JURISDICITON(), which is defined elsewhere.
  Returns whatever that function returns.'
  
  #uf <- lapply(univariate_results$forecasts, function(x) x %>% filter(Jurisdiction == jurisdiction_interest))
  check_same_weeks(list(VAR = var_output$forecasts, AR = ar_output$forecasts, ARIMA = arima_output$forecasts,
                        NB = nb_output$forecasts, Naive = naive_output$forecasts))
  PLOT_FORECASTS_SINGLE_JURISDICITON(var_output$forecasts, ar_output$forecasts, arima_output$forecasts,
                                     nb_output$forecasts, naive_output$forecasts, title_plot)
}


#*****************************************
# 4. POPULATION-ADJUSTED FIGURE (Supplementary Figure S2)
#*****************************************

PLOT_FORECASTS_SUBSET_POPULATION <- function(df_preds_var, df_preds_ar, title_plot,
                                             jurisdiction_interest = "SanDiego",
                                             point_size = 3.8, line_size = 1.45,
                                             y_label = "Case counts") {
  
  'Plot the VAR-Lasso and AR-Lasso two-week-ahead forecasts for one
  jurisdiction against the observed series, for the population-adjusted
  (incidence-rate) analysis.
  
  Args:
    df_preds_var, df_preds_ar : forecast data frames, or the model output
                    lists that contain them (a forecasts element, or a list
                    whose first element is the data frame). Columns needed:
                    Week_Number, date_week_start, Jurisdiction, Predicted and
                    Actual.
    title_plot    : plot title.
    jurisdiction_interest : jurisdiction to plot (default SanDiego).
    point_size, line_size : marker and line sizes.
    y_label       : y-axis label. The default is Case counts, which is wrong
                    for rate fits, so for rates pass
                    y_label = "Weekly incidence rate (cases per 100,000 population)".
  
  Observed values come from the Actual column of the VAR-Lasso data frame, and
  are on the same scale as the forecasts. A label every fourth week is shown,
  plus the last week.
  
  Returns a ggplot.'
  
  unwrap <- function(x) {
    if (is.data.frame(x)) return(x)
    if (is.list(x) && !is.null(x$forecasts)) return(unwrap(x$forecasts))
    if (is.list(x) && length(x) >= 1)        return(unwrap(x[[1]]))
    stop("Input is not a forecast data frame")
  }
  df_preds_var <- unwrap(df_preds_var)
  df_preds_ar <- unwrap(df_preds_ar)
  
  # tag each model, keep one jurisdiction
  fmt <- function(df, m) df %>%
    filter(Jurisdiction == jurisdiction_interest) %>%
    mutate(date_week_start = as.Date(date_week_start), Method = m)
  df_var   <- fmt(df_preds_var,   "VAR-Lasso")
  df_ar    <- fmt(df_preds_ar,    "AR-Lasso")
  
  methods <- c("VAR-Lasso", "AR-Lasso")
  df_true <- df_var %>% distinct(Week_Number, date_week_start, Actual) %>% rename(Cases = Actual)
  df_pred <- bind_rows(df_var, df_ar) %>%
    dplyr::select(Week_Number, date_week_start, Method, Cases = Predicted) %>%
    mutate(Method = factor(Method, levels = methods))
  
  # styling
  cols <- c("VAR-Lasso" = "#0072B2", "AR-Lasso" = "#E69F00")
  shps <- c("VAR-Lasso" = 17,        "AR-Lasso" = 15)
  ltys <- c("VAR-Lasso" = "solid",   "AR-Lasso" = "dashed")
  
  # x axis: every 4th week, always include the last
  dates    <- sort(unique(df_true$date_week_start))
  x_breaks <- unique(c(dates[seq(1, length(dates), by = 4)], tail(dates, 1)))
  
  ggplot() +
    geom_line (data = df_true, aes(date_week_start, Cases), colour = "black", linewidth = line_size) +
    geom_point(data = df_true, aes(date_week_start, Cases), colour = "black", shape = 16, size = point_size) +
    geom_line (data = df_pred, aes(date_week_start, Cases, colour = Method, linetype = Method), linewidth = line_size) +
    geom_point(data = df_pred, aes(date_week_start, Cases, colour = Method, shape = Method), size = point_size) +
    scale_color_manual(name = "", values = cols) +
    scale_shape_manual(values = shps, guide = "none") +
    scale_linetype_manual(values = ltys, guide = "none") +
    guides(colour = guide_legend(override.aes = list(shape = unname(shps), linetype = unname(ltys)))) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    labs(x = "Date (week start date of reported cases)", y = y_label, title = title_plot) +
    theme_minimal(base_size = 17) +
    theme(plot.title = element_text(size = 22, hjust = 0.5),
          legend.position = "bottom",
          legend.text = element_text(size = 16), legend.title = element_text(size = 16),
          legend.key.size = unit(1.2, "lines"), legend.key.width = unit(2, "cm"),
          legend.key.height = unit(0.8, "cm"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
          axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18))
}
