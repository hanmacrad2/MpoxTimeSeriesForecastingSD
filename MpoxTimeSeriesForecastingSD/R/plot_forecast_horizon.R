PLOT_FORECAST_HORIZON <- function(
    data_model,
    df_preds_var,
    df_preds_ar,
    df_preds_arima,
    df_preds_nb,
    df_preds_naive,
    jurisdiction,
    title_plot = "Forecasts",
    true_data_line_size = 1.1,
    true_data_point_size = 2.2,
    forecast_line_size = 1.5,
    forecast_point_size = 3.8,
    train_long = FALSE
){
  
  #----------------------------------
  # OBSERVED DATA
  #----------------------------------
  
  data_plot <- data_model %>%
    filter(Jurisdiction == jurisdiction) %>%
    mutate(
      date_week_start = as.Date(date_week_start)
    )
  
  #LABELS
  if(train_long == TRUE){
    train_data_label = "Train data (all)"
  } else {
    train_data_label = "Train data\n(latter segment)"
  }
  
  #----------------------------------
  # FORMAT FORECASTS
  #----------------------------------
  
  df_var <- df_preds_var %>%
    filter(Jurisdiction == jurisdiction) %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "VAR-Lasso"
    )
  
  df_ar <- df_preds_ar %>%
    filter(Jurisdiction == jurisdiction) %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "AR-Lasso"
    )
  
  df_arima <- df_preds_arima %>%
    filter(Jurisdiction == jurisdiction) %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "ARIMA"
    )
  
  df_nb <- df_preds_nb %>%
    filter(Jurisdiction == jurisdiction) %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "X", #"Negative-Binomial-AR"
    )
  
  df_naive <- df_preds_naive %>%
    filter(Jurisdiction == jurisdiction) %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "Naive"
    )
  
  
  forecasts <- bind_rows(
    df_var,
    df_ar,
    df_arima,
    df_nb,
    df_naive
  )
  
  
  #----------------------------------
  # DETERMINE FORECAST ORIGIN
  #----------------------------------
  
  forecast_start_week <- min(forecasts$Week_Number)
  
  # two-week-ahead forecast:
  # model cutoff is two weeks before predictions begin
  origin_week <- forecast_start_week - 2
  
  
  origin_date <- data_plot %>%
    filter(Week_Number == origin_week) %>%
    pull(date_week_start)
  
  
  forecast_start_date <- forecasts %>%
    filter(Week_Number == forecast_start_week) %>%
    pull(date_week_start) %>%
    first()
  
  
  # keep this based on the full observed series (as in the original),
  # not just the forecasts, so the shaded/eval region and the trailing
  # comparison line extend through all available observed data
  forecast_end_date <- max(data_plot$date_week_start)
  
  
  #----------------------------------
  # SPLIT OBSERVED DATA INTO 3 SEGMENTS
  #----------------------------------
  
  # 1) Training data actually seen by the model (solid)
  train_data <- data_plot %>%
    filter(date_week_start <= origin_date)
  
  # 2) Blind 2-week-ahead horizon: observed but not given to the model (dashed)
  #    (inclusive boundaries so geom_line has points to connect to train_data)
  horizon_data <- data_plot %>%
    filter(
      date_week_start >= origin_date,
      date_week_start <= forecast_start_date
    )
  
  # 3) Observed data from forecast start onward: this is what the forecasts
  #    are being compared against, so it must stay visible (solid) -
  #    this is the segment that went missing in the last version
  post_forecast_data <- data_plot %>%
    filter(date_week_start >= forecast_start_date)
  
  
  #----------------------------------
  # X AXIS
  #----------------------------------
  
  date_seq <- data_plot$date_week_start
  
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  
  if(train_long == FALSE){
    x_breaks <- c(x_breaks, tail(date_seq,1))
  }
  
  
  # if(tail(date_seq,1) != tail(x_breaks,1)){
  #   x_breaks <- c(x_breaks, tail(date_seq,1))
  # }
  
  
  #----------------------------------
  # PLOT
  #----------------------------------
  
  ggplot() +
    
    # Forecast evaluation period - shaded from where the coloured
    # forecast lines actually begin (forecast_start_date), not from
    # the model cutoff/origin
    annotate(
      "rect",
      xmin = forecast_start_date,
      xmax = forecast_end_date,
      ymin = -Inf,
      ymax = Inf,
      alpha = 0.08,
      fill = "grey50"
    ) +
    
    
    # Model cutoff (last observation available)
    geom_vline(
      xintercept = origin_date,
      linetype = "dashed",
      linewidth = 0.9
    ) +
    
    
    # Actual forecast start
    geom_vline(
      xintercept = forecast_start_date,
      linetype = "dotted",
      linewidth = 0.9
    ) +
    
    
    #----------------------------------
  # TRUE DATA (3 segments)
  #----------------------------------
  
  # Training data seen by model - solid
  geom_line(
    data = train_data,
    aes(
      x = date_week_start,
      y = Cases
    ),
    colour = "black",
    linewidth = true_data_line_size
  ) +
    
    geom_point(
      data = train_data,
      aes(
        x = date_week_start,
        y = Cases
      ),
      colour = "black",
      shape = 16,
      size = true_data_point_size
    ) +
    
    
    # Blind 2-week horizon - dashed
    geom_line(
      data = horizon_data,
      aes(
        x = date_week_start,
        y = Cases
      ),
      colour = "black",
      linewidth = true_data_line_size,
      linetype = "dashed"
    ) +
    
    geom_point(
      data = horizon_data,
      aes(
        x = date_week_start,
        y = Cases
      ),
      colour = "black",
      shape = 16,
      size = true_data_point_size
    ) +
    
    
    # Observed data during/after forecast period - solid again,
    # so it stays visible as the comparison line against the forecasts
    geom_line(
      data = post_forecast_data,
      aes(
        x = date_week_start,
        y = Cases
      ),
      colour = "black",
      linewidth = true_data_line_size
    ) +
    
    geom_point(
      data = post_forecast_data,
      aes(
        x = date_week_start,
        y = Cases
      ),
      colour = "black",
      shape = 16,
      size = true_data_point_size
    ) +
    
    
    #----------------------------------
  # FORECAST MODELS
  #----------------------------------
  
  geom_line(
    data = forecasts,
    aes(
      x = date_week_start,
      y = Predicted,
      colour = Method,
      linetype = Method
    ),
    linewidth = forecast_line_size
  ) +
    
    geom_point(
      data = forecasts,
      aes(
        x = date_week_start,
        y = Predicted,
        colour = Method,
        shape = Method
      ),
      size = forecast_point_size
    ) +
    
    
    scale_colour_manual(
      name = "",
      values = c(
        "VAR-Lasso" = "#0072B2",
        "AR-Lasso" = "#E69F00",
        "ARIMA" = "#CC79A7",
        "X" = "#0072B2", #"white", #Negative-Binomial-AR" = "white", #'lightgrey',  #"#009E73",
        "Naive" = "#7F7F7F"
      )
    ) +
    
    
    scale_shape_manual(
      values = c(
        "VAR-Lasso" = 17,
        "AR-Lasso" = 15,
        "ARIMA" = 18,
       "X" = 16, #"Negative-Binomial-AR" = 16,
        "Naive" = 3
      ),
      guide = "none"
    ) +
    
    
    scale_linetype_manual(
      values = c(
        "VAR-Lasso" = "solid",
        "AR-Lasso" = "dashed",
        "ARIMA" = "dotdash",
        "X" = "twodash", # "Negative-Binomial-AR" = "twodash",
        "Naive" = "dotted"
      ),
      guide = "none"
    ) +
    
    
    guides(
      colour = guide_legend(
        override.aes = list(
          shape = c(17,15,18,16,3),
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
    
    
    #----------------------------------
  # LABELS
  #----------------------------------
  
  # Training data label
  
  annotate(
    "label",
    x = origin_date -
      (origin_date - min(data_plot$date_week_start))/2,
    y = max(data_plot$Cases)*1.15,
    label = train_data_label,
    size = 5,
    fill = "white",
    alpha = 0.85,
    label.size = NA
  ) +
    
    
    # Model cutoff label
    
    annotate(
      "label",
      x = origin_date,
      y = max(data_plot$Cases)*1.05,
      label = "Train data cutoff",
      hjust = 0.5,
      size = 5,
      fill = "white",
      alpha = 0.85,
      label.size = NA
    ) +
    
    
    # Forecast start label
    
    annotate(
      "label",
      x = forecast_start_date,
      y = max(data_plot$Cases)*0.98,
      label = "",
      hjust = 0.5,
      size = 5,
      fill = "white",
      alpha = 0.85,
      label.size = NA
    ) +
    
    
    # Two-week blind forecast horizon
    
    annotate(
      "label",
      x = origin_date +
        (forecast_start_date-origin_date)/2,
      y = max(data_plot$Cases)*0.95,
      label = "2-week ahead horizon\n(unseen data)",
      size = 4.5,
      fill = "white",
      alpha = 0.85,
      label.size = NA
    ) +
    
    
    # Forecast period label
    
    annotate(
      "label",
      x = forecast_start_date +
        (forecast_end_date-forecast_start_date)/2,
      y = max(data_plot$Cases)*1.15,
      label = "Forecast period",
      size = 5,
      fill = "white",
      alpha = 0.85,
      label.size = NA
    ) +
    
    
    scale_x_date(
      breaks = x_breaks,
      date_labels = "%m/%d/%y"
    ) +
    
    
    # Slight headroom above the "Train data" / "Forecast period" labels
    expand_limits(
      y = max(data_plot$Cases, na.rm = TRUE) * 1.2
    ) +
    
    
    labs(
      x = "Month",
      y = "Case counts",
      title = title_plot
    ) +
    
    
    theme_minimal(base_size = 17) +
    
    theme(
      plot.title = element_text(
        size = 22,
        hjust = 0.5
      ),
      legend.position = "bottom",
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 16),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 18
      ),
      axis.text.y = element_text(size = 18),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      legend.key.size = unit(1.2,"lines"),
      legend.key.width = unit(2,"cm"),
      legend.key.height = unit(0.8,"cm")
    )
  
}



#************************
#* MULTI-JURSIDCTIONAL
#************************

#*******************************************
#* PLOT FORECAST HORIZON - MULTI JURISDICTION
#* ******************************************
#
# Facets PLOT_FORECAST_HORIZON's single-jurisdiction shaded-horizon
# plot across several jurisdictions at once (default 2 columns).
#
# Key difference from the single-jurisdiction version: annotate()
# places the SAME x/y position on every facet panel, which breaks
# once jurisdictions have different case scales or slightly
# different date ranges. Instead, we build one row of "meta" info
# per jurisdiction (origin date, forecast start, forecast end, max
# cases, min date) and pass that to geom_vline()/geom_rect()/
# geom_label() via data = ..., so each panel gets its own correctly
# positioned cutoff line, shading, and labels.

#*******************************************
#* PLOT FORECAST HORIZON - MULTI JURISDICTION
#* ******************************************
#
# Facets PLOT_FORECAST_HORIZON's single-jurisdiction shaded-horizon
# plot across several jurisdictions at once (default 2 columns).
#
# Key difference from the single-jurisdiction version: annotate()
# places the SAME x/y position on every facet panel, which breaks
# once jurisdictions have different case scales or slightly
# different date ranges. Instead, we build one row of "meta" info
# per jurisdiction (origin date, forecast start, forecast end, max
# cases, min date) and pass that to geom_vline()/geom_rect()/
# geom_label() via data = ..., so each panel gets its own correctly
# positioned cutoff line, shading, and labels.

PLOT_FORECAST_HORIZON_MULTI <- function(
    data_model,
    df_preds_var,
    df_preds_ar,
    df_preds_arima,
    df_preds_nb,
    df_preds_naive,
    list_ordered_jur,
    title_plot = "Forecasts",
    true_data_line_size = 1.1,
    true_data_point_size = 2.2,
    forecast_line_size = 1.5,
    forecast_point_size = 3.8,
    train_long = FALSE,
    n_col_plot = 2,
    two_figures = TRUE
){
  
  jur_labels <- GET_JUR_LABELS()
  
  #----------------------------------
  # LABEL FOR TRAIN DATA ANNOTATION
  #----------------------------------
  
  train_data_label <- if (train_long) {
    "Train data (all)"
  } else {
    "Train data (latter segment)" #"Train data\n(latter segment)"
  }
  
  
  #----------------------------------
  # OBSERVED DATA (ALL JURISDICTIONS)
  #----------------------------------
  
  data_plot <- data_model %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      date_week_start = as.Date(date_week_start)
    )
  
  
  #----------------------------------
  # FORMAT FORECASTS (ALL JURISDICTIONS)
  #----------------------------------
  
  df_var <- df_preds_var %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "VAR-Lasso"
    )
  
  df_ar <- df_preds_ar %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "AR-Lasso"
    )
  
  df_arima <- df_preds_arima %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "ARIMA"
    )
  
  df_nb <- df_preds_nb %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "Negative-Binomial-AR"
    )
  
  df_naive <- df_preds_naive %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Method = "Naive"
    )
  
  forecasts <- bind_rows(
    df_var,
    df_ar,
    df_arima,
    df_nb,
    df_naive
  )
  
  
  #----------------------------------
  # PER-JURISDICTION FORECAST ORIGIN / START / END
  #----------------------------------
  
  # forecast_start_week and origin_week, one row per jurisdiction
  jur_meta <- forecasts %>%
    group_by(Jurisdiction) %>%
    summarise(
      forecast_start_week = min(Week_Number),
      .groups = "drop"
    ) %>%
    mutate(
      origin_week = forecast_start_week - 2
    )
  
  # origin_date per jurisdiction (last observation available to the model)
  origin_dates <- data_plot %>%
    inner_join(
      jur_meta %>% dplyr::select(Jurisdiction, origin_week),
      by = "Jurisdiction"
    ) %>%
    filter(Week_Number == origin_week) %>%
    distinct(Jurisdiction, origin_date = date_week_start)
  
  # forecast_start_date per jurisdiction
  forecast_start_dates <- forecasts %>%
    inner_join(
      jur_meta %>% dplyr::select(Jurisdiction, forecast_start_week),
      by = "Jurisdiction"
    ) %>%
    filter(Week_Number == forecast_start_week) %>%
    group_by(Jurisdiction) %>%
    summarise(
      forecast_start_date = first(date_week_start),
      .groups = "drop"
    )
  
  # forecast_end_date, min_date, max_cases per jurisdiction
  # (forecast_end_date kept as the full observed series, as in the
  # single-jurisdiction version, so shading/trailing comparison line
  # extend through all available observed data for that jurisdiction)
  jur_ranges <- data_plot %>%
    group_by(Jurisdiction) %>%
    summarise(
      forecast_end_date = max(date_week_start),
      min_date          = min(date_week_start),
      max_cases         = max(Cases, na.rm = TRUE),
      .groups = "drop"
    )
  
  jur_meta <- jur_meta %>%
    left_join(origin_dates,        by = "Jurisdiction") %>%
    left_join(forecast_start_dates, by = "Jurisdiction") %>%
    left_join(jur_ranges,          by = "Jurisdiction")
  
  
  #----------------------------------
  # APPLY JURISDICTION LABELS + FACET ORDER
  #----------------------------------
  
  data_plot <- data_plot %>%
    mutate(
      Jurisdiction_orig = Jurisdiction,
      Jurisdiction = factor(
        jur_labels[Jurisdiction],
        levels = jur_labels[list_ordered_jur]
      )
    )
  
  forecasts <- forecasts %>%
    mutate(
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
  
  jur_meta <- jur_meta %>%
    mutate(
      Jurisdiction_orig = Jurisdiction,
      Jurisdiction = factor(
        jur_labels[Jurisdiction],
        levels = jur_labels[list_ordered_jur]
      )
    )
  
  
  #----------------------------------
  # SPLIT OBSERVED DATA INTO 3 SEGMENTS (PER JURISDICTION)
  #----------------------------------
  
  data_plot_joined <- data_plot %>%
    left_join(
      jur_meta %>% dplyr::select(Jurisdiction, origin_date, forecast_start_date),
      by = "Jurisdiction"
    )
  
  # 1) Training data actually seen by the model (solid)
  train_data <- data_plot_joined %>%
    filter(date_week_start <= origin_date)
  
  # 2) Blind 2-week-ahead horizon: observed but not given to the model (dashed)
  horizon_data <- data_plot_joined %>%
    filter(
      date_week_start >= origin_date,
      date_week_start <= forecast_start_date
    )
  
  # 3) Observed data from forecast start onward - stays visible (solid)
  #    as the comparison line against the forecasts
  post_forecast_data <- data_plot_joined %>%
    filter(date_week_start >= forecast_start_date)
  
  
  #----------------------------------
  # X AXIS
  #----------------------------------
  
  # Assumes a shared reporting calendar across jurisdictions (same
  # weeks), which is typical when all jurisdictions come from one
  # forecast run. Breaks are shared across facets since x is not free.
  date_seq <- data_plot %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  
  last_break <- tail(x_breaks, 1)
  last_date  <- tail(date_seq, 1)
  
  if (!train_long && last_date != last_break) {
    
    gap_weeks <- as.numeric(difftime(last_date, last_break, units = "weeks"))
    
    if (gap_weeks < 2) {
      x_breaks[length(x_breaks)] <- last_date
    } else {
      x_breaks <- c(x_breaks, last_date)
    }
    
  }
  
  
  #----------------------------------
  # PLOT FUNCTION (one set of jurisdictions -> one figure)
  #----------------------------------
  
  plot_subset <- function(subset_jur_orig) {
    
    meta_sub  <- jur_meta            %>% filter(Jurisdiction_orig %in% subset_jur_orig)
    train_sub <- train_data          %>% filter(Jurisdiction_orig %in% subset_jur_orig)
    horiz_sub <- horizon_data        %>% filter(Jurisdiction_orig %in% subset_jur_orig)
    post_sub  <- post_forecast_data  %>% filter(Jurisdiction_orig %in% subset_jur_orig)
    fc_sub    <- forecasts           %>% filter(Jurisdiction_orig %in% subset_jur_orig)
    
    ggplot() +
      
      # Forecast evaluation period - shaded per jurisdiction panel,
      # from where the coloured forecast lines begin through to the
      # end of that jurisdiction's observed data
      geom_rect(
        data = meta_sub,
        aes(
          xmin = forecast_start_date,
          xmax = forecast_end_date
        ),
        ymin = -Inf,
        ymax = Inf,
        alpha = 0.08,
        fill = "grey50",
        inherit.aes = FALSE
      ) +
      
      
      # Model cutoff (last observation available) - per panel
      geom_vline(
        data = meta_sub,
        aes(xintercept = origin_date),
        linetype = "dashed",
        linewidth = 0.9
      ) +
      
      
      # Actual forecast start - per panel
      geom_vline(
        data = meta_sub,
        aes(xintercept = forecast_start_date),
        linetype = "dotted",
        linewidth = 0.9
      ) +
      
      
      #----------------------------------
    # TRUE DATA (3 segments)
    #----------------------------------
    
    geom_line(
      data = train_sub,
      aes(x = date_week_start, y = Cases),
      colour = "black",
      linewidth = true_data_line_size
    ) +
      
      geom_point(
        data = train_sub,
        aes(x = date_week_start, y = Cases),
        colour = "black",
        shape = 16,
        size = true_data_point_size
      ) +
      
      
      geom_line(
        data = horiz_sub,
        aes(x = date_week_start, y = Cases),
        colour = "black",
        linewidth = true_data_line_size,
        linetype = "dashed"
      ) +
      
      geom_point(
        data = horiz_sub,
        aes(x = date_week_start, y = Cases),
        colour = "black",
        shape = 16,
        size = true_data_point_size
      ) +
      
      
      geom_line(
        data = post_sub,
        aes(x = date_week_start, y = Cases),
        colour = "black",
        linewidth = true_data_line_size
      ) +
      
      geom_point(
        data = post_sub,
        aes(x = date_week_start, y = Cases),
        colour = "black",
        shape = 16,
        size = true_data_point_size
      ) +
      
      
      #----------------------------------
    # FORECAST MODELS
    #----------------------------------
    
    geom_line(
      data = fc_sub,
      aes(
        x = date_week_start,
        y = Predicted,
        colour = Method,
        linetype = Method
      ),
      linewidth = forecast_line_size
    ) +
      
      geom_point(
        data = fc_sub,
        aes(
          x = date_week_start,
          y = Predicted,
          colour = Method,
          shape = Method
        ),
        size = forecast_point_size
      ) +
      
      
      scale_colour_manual(
        name = "",
        values = c(
          "VAR-Lasso" = "#0072B2",
          "AR-Lasso" = "#E69F00",
          "ARIMA" = "#CC79A7",
          "Negative-Binomial-AR" = "#0072B2", #"#009E73",
          "Naive" = "#7F7F7F"
        )
      ) +
      
      scale_shape_manual(
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
            shape = c(17,15,18,16,3),
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
      
      
      #----------------------------------
    # LABELS (per panel, driven by meta_sub)
    #----------------------------------
    
    # Training data label
    geom_label(
      data = meta_sub,
      aes(
        x = origin_date - (origin_date - min_date) / 2,
        y = max_cases * 1.15
      ),
      label = train_data_label,
      size = 5,
      fill = "white",
      alpha = 0.85,
      label.size = NA,
      inherit.aes = FALSE
    ) +
      
      
      # Train data cutoff label
      geom_label(
        data = meta_sub,
        aes(
          x = origin_date,
          y = max_cases * 1.05
        ),
        label = "", #Train data cutoff",
        hjust = 0.5,
        size = 5,
        fill = "white",
        alpha = 0.85,
        label.size = NA,
        inherit.aes = FALSE
      ) +
      
      
      # Two-week blind forecast horizon label
      geom_label(
        data = meta_sub,
        aes(
          x = origin_date + (forecast_start_date - origin_date) / 2,
          y = max_cases * 0.95
        ),
        label = "", # "2-week ahead horizon\n(unseen data)",
        size = 4.5,
        fill = "white",
        alpha = 0.85,
        label.size = NA,
        inherit.aes = FALSE
      ) +
      
      
      # Forecast period label
      geom_label(
        data = meta_sub,
        aes(
          x = forecast_start_date + (forecast_end_date - forecast_start_date) / 2,
          y = max_cases * 1.15
        ),
        label = "Forecast period",
        size = 5,
        fill = "white",
        alpha = 0.85,
        label.size = NA,
        inherit.aes = FALSE
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
          hjust = 1,
          size = 16
        ),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        strip.text = element_text(size = 19),
        legend.key.size = unit(1.2,"lines"),
        legend.key.width = unit(2,"cm"),
        legend.key.height = unit(0.8,"cm")
      )
  }
  
  
  #----------------------------------
  # RETURN PLOT(S)
  #----------------------------------
  
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

