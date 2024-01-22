#' Calculate Tag Overlap Statistic
#'
#' This function computes the tag overlap statistic and tagging rate per vessel based on the provided CCAMLR data extract.
#'
#' @param ccamlr_data A mandatory parameter representing the official CCAMLR data extract. The function relies on the presence of five specific data frames in the extract: C2, C2_CATCH, OBS_HAUL_BIOLOGY, OBS_HAUL_TAG_RELEASE, OBS_HAUL_TAG_RECAPTURE.
#' @param taxon A mandatory parameter specifying the taxon code for which the overlap statistic is calculated. Options are 'TOP' or 'TOA'.
#' @param seasons An optional parameter with a default value of NULL. Specify a list of seasons for which you want to calculate the overlap statistic, e.g., c(2020, 2021, 2022). The default value of NULL calculates the overlap statistics for all seasons present in the data.
#' @param vessels An optional parameter with a default value of NULL. Specify a list of vessels for which you want to calculate the overlap statistic, e.g., c('Tronio', 'Janas'). The default value of NULL calculates the overlap statistics for all vessels present in the data.
#' @param areas An optional parameter with a default value of 'ALL'. Specify a list of asd_codes, ssru_codes, or rb_codes for which you want to calculate the overlap statistic, e.g., c('881', '881H', '881K', '486_2', 'combined'). Use 'combined' to calculate a single combined statistic for all areas. Use 'CCEP' to calculate the overlap statistic solely for areas where a CM requires a minimum overlap. The default value of 'ALL' calculates the overlap statistics for all possible areas present in the data.
#' @param plot An optional parameter with a default value of FALSE. When set to TRUE, the tag overlap statistic function will generate a plot for each calculated overlap statistic.
#' 
#' @import dplyr ggplot2
#' @export
#'
#' @examples
#' tag_olap(ccamlr_data = ccamlr_data, taxon = 'TOP', seasons = 2017, areas = '881')
#' tag_olap(ccamlr_data = ccamlr_data, taxon = 'TOA', seasons = c(2016, 2017), areas = 'CCEP')
#' tag_olap(ccamlr_data = ccamlr_data, taxon = 'TOA', vessels = c('vessel_2'), seasons = c(2016, 2017), areas = c("881"), plot = TRUE)
#'

tag_olap <- function(ccamlr_data, taxon = NA, seasons = NULL, vessels = NULL, areas = 'ALL', plot = F) {
  
  # Check if ccamlr_data is a list of required dataframes
  required_dataframes <- c('C2', 'C2_CATCH', 'OBS_HAUL_BIOLOGY', 'OBS_HAUL_TAG_RELEASE', 'OBS_HAUL_TAG_RECAPTURE')
  if (!is.list(ccamlr_data) || any(!required_dataframes %in% names(ccamlr_data))) {
    stop('ccamlr_data should be a list of dataframes including C2, C2_CATCH, OBS_HAUL_BIOLOGY, OBS_HAUL_TAG_RELEASE, OBS_HAUL_TAG_RECAPTURE')
  }
  
  
  # Check if valid taxon is provided (TOP or TOA)
  if (!taxon %in% c('TOP', 'TOA')) {
    cat('No valid taxon provided (only TOP or TOA are accepted). The tag overlap statistic will be calculated for TOA\n')
    taxon <- 'TOA'
  }
  
  # Set areas for CCEP condition and set filter_areas parameter to determine if the data will be filtered on area.

  if (length(areas) == 1) {
    if (areas == 'CCEP') {
      areas <- c('484', '486_2', '486_3', '486_4', '486_5', '5842_1', '5842_2', '5841_1',
                 '5841_2', '5841_3', '5841_4', '5841_5', '5841_6', '882H', '882C', "882D",
                 "882E", "882F", "882G", '882SRU_C_G', "S70", "N70", 'SRZ')
      CCEP <- TRUE
      filter_areas <- TRUE
    } else {
      CCEP <- FALSE
      filter_areas <- areas != 'ALL'
    }
  } else {
    filter_areas <- TRUE
    CCEP <- FALSE
  }
  

  # Keep only the necessary data frames for calculating the overlap statistic
  C2 <- ccamlr_data$C2
  C2_CATCH <- ccamlr_data$C2_CATCH
  OBS_HAUL_BIOLOGY <- ccamlr_data$OBS_HAUL_BIOLOGY
  OBS_HAUL_TAG_RELEASE <- ccamlr_data$OBS_HAUL_TAG_RELEASE
  OBS_HAUL_TAG_RECAPTURE <- ccamlr_data$OBS_HAUL_TAG_RECAPTURE
  
  # Remove the entire ccamlr_data list to free up memory
  rm('ccamlr_data')
  
  
  # Check if there is data available in the selection
  CHECK_DATA <- C2 %>%
    dplyr::filter(if (!is.null(seasons)) season_ccamlr %in% seasons else TRUE) %>%
    dplyr::filter(if (!is.null(vessels)) vessel_name %in% vessels else TRUE) %>%
    dplyr::filter(obs_haul_id %in% unique(OBS_HAUL_BIOLOGY$obs_haul_id))
  
  if (nrow(CHECK_DATA) == 0) {
    stop('No data provided matching the seasons and vessels filter.')
  }
  
  
  
  ########################################
  ### prepare aggregation dataframe  #####
  ########################################
  
  aggregation <- C2 %>% dplyr::select(
    latitude_set_start, longitude_set_start,
    latitude_set_end, longitude_set_end,
    c2_id, obs_haul_id, obs_logbook_id, season_ccamlr, vessel_name, vessel_nationality_code)
  
  
  ## geo-reference the set start coordinates
  assign("ASDs", CCAMLRGIS::load_ASDs(), envir = globalenv())
  assign("RBs", CCAMLRGIS::load_RBs(), envir = globalenv())
  assign("SSRUs", CCAMLRGIS::load_SSRUs(), envir = globalenv())
  assign("MPAs", CCAMLRGIS::load_MPAs(), envir = globalenv())
  assign("MAs", CCAMLRGIS::load_MAs(), envir = globalenv())
  
  
  assign_areas_temp <- function(aggregation, polys, names_prefix) {
    aggregation <- CCAMLRGIS::assign_areas(
      Input = aggregation,
      NamesIn = c("latitude_set_start", "longitude_set_start"),
      Polys = polys,
      AreaNameFormat = c('GAR_Short_Label'),
      NamesOut = paste0(names_prefix, '_start')
    )
    
    aggregation <- CCAMLRGIS::assign_areas(
      Input = aggregation,
      NamesIn = c("latitude_set_end", "longitude_set_end"),
      Polys = polys,
      AreaNameFormat = c('GAR_Short_Label'),
      NamesOut = paste0(names_prefix, '_end')
    )
    
    return(aggregation)
  }
  
  # RB
  aggregation <- assign_areas_temp(aggregation, 'RBs', 'rb')
  
  # ASD
  aggregation <- assign_areas_temp(aggregation, 'ASDs', 'asd')
  
  # SSRU
  aggregation <- assign_areas_temp(aggregation, 'SSRUs', 'ssru')
  
  # MPAs
  aggregation <- assign_areas_temp(aggregation, 'MPAs', 'mpa')
  
  # MAs
  aggregation <- assign_areas_temp(aggregation, 'MAs', 'ma')
  
  
  
  
  rm("ASDs", "RBs", "SSRUs", "MAs", 'MPAs', envir = globalenv())
  
  
  
  
  # Create a GROUP_ID based on different areas
  
  ## combine if set start is outside a research block, consider set end
  aggregation <- aggregation %>% dplyr::mutate(
    rb = dplyr::case_when(!is.na(rb_start) ~ rb_start,
                   !is.na(rb_end) ~ rb_end),
    asd = dplyr::case_when(!is.na(asd_start) ~ asd_start,
                    !is.na(asd_end) ~ asd_end),
    ssru = dplyr::case_when(!is.na(ssru_start) ~ ssru_start,
                     !is.na(ssru_end) ~ ssru_end),
    mpa = dplyr::case_when(!is.na(mpa_start) ~ mpa_start,
                    !is.na(mpa_end) ~ mpa_end),
    ma = dplyr::case_when(!is.na(ma_start) ~ ma_start,
                   !is.na(ma_end) ~ ma_end))
  
  
  ## create a GROUP_ID variable
  
  aggregation_rb <- aggregation %>% dplyr::mutate(
    GROUP_ID = paste0(season_ccamlr, "_", vessel_name, "_", rb),
    area = rb) # this data frame is for calculation of olap in RBs
  
  aggregation_asd <- aggregation %>% dplyr::mutate(
    GROUP_ID = paste0(season_ccamlr, "_", vessel_name, "_", asd),
    area = asd) # this data frame is for calculation of olap asds
  
  aggregation_ssru <- aggregation %>% dplyr::mutate(
    GROUP_ID = paste0(season_ccamlr, "_", vessel_name, "_", ssru),
    area = ssru) # this data frame is for calculation of olap in ssrus
  
  aggregation_mpa <- aggregation %>% dplyr::mutate(
    GROUP_ID = paste0(season_ccamlr, "_", vessel_name, "_", mpa),
    area = mpa) # this data frame is for calculation of olap in the mpas
  
  aggregation_ma <- aggregation %>% dplyr::mutate(
    GROUP_ID = paste0(season_ccamlr, "_", vessel_name, "_", ma),
    area = ma) # this data frame is for calculation of olap management areas
  
  
  aggregation_882SRU_C_H <- aggregation %>%
    dplyr::filter(ssru %in% c('882C', "882D", "882E", "882F", "882G")) %>%
    dplyr::mutate(ssru = '882SRU_C_G',
                  GROUP_ID = paste0(season_ccamlr, "_", vessel_name, "_", ssru),
                  area = ssru) #  for calculation of combined ssru
  
  
  aggregation_combined <- aggregation %>% dplyr::mutate(
    GROUP_ID = paste0(season_ccamlr, "_", vessel_name, "_", "combined"),
    area = "combined") # this data frame is for calculation of a combined statistic over all areas in the data frame
  
  
  ## combine RB and ASD aggregation in order to calculate overlap statistics for both.
  aggregation = aggregation_rb %>% dplyr::bind_rows(aggregation_asd) %>% dplyr::bind_rows(aggregation_ssru) %>% dplyr::bind_rows(aggregation_combined) %>%
    dplyr::bind_rows(aggregation_mpa) %>%  dplyr::bind_rows(aggregation_ma) %>% dplyr::bind_rows(aggregation_882SRU_C_H) %>% dplyr::filter(!is.na(area))
  
  aggregation <- aggregation %>% dplyr::select(GROUP_ID, obs_logbook_id, c2_id, obs_haul_id, area, asd, season_ccamlr, vessel_name,vessel_nationality_code)
  
  
  rm("aggregation_rb", "aggregation_asd", 'aggregation_ssru', 'aggregation_combined', 'aggregation_ma', 'aggregation_mpa', 'aggregation_882SRU_C_H')
  
  
  
  
  
  # Apply filters to the aggregation data_frame
  if (!is.null(vessels)) {
    aggregation <- dplyr::filter(aggregation, vessel_name %in% vessels)
  }
  
  if (!is.null(seasons)) {
    aggregation <- dplyr::filter(aggregation, season_ccamlr %in% seasons)
  }
  
  if (filter_areas) {
    aggregation <- dplyr::filter(aggregation, area %in% areas)
    if (nrow(aggregation) == 0) {
      stop('No data provided matching the areas filter.')
    }
  }
  
  cat('Overlap statistic calculated for the following areas:', 
      paste(sort(unique(aggregation$area)), collapse = '; '), '\n')
  
  # Only calculate overlap statistic when observer data are available
  NO_OBSERVER_DATA <- dplyr::filter(aggregation, is.na(obs_haul_id)) %>%
    dplyr::distinct(c2_id, season_ccamlr, vessel_name, asd) %>%
    dplyr::group_by(season_ccamlr, vessel_name, asd) %>%
    dplyr::tally()
  
  aggregation <- dplyr::filter(aggregation, !is.na(obs_haul_id))
  
  if (nrow(NO_OBSERVER_DATA) > 0) {
    warning(
      paste('No observer data available for', sum(NO_OBSERVER_DATA$n), 'records from seasons', 
            paste(unique(NO_OBSERVER_DATA$season_ccamlr), collapse = ', '), 'by vessels',
            paste(unique(NO_OBSERVER_DATA$vessel_name), collapse = ', '), 'in areas',
            paste(unique(NO_OBSERVER_DATA$asd), collapse = ', '), 
            '. These records have been excluded from the calculation.')
    )
  }
  
  
  
  
  #######################################
  ###   prepare the original data   #####
  #######################################
  
  ### prepare C2_CATCH dataframe
  
  C2_CATCH <- dplyr::distinct(aggregation, c2_id, obs_haul_id) %>%
    dplyr::inner_join(C2_CATCH, by = 'c2_id', multiple = "all") %>%
    dplyr::filter((taxon_code %in% taxon)) %>%
    dplyr::select(
      c2_id,
      obs_haul_id,
      taxon_code,
      greenweight_caught_kg,
      individualcount_caught,
      individualcount_released_withtags
    )
  
  C2_CATCH_TOTAL <- C2_CATCH %>%
    dplyr::group_by(obs_haul_id, taxon_code) %>%
    dplyr::summarise(
      catch_n = sum(individualcount_caught, na.rm = TRUE),
      .groups = "drop"
    )  # Addresses QC issue of multiple records of the same taxon on the same haul.
  
  
  ### prepare HAUL_BIOLOGY dataframe
  
  # Filter and select relevant columns in HAUL_BIOLOGY
  HAUL_BIOLOGY <- OBS_HAUL_BIOLOGY %>%
    dplyr::filter(taxon_code %in% taxon, obs_haul_id %in% aggregation$obs_haul_id) %>%
    dplyr::select(obs_haul_id, taxon_code, length_total_cm)
  
  # Identify and handle too small fish
  too_small_fish <- HAUL_BIOLOGY %>% dplyr::filter(length_total_cm < 10)
  
  if (nrow(too_small_fish) > 0) {
    warning(
      paste('There are', nrow(too_small_fish), 'records in the dataframe OBS_HAUL_BIOLOGY with fish lengths smaller than 10 cm which have been excluded.')
    )
    
    # Exclude too small fish from HAUL_BIOLOGY
    HAUL_BIOLOGY <- HAUL_BIOLOGY %>% dplyr::filter(length_total_cm >= 10)
  }
  
  
  ### prepare TAG_RELEASE dataframe
  
  # Filter and select relevant columns in TAG_RELEASE
  TAG_RELEASE <- OBS_HAUL_TAG_RELEASE %>%
    dplyr::filter(taxon_code %in% taxon, obs_haul_id %in% aggregation$obs_haul_id) %>%
    dplyr::select(obs_haul_id, taxon_code, length_total_cm)
  
  # Identify and handle too small fish
  too_small_fish <- TAG_RELEASE %>% dplyr::filter(length_total_cm < 10)
  
  if (nrow(too_small_fish) > 0) {
    warning(
      paste('There are', nrow(too_small_fish), 'records in the dataframe TAG_RELEASE with fish lengths smaller than 10 cm which have been excluded.')
    )
    
    # Exclude too small fish from TAG_RELEASE
    TAG_RELEASE <- TAG_RELEASE %>% dplyr::filter(length_total_cm >= 10)
  }
  
  
  
  ###########################################
  ### prepare TAG_RECAPTURE   dataframe  ####
  ###########################################
  
  TAG_RECAPTURE <-
    OBS_HAUL_TAG_RECAPTURE %>%
    dplyr::filter(taxon_code %in% taxon,obs_haul_id %in% aggregation$obs_haul_id) %>%
    dplyr::select(obs_haul_id, taxon_code)
  
  
  
  ###########################################
  ###      FILTERS                       ####
  ###########################################
  
  # Keep only sets with data
  aggregation <- aggregation %>%
    dplyr::filter(
      c2_id %in% C2_CATCH$c2_id |
        obs_haul_id %in% unique(c(TAG_RELEASE$obs_haul_id, TAG_RECAPTURE$obs_haul_id, HAUL_BIOLOGY$obs_haul_id))
    )
  
  # QC issue - lengths are reported but no catch - issue for up scaling catch
  missing_catch_biology <- aggregation %>%
    dplyr::filter(
      obs_haul_id %in% HAUL_BIOLOGY$obs_haul_id,
      !c2_id %in% C2_CATCH$c2_id
    )
  
  # QC issue - tag is released but no catch is reported
  missing_catch_tagged <- aggregation %>%
    dplyr::filter(
      obs_haul_id %in% TAG_RELEASE$obs_haul_id,
      !c2_id %in% C2_CATCH$c2_id
    )
  
  # Keep measured fish with associated catch (no up-scaling is possible otherwise)
  HAUL_BIOLOGY <- HAUL_BIOLOGY %>%
    dplyr::filter(obs_haul_id %in% C2_CATCH$obs_haul_id)
  
  
  
  
  #####################################
  ###  calculate tag rates  ###########
  #####################################
  
  tag_rate <- aggregation %>%
    dplyr::left_join(
      C2_CATCH %>% dplyr::select(-obs_haul_id),
      by = 'c2_id'
    ) %>%
    dplyr::mutate(taxon_code = taxon) %>%
    dplyr::left_join(
      TAG_RECAPTURE %>% 
        dplyr::group_by(obs_haul_id, taxon_code) %>% 
        dplyr::tally(),
      by = c("obs_haul_id", "taxon_code")
    ) %>%
    dplyr::left_join(
      TAG_RELEASE %>% 
        dplyr::group_by(obs_haul_id, taxon_code) %>% 
        dplyr::tally(),
      by = c("obs_haul_id", "taxon_code"),
      suffix = c("", "_tagg")
    ) %>%
    dplyr::left_join(
      HAUL_BIOLOGY %>% 
        dplyr::group_by(obs_haul_id, taxon_code) %>% 
        dplyr::tally(),
      by = c("obs_haul_id", "taxon_code"),
      suffix = c("", "_measured")
    ) %>%
    dplyr::group_by(
      GROUP_ID, AREA = area, SEASON = season_ccamlr, SHIP_NAME = vessel_name, NATIONALITY_CODE = vessel_nationality_code, TAXON = taxon_code
    ) %>%
    dplyr::summarise(
      LOGBOOK_ID = paste0(unique(obs_logbook_id), collapse = " , "),
      CATCH_TONNES = round(sum(greenweight_caught_kg, na.rm = TRUE) / 1000, 1),
      CATCH_N = sum(individualcount_caught, na.rm = TRUE),
      MEASURED_N = sum(n_measured, na.rm = TRUE),
      TAGGED_N = sum(n_tagg, na.rm = TRUE),
      RECAPTURED_N = sum(n, na.rm = TRUE),
      TAG_RATE = round(TAGGED_N / CATCH_TONNES, 1),
      .groups = "drop"
    )
  
  
  
  ##########################################
  ### calculation of overlap statistics ####
  ##########################################
  
  # Calculation of overlap statistics
  # Set parameters for loop
  breaks <- seq(9.999, 260, 10)  # Set the bins for the overlap statistic.
  
  # Place holders
  tagged_ld <- NULL
  caught_ld_g <- 0
  caught_ld <- NULL
  
  # Calculate frequencies per grouping
  for (group in unique(aggregation$GROUP_ID)) {
    hauls_includes <- aggregation %>% dplyr::filter(GROUP_ID == group)
    
    # Calculate per haul the length distribution for the total catch
    for (current_haul in unique(hauls_includes$obs_haul_id)) {
      catch_n <- (C2_CATCH_TOTAL %>% dplyr::filter(obs_haul_id %in% current_haul))$catch_n
      lengths_current_set <- HAUL_BIOLOGY %>% dplyr::filter(obs_haul_id == current_haul)
      measured_n <- length(lengths_current_set$length_total_cm)
      
      if (measured_n > 0) {
        set_lf <- graphics::hist(lengths_current_set$length_total_cm, breaks = breaks, plot = FALSE)
        
        if (length(catch_n) > 0) {
          caught_ld_h <- set_lf$counts * catch_n / measured_n
        } else {
          caught_ld_h <- set_lf$counts
        }
        
        caught_ld_g <- caught_ld_g + caught_ld_h
      }
    }
    
    # Calculate length distribution for released fish
    lengths_tag <- TAG_RELEASE %>% dplyr::filter(obs_haul_id %in% hauls_includes$obs_haul_id)
    tag_lf <- graphics::hist(lengths_tag$length_total_cm, breaks = breaks, plot = FALSE)
    
    caught_ld_g <- caught_ld_g + tag_lf$counts
    caught_ld_g <- data.frame(counts = caught_ld_g, fraction = c(1:25), GROUP_ID = group)
    caught_ld <- dplyr::bind_rows(caught_ld, caught_ld_g)
    caught_ld_g <- 0
    tagged_ld_g <- data.frame(GROUP_ID = group, counts = tag_lf$counts, fraction = c(1:25))
    tagged_ld <- dplyr::bind_rows(tagged_ld, tagged_ld_g)
  }
  
  rm('catch_n', 'current_haul', 'lengths_current_set', 'caught_ld_g', 'caught_ld_h', 'group',
     'hauls_includes', 'set_lf', 'tag_lf', 'lengths_tag', "breaks", "measured_n")
  
  # Create frequency bins
  caught_freq <- caught_ld %>%
    dplyr::group_by(GROUP_ID) %>%
    dplyr::mutate(caught_freq = counts / sum(counts, na.rm = TRUE))
  
  tagged_freq <- tagged_ld %>%
    dplyr::group_by(GROUP_ID) %>%
    dplyr::mutate(tagged_freq = counts / sum(counts, na.rm = TRUE))
  
  # Calculate the overlap statistic
  olap <- caught_freq %>%
    dplyr::inner_join(tagged_freq, by = c("GROUP_ID", "fraction")) %>%
    dplyr::mutate(difference = abs(caught_freq - tagged_freq)) %>%
    dplyr::group_by(GROUP_ID) %>%
    dplyr::summarise(TAG_OVERLAP = round((1 - sum(difference, na.rm = TRUE) / 2) * 100, 2),
                     .groups = "keep")
  
  
  
  
  ##################################################
  ### Combine tag rates and overlap statistic ######
  ##################################################
  
  summary <- tag_rate %>% dplyr::left_join(olap, by = c("GROUP_ID"))
  
  if(CCEP){
    summary <- summary  %>%  dplyr::mutate(TAG_OVERLAP = ifelse(TAGGED_N < 30, "NC", as.character(TAG_OVERLAP)))
  }
  
  
  ##################################################
  ###   Plot charts of overlap statistic      ######
  ##################################################
  if(plot) {
    
    plots <- list()
    
    for (group in unique(olap$GROUP_ID)) {
      # Filter the relevant data
      caught_freq_g <- dplyr::filter(caught_freq, GROUP_ID == group) %>% 
        dplyr::mutate(type = 'caught', freq = caught_freq)
      
      tagged_freq_g <- dplyr::filter(tagged_freq, GROUP_ID == group) %>% 
        dplyr::mutate(type = 'released', freq = tagged_freq)
      
      # Obtain values for the title of the chart
      olap_g <- dplyr::filter(olap, GROUP_ID == group)
      group_info <- dplyr::filter(tag_rate, GROUP_ID == group)
      plot_title <- sprintf(
        'Vessel %s in season %s in area %s had an overlap statistic of %s for %s',
        group_info$SHIP_NAME, group_info$SEASON, group_info$AREA,
        olap_g$TAG_OVERLAP, group_info$TAXON
      )
      
      # Set boundaries for the X axis of the plot.
      df <- dplyr::bind_rows(caught_freq_g, tagged_freq_g)
      
      caught_n = sum(caught_freq_g$counts)
      
      max <- (df %>% 
        dplyr::filter(counts > 0) %>% 
        dplyr::summarise(max = max(fraction)))$max + 4
      
      min <- (df %>% 
        dplyr::filter(counts > 0) %>% 
        dplyr::summarise(min = min(fraction)))$min - 4
      
      df <- dplyr::filter(df, fraction > min & fraction < max) %>% 
        dplyr::mutate(xlab = paste0(fraction * 10, " - ", (fraction + 1) * 10),
                      xlab = factor(xlab, levels = unique(xlab)))
      
      
      # Generate plot
      plots[[group]] <- ggplot(data = df, aes(x = xlab, y = freq, group = type)) +
        geom_line(aes(color = type)) +
        scale_colour_discrete(
          labels = c(
            "released" = sprintf('released (n = %d)', group_info$TAGGED_N),
            "caught" = sprintf('caught (n = %d)', caught_n)
          ),
          limits = c("released", "caught")
        ) +
        geom_point() +
        labs(
          title = plot_title,
          x = "length class (cm)",
          y = "proportion  of fish at length"
        ) +
        theme_minimal() +
        theme(
          legend.position = c(0.9, 0.8),
          legend.text = element_text(size = 14),
          legend.title = element_blank(),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 80, vjust = 0.5, hjust = 0.2),
          axis.title = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
        )
    }
    
    out <- list()
    out$summary <- summary
    out$plots <- plots

    return(out)
    
  } else {  # when plot = FALSE
    
    return(summary)
    
  }
}


