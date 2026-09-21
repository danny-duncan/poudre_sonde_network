## Determining uploads

#This function looks at the user inputs for calibration report collect and logs collected.
#Based on these inputs, it looks at all the uploaded logs or calibration reports
#and will print out what logs are missing and who to contact to get those files uploaded.

#' @title files_missing
#' @description This function looks through all the calibration reports,
#' @param field_notes A dataframe containing field notes with columns: site, crew, start_DT,end_dt, cal_report_collected, cals_performed, log_downloaded, log1_type,log1_mmdd,  log2_type, log2_mmdd
#' @param sonde_tracking_file_path A string filepath to the current file where sonde info is tracked. This is an .xslx file with the sheet "station_info" where we will update the sonde numbers,
#' and the sheet "ownership" where we will look for sensors that are not to be included in the current sensor deployments.
#' @param cal_report_file_path (optional) A string filepath to the folder where calibration reports are stored. Default is here("data", "raw", "calibration_reports")
#' @param log_dwnl_data_file_path (optional) A string filepath to the folder where sensor logs are stored. Default is here("data", "raw", "sensor_data", year(Sys.Date()))
#' @param logical A boolean indicating whether to return a logical vector of missing files (default is FALSE, which only prints to console)
#'@example
#' source("src/files_missing")
#' files_missing(field_notes = all_notes_cleaned,
#'               sonde_tracking_file_path = "data/metadata/2025_sensor_tracking.xlsx")
#'
#' file_check <- files_missing(field_notes = all_notes_cleaned,
#'               sonde_tracking_file_path = "data/metadata/2025_sensor_tracking.xlsx",
#'               logical = T)

files_missing <- function(field_notes, sonde_tracking_file_path, cal_report_file_path = here("data", "raw","sensor", "calibration_reports"),
                          log_dwnl_data_file_path  = here("data", "raw", "sensor", "log_download"), logical = F){

  `%nin%` = Negate(`%in%`)

  field_season <- year(Sys.Date())

  #grab cal reports from folder
  cal_reports_simple <- str_extract(list.files(path = cal_report_file_path), ".*_\\d{8}" )%>%tolower()
  logs_simple <- str_extract(list.files(path = here(log_dwnl_data_file_path, field_season), recursive = TRUE), "\\w+_\\d{8}_(vulink|troll)")%>%tolower()


  # Find relevant sonde SN
  active_sns <- readxl::read_xlsx(here(sonde_tracking_file_path), sheet = "station_info")%>%
    select(site = site_code, sn = current_sonde_sn)%>%
    filter(!is.na(sn))%>%
    mutate(site = tolower(site))

  #Sites to ignore for the Loveland Project
  loveland_sites <- c("grgr_catwalk", "canal", "big_thompson")

  #grab sensor notes that have logs or cal reports that should be  downloaded
  sensor_files <- field_notes%>%
    filter(year(DT_round) == field_season)%>%
    filter(!site %in% loveland_sites)%>%
    filter(grepl("Sensor",visit_type, ignore.case = TRUE))%>%
    filter(cal_report_collected|log_downloaded)%>%
    select(site, crew, start_DT,end_dt, cal_report_collected, cals_performed, log_downloaded, log1_type,log1_mmdd,  log2_type, log2_mmdd)%>%
    mutate(
      #make all site names lower
      site = tolower(site),
      # Create basis for calibration report name
      # this will be used to check for calibration report in data files and then b
      cal_report_name = case_when(cal_report_collected == TRUE ~ paste0(site, "_", format(start_DT, "%Y%m%d")),
                                  cal_report_collected == NA ~ NA),
      full_cal_name = case_when(cal_report_collected == TRUE ~ paste0(site, "_", format(end_dt, "%Y%m%d_%H%M_mst")),
                                cal_report_collected == NA ~ NA),
      log1_mmdd = case_when(nchar(as.character(log1_mmdd)) == 3 ~ paste0("0",log1_mmdd),
                            TRUE ~ as.character(log1_mmdd)),
      log1_type = case_when( grepl("aquatroll", log1_type,ignore.case = TRUE) ~  "troll",
                             grepl("at", log1_type,ignore.case = TRUE) ~  "troll",
                             grepl("aqua troll", log1_type,ignore.case = TRUE) ~  "troll",
                             grepl("vulink", log1_type,ignore.case = TRUE) ~  "vulink",
                             grepl("vu link", log1_type,ignore.case = TRUE) ~  "vulink",
                             TRUE ~ tolower(log1_type)),
      log2_type = case_when( grepl("aquatroll", log2_type,ignore.case = TRUE) ~  "troll",
                             grepl("at", log2_type,ignore.case = TRUE) ~  "troll",
                             grepl("aqua troll", log2_type,ignore.case = TRUE) ~  "troll",
                             grepl("vulink", log2_type,ignore.case = TRUE) ~  "vulink",
                             grepl("vu link", log2_type,ignore.case = TRUE) ~  "vulink",
                             TRUE ~ tolower(log2_type)),
      log2_mmdd = case_when(nchar(as.character(log2_mmdd)) == 3 ~ paste0("0",log2_mmdd),
                            TRUE ~ as.character(log2_mmdd)),
      log1_user_error = case_when( is.na(log_downloaded)~ FALSE,
                                   log_downloaded == FALSE ~ FALSE,
                                   is.na(log1_mmdd) | is.na(log1_type) ~ TRUE,
                                   TRUE ~ FALSE ),
      log2_user_error = case_when(is.na(log_downloaded)~ FALSE,
                                  log_downloaded == FALSE ~ FALSE,
                                  is.na(log2_mmdd) & is.na(log2_type) ~ FALSE,
                                  is.na(log2_mmdd) | is.na(log2_type) ~ TRUE,
                                  TRUE ~ FALSE),
      log1_name = case_when(!(is.na(log1_mmdd) | is.na(log1_type)) ~ paste0(site,"_",year(start_DT),log1_mmdd,
                                                                            "_", format(start_DT, "%Y%m%d"), "_", log1_type),
                            (is.na(log1_mmdd) | is.na(log1_type))  ~ NA),
      log2_name = case_when(!(is.na(log2_mmdd) | is.na(log2_type)) ~ paste0(site,"_",year(start_DT),log2_mmdd,
                                                                            "_", format(start_DT, "%Y%m%d"), "_", log2_type),
                            (is.na(log2_mmdd) | is.na(log2_type))  ~ NA),
      log_missing1 = case_when(
        is.na(log_downloaded)| log_downloaded == FALSE ~ FALSE,
        is.na(log1_name) ~ FALSE,
        log1_name %nin% logs_simple ~ TRUE,
        TRUE ~ FALSE
      ),
      log_missing2 = case_when(
        is.na(log_downloaded)| log_downloaded == FALSE ~ FALSE,
        is.na(log2_name) ~ FALSE,
        log2_name %nin% logs_simple ~ TRUE,
        TRUE ~ FALSE
      ),
      log_missing = case_when(
        log_missing1 | log_missing2 ~ TRUE,
        TRUE ~ FALSE
      ),
      cal_missing = case_when(
        is.na(cal_report_collected) ~ FALSE,
        cal_report_name %nin% cal_reports_simple ~ TRUE,
        TRUE ~ FALSE)
      )%>%
    left_join(active_sns, by = "site")

missing_files <- F

  for (i in 1:nrow(sensor_files)) {

    #if log missing print out missing logs or cal reports
    if(sensor_files$log_missing[i]){
      cat("\nLog Missing: ", sensor_files$log1_name[i], " and/or ", sensor_files$log2_name[i], "\nContact: ", sensor_files$crew[i], "\n")
      missing_files <- T

    }
    #if log missing print out missing logs or cal reports
    if(sensor_files$cal_missing[i]){
      cat("\nCal Missing: ", sensor_files$full_cal_name[i],"\nSonde SN: ", sensor_files$sn[i], " \nContact: ", sensor_files$crew[i], "\n")
      missing_files <- T
    }


  }


  if(logical == T){
    return(missing_files)
  }else{
    if(missing_files == F){
      cat("\nFile Check Complete, no missing files were found\n")
    }
  }


}

