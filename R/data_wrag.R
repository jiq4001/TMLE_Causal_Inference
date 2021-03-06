#'
#'@description perform wrangling
#'@parma unit: int default to 30 (days). convert date to month interval
#'@parma int_tp: int, interval between nodes (months) 
#'@parma max_censor: costum censoring time (month), use large number for administritive censoring
#'@parma med_to_check: a string, treatment (which medication) to evaluate
#'@parma cwd inherit work directory passed by warraper function
#'@export dataframe daata used to pass to tmle() in txt format
#'@export image event-time/event-count histograph
#'@export image dynamic treatment regime
#'@depends tidyverse, magrittr, janitor, mice
#'
#library(tidyverse)
#library(magrittr)
#library(janitor)
#library(mice)
#
HMIII_data_wrag <- function(unit = 30, ..., #days/  date to month
                            int_tp, #= 3, #month/ between nodes
                            max_censor,# = 12,
                            med_to_check, 
                            cwd){ 

  if(is.null(int_tp) | is.null(max_censor) | is.null(med_to_check)){
    stop("needs nodes interval and max_censoring frame and treatment.\n")
  }
    
  my_plot_theme <- function () {
    theme_classic() %+replace% 
      theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
            plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
            legend.position = "bottom",
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black", size = rel(1)), legend.key = element_blank(), 
            strip.background = element_rect(fill = "white", colour = "black", size = rel(2)), 
            complete = TRUE)
  }
  #----------readin dataset--------------
  med1 <- readxl::read_xlsx(paste(cwd, "Data/Raw/BaselineHM3Labs.discharge.xlsx", sep = "/"), 
                            sheet = 1) %>% 
    clean_names() %>% 
    filter(!is.na(id)) %>%
    dplyr::mutate(visit_date = as.Date(visit_date))
  add_1 <- readxl::read_xlsx(paste(cwd, "Data/Raw/BaselineHM3Labs.discharge.xlsx", sep = "/"), 
                             sheet = 2) %>% 
    clean_names() %>% 
    filter(!is.na(id)) %>%
    dplyr::mutate(discharge_date = as.Date(discharge_date))
  
  ques1 <- readxl::read_xlsx(paste(cwd, "Data/Raw/medication spreadsheet 3.14 HM3 only deidentified.xlsx", sep = "/"),
                             sheet = 2) %>% 
    clean_names() %>%
    dplyr::mutate(admitted = as.Date(admitted),
                  implant_date = as.Date(implant_date),
                  discharge_date = as.Date(discharge_date),
                  mcs_start_date = as.Date(as.numeric(mcs_start_date), origin = "1899-12-30"),
                  x1st_device_exchange_date = as.Date(as.numeric(x1st_device_exchange_date), origin = "1899-12-30"),
                  outcome_date = as.Date(outcome_date),
                  gib_date = as.Date(as.numeric(gib_date), origin = "1899-12-30")) %>%
    dplyr::mutate_if(is.character, tolower) %>%
    dplyr::mutate(race = case_when(hispanic_yes_no == "yes" ~ "hisp",
                                   race_afroamerican == "yes" ~ "afam",
                                   race_white == "yes" ~ "white",
                                   race_asian == "yes" ~ "asian",
                                   TRUE ~ "others")) %>%
    merge.data.frame(add_1, by = c("id", "discharge_date"))
  
  ques1$idx <- 1: nrow(ques1) # simple index
  lab1 <- readxl::read_xlsx(paste(cwd, "Data/Raw/BaselineHM3Labs.discharge.xlsx", sep = "/"), 
                            sheet = 3) %>% 
    clean_names() %>%
    select(-visit_n, -visit_date_meds, -comments) %>%  # only keep lab data for relationship table
    filter(!is.na(ideal_date_labs)) %>%
    dplyr::mutate(ideal_date_labs = as.Date(as.numeric(ideal_date_labs), origin = "1899-12-30"),
                  id = factor(id)) %>%
    dplyr::mutate_if(is.character, as.numeric) 
  ques1 %>%
    select(id, idx, discharge_date, outcome_date, final_outcome_reason, ever_gib_yes_1_no_0, gib_date) %>%
    dplyr::mutate(month_to_outcome = ceiling(as.numeric(difftime(outcome_date, discharge_date)/unit)),
                  max_censor = min(max_censor, max(month_to_outcome)),
                  month_to_death = ifelse(final_outcome_reason == "expired", month_to_outcome, NA),
                  month_to_censored_d = ifelse(!is.na(month_to_death) & (month_to_outcome < max_censor), NA, pmin(max_censor, month_to_outcome)),
                  month_to_gib = ceiling(as.numeric(difftime(gib_date, discharge_date)/unit)),
                  month_to_gib_or_death = ifelse(final_outcome_reason == "expired" | (ever_gib_yes_1_no_0 == 1) , pmin(month_to_outcome, month_to_gib, na.rm = T), NA),
                  month_to_gib_or_death = ifelse(month_to_gib_or_death <= 0 | month_to_gib_or_death > max_censor, NA, month_to_gib_or_death),
                  month_to_DorB_censored = ifelse(!is.na(month_to_gib_or_death) & (month_to_gib_or_death < max_censor), NA, pmin(max_censor, month_to_outcome))) -> ques2
  #----------event plot--------------  
  p1 <- ggplot(data.frame(table(ques2$month_to_gib_or_death))%>%
           rename(event_time = Var1, event_count = Freq))+
    geom_bar(aes(event_time, event_count), stat="identity")+
    labs(title = "Histogram of GIB/Death occurrence frequence over customed\n censoring-node frame",
         x = "follow-up time")+
    my_plot_theme()
  event2nd <- 
    ques2%>%
    select(idx, month_to_DorB_censored, month_to_gib_or_death)%>%
    mutate(nodes = pmin(month_to_DorB_censored, month_to_gib_or_death, na.rm = T),
           nodes = abs(ceiling( nodes/int_tp)))%>%
    mutate(month_to_DorB_censored = abs(month_to_DorB_censored),
           month_to_gib_or_death = abs(month_to_gib_or_death)) %>% #### temporary fix #####
  filter(nodes > 0)
  
  
  timeframe <- expand.grid(idx = event2nd$idx, 
                           nodes = 1:((ques2$max_censor)/int_tp)) %>% 
    merge.data.frame(event2nd, all.x = T)%>%
    mutate(event = month_to_gib_or_death,
           censor = month_to_DorB_censored)%>%
    group_by(idx)%>%
    arrange(idx, nodes)%>%
    fill(event, censor, .direction = "down")%>%
    mutate(event = ifelse(!is.na(event), 1, 0),
           censor = ifelse(!is.na(censor) & event == 0, 1, 0 ))
  #----------baseline vars--------------
  W <- ques1[, c(4:7, 9, 10, 11:19, 25, 31:34, 38:46, 50:57)] %>% 
    mutate_if(is.character, as.factor)
  
  W %>%
    mutate(sex_m_male = factor(ifelse(sex_m_male %in% c("f", "female"), "f", "m")),
           strategy = factor(ifelse(strategy != "dt", "other", "dt")),
           race = factor(ifelse(race != "afam", "other", "afam")))%>%
    select(-implant_date, -device_type, -dialysis_pre_vad_1_yes_0_no,  -x1st_device_exchange) -> W  ### needs to decide Var with multi categories
  W <- complete(mice(W, seed = 11))
  W$albumin_g_d_l  <- as.numeric(W$albumin_g_d_l)
  W$tbili  <- as.numeric(W$tbili)
  W$total_los_includes  <- as.numeric(W$total_los_includes)
  ques2 %>%
    select(id, idx, discharge_date) %>%
    merge.data.frame(med1, by = "id")%>%
    merge.data.frame(lab1, by.x = c("id", "visit_date"), by.y = c("id", "ideal_date_labs"),
                     all.x = T, all.y = T)%>%     # join observed med, lab, with baseline table require the discharge date
    group_by(id) %>%
    arrange(id, visit_date) %>%
    fill(-id, .direction = "down") %>% # unobserved info if filled by last visit 
    unique()%>%
    merge.data.frame(med1 %>% select(id, visit_date), by = c("id", "visit_date"), all.y = T)%>%
    group_by(id) %>%
    arrange(id, visit_date) %>%
    mutate(med_date_update = visit_date, # lag(visit_date, default = unique(discharge_date)), no use for now   
           days_to_visit = as.numeric(difftime(visit_date, discharge_date)),
           month_to_visit = ceiling(days_to_visit/unit),
           visits_int = (days_to_visit - lag(days_to_visit, default = 0))/unit)%>%
    filter(month_to_visit <= min(max_censor, max(ques2$max_censor)))-> med2
  
  med2%>%
    #mutate_if(is.numeric, funs(replace_na(., 0)))
    mutate_if(is.character, as.numeric)%>% 
    mutate(i_aspirin = aspirin * visits_int,  # cumulated binary med
           i_dipyridamole = dipyridamole * visits_int,
           i_bb = bb_y_n * visits_int,
           i_ace = ace_y_n * visits_int,
           i_arb = arb_y_n * visits_int,
           i_ccb = ccb_y_n * visits_int,
           i_aldo = aldo_b_y_n * visits_int,
           i_pde5i = pde5i_y_n * visits_int,
           i_loop_d = loop_d_y_n * visits_int,
           i_thiaz_d = thiaz_d_y_n * visits_int,
           i_statin = statin_y_n * visits_int,
           i_amiodarone = amiodarone_y_n * visits_int,
           i_digoxin = digoxin_y_n * visits_int,
           i_hydralazine = hydralazine * visits_int,
           i_ace_arb = ifelse(!is.na(ace_y_n) | !is.na(arb_y_n), 1, NA),
           i_ace_arb = i_ace_arb * visits_int,
           ## cumumlated dose
           d_aspirin = i_aspirin,
           d_dipyridamole = i_dipyridamole,
           d_bb = i_bb * bb_dosage,
           d_ace = ifelse(!is.na(i_ace), i_ace * ace_dosage, 0),
           d_arb = ifelse(!is.na(i_arb), i_arb * arb_dosage, 0),
           d_ccb = i_ccb * ccb_dosage ,
           d_aldo = i_aldo * aldo_b_dosage ,
           d_pde5i = i_pde5i * pde5i_dosage ,
           d_loop_d = i_loop_d * loop_d_dosage ,
           d_thiaz_d = i_thiaz_d * thiaz_d_dosage ,
           d_statin = i_statin * statin_dosage ,
           d_amiodarone = i_amiodarone * amiodarone_dosage ,
           d_digoxin = i_digoxin * digoxin_dosage ,
           d_hydralazine = i_hydralazine * hydralazine,
           d_ace_arb = sum(d_ace + d_arb), 
           nodes = ceiling(month_to_visit / int_tp))-> med3
  Med <- med3[, c(3, 60:90 )]%>%
    filter(nodes > 0) %>%   
    merge.data.frame(timeframe %>% select(idx, nodes), by = c("idx", "nodes"), all.y = T)%>%  # merge with final timeframe 
    group_by(idx, nodes)%>%
    arrange(idx, nodes)%>%
    summarise_all(sum)
  #----------all treatment binary--------------
  A <- med3[, c(3, 90, grep("^i_", colnames(med3)))]%>%
    merge.data.frame(timeframe %>% select(idx, nodes), by = c("idx", "nodes"), all.y = T)%>%
    group_by(idx, nodes)%>%
    summarise_all(sum)%>%
    group_by(idx, nodes)%>% # set key wont be convert
    mutate_all(funs(ifelse(!is.na(.) & . > 0, 1, 0)))%>% # if ever prescribed as Yes pair with dose data
    #mutate_all(funs(ifelse(!is.na(.) & . > 0.5*int_tp, 1, 0)))%>% # med prescription > 0 as Yes, L matrix will use cumulated incoperate duration
    group_by(idx)%>%
    arrange(nodes)%>%
    mutate(nodes = lag(nodes, default = 0)) %>%
    filter((nodes!=max(event2nd$nodes)))%>%
    arrange(idx)
  #----------all lab L--------------
  Lab<- med3[, c(3, 90, 49:55)]%>%
    merge.data.frame(timeframe %>% select(idx, nodes), by = c("idx", "nodes"), all.y = T)%>%
    group_by(idx, nodes)%>%
    arrange(idx, nodes)%>%
    #fill(everything(), .direction = "up")%>%
    fill(-idx, -nodes, .direction = "down")%>%
    mutate(lastlab = 1:n())%>%
    filter(lastlab == max(lastlab))%>%
    select(-lastlab)
  col_text <- paste0(med_to_check, " Prescription")
  title_text <- paste0(med_to_check, " Treatment Timeline")
  sub_text <- "Event: Death"
  sub_text <- "Event: Death or GIB"
  #----------image dinamic treatment regimes--------------  
  p2 <- event2nd %>% 
    merge.data.frame(A[, c("idx", "nodes", med_to_check)], by = c("idx", "nodes"), all = T) %>%
    group_by(idx)%>%
    arrange(idx, nodes)%>%
    fill(-idx, .direction = "down")%>%
    mutate(censored_death = lag(ifelse(!is.na(month_to_DorB_censored)|!is.na(month_to_gib_or_death), 1, NA)),
           Event_circle = ifelse(!is.na(month_to_gib_or_death), nodes, NA))%>%
    filter(is.na(censored_death))%>%
    ggplot(aes(nodes, idx, group=idx, col=factor(get(med_to_check)))) +
    geom_line(size=.4) +
    geom_point(shape=15, size= 1) +
    geom_point(aes(Event_circle), col="black", shape=4, size=2) +
    theme_classic() + 
    scale_x_continuous(expand=c(0,0.1), breaks = seq(0, max_censor/int_tp), labels = seq(0, max_censor, by = int_tp)) +
    scale_y_continuous(expand=c(0.01,0.01)) +
    labs(x="Months since Discharge", y="Patient ID", col=col_text, title=title_text, subtitle = sub_text) +
    scale_color_manual(labels=c("No","Yes"), values=c("#FFC72C","#B31B1B")) +
    theme(legend.position = "bottom",text=element_text(size=10),
          legend.box.background = element_rect(color = "black"),
          legend.background = element_blank(),
          plot.title = element_text(hjust = 0.5))
  #----------output folder--------------
  result_path <- paste(cwd, "result/", sep = "/")
  
  pdf(paste0(result_path, title_text, "max_censoring", max_censor, "at", int_tp, ".pdf"))
  print(p1)
  print(p2)
  dev.off()
  #----------pull treatment of interest-------------- 
  trt.matrix <- A%>%
    data.frame()%>%
    select(idx, nodes, med_to_check)%>%
    reshape(v.names = med_to_check, idvar = "idx", timevar = "nodes", direction = "wide")%>%
    mutate_all(list(~replace_na(.,0))) 
  colnames(trt.matrix) <- c("PatientID", paste("A_", c(0: (max(event2nd$nodes)-1)), "_", sep = ""))
  #----------L--------------
  L <- merge.data.frame(Med, Lab)
  L <- L[, -c(grep("^i_", colnames(L)))]
  Lnode.list <- list()
  for (Var in 3:ncol(L)) {
    i <- colnames(L)[Var]
    Lnode.list[[i]]<- 
      data.frame(L[, c("idx", "nodes", i)])%>%
      reshape(v.names = i, idvar = "idx", timevar = "nodes", direction = "wide")%>%
      mutate_all(list(~replace_na(.,0)))
    colnames(Lnode.list[[i]]) <- c("PatientID", paste(paste0("L_", Var), 1:max(event2nd$nodes), "_", sep = "_"))
    Lnode.list[[i]] <- Lnode.list[[i]][,-1]
  }
  L.matrix <- do.call(cbind, Lnode.list)
  colnames(L.matrix) <- gsub("^.*L", "L", colnames(L.matrix), perl = T)
  L.matrix$PatientID <- trt.matrix$PatientID
  #----------C--------------  
  c.matrix <- timeframe%>%
    data.frame()%>% 
    dplyr::select(idx, nodes, censor)%>%
    reshape(v.names = "censor", idvar = "idx", timevar = "nodes", direction = "wide")
  colnames(c.matrix) <- c("PatientID", paste("C_", 1:max(event2nd$nodes), "_", sep = ""))
  #----------Y--------------  
  y.matrix <- timeframe %>%
    dplyr::select(idx, nodes, event)%>%
    data.frame()%>%
    reshape(v.names = "event", idvar = "idx", timevar = "nodes", direction = "wide")%>%
    mutate_all(funs(ifelse(is.na(.), 0, .)))
  colnames(y.matrix) <- c("PatientID", paste("Y_", 1:max(event2nd$nodes), "_", sep = ""))
  #----------final df var sequence C-L-A-Y--------------
    CLAY <- merge.data.frame(c.matrix, L.matrix, by = "PatientID")%>%
    merge.data.frame(trt.matrix, by = "PatientID")%>%
    merge.data.frame(y.matrix, by = "PatientID")
  Temp.CLAY <- CLAY[, c(1, grep("*_0_", colnames(CLAY)))]
  for (i in 1 : (ncol(y.matrix)-1)) {
    pat <- paste(".*_", i, "_", sep = "")
    Temp.CLAY <- cbind.data.frame(Temp.CLAY, CLAY[, c(grep(pattern = pat, colnames(CLAY)))])
  }
  finaldf <- W%>%
    merge.data.frame(Temp.CLAY, by.x = "idx", by.y = "PatientID")%>%
    dplyr::select(-idx)
  
  write_delim(finaldf, paste0(result_path, title_text, "max_censoring", max_censor, "at", int_tp, ".txt"))
  return(finaldf)
}