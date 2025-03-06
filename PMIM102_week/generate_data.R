generate_data <- function(population_size = 1000,
                          number_of_pc_records = 100000,
                          number_of_ed_records = 5000,
                          number_of_ha_records = 1000,
                          start_dt = as.Date('2000/01/01'), 
                          end_dt = as.Date('2019/12/31')
                          ){
  set.seed(0)
  
  # Generate healthcare registration table =================
  hc_reg = data.frame(
    ID = 1:1000, 
    START_DT = sample(seq(start_dt, end_dt, by="day"), population_size)
  )
  hc_reg$END_DT = hc_reg$START_DT + sample.int(365*20, population_size, replace=TRUE)
  # Truncate end dates to 2023/12/31
  hc_reg$END_DT[hc_reg$END_DT > end_dt] = as.Date('9999/01/01')
  
  # Use a subset of ICD-10 codes =================
  diagnostic_codes = read.csv("ICD10-F.csv")
  diagnostic_codes = diagnostic_codes[nchar(diagnostic_codes$CD1) == 3, ]
  diagnostic_codes = diagnostic_codes[sample(1:nrow(diagnostic_codes), 100), ]
  
  
  # Generate primary care records =================
  pc = data.frame(ID = sample(hc_reg$ID, 
                              number_of_pc_records, 
                              replace=TRUE),
                  EVENT_CD = sample(diagnostic_codes$CD1, 
                                    number_of_pc_records, 
                                    replace=TRUE))
  # Event dates must be within the registered period
  pc$EVENT_DT = as.Date('9999-01-01')
  for (ID_i in unique(pc$ID)){
    mask_i = pc$ID == ID_i
    ID_i = 1
    dt = difftime(min(end_dt, hc_reg$END_DT[hc_reg$ID == ID_i]), 
                  hc_reg$START_DT[hc_reg$ID == ID_i])
    pc$EVENT_DT[mask_i] = 
      hc_reg$START_DT[hc_reg$ID == ID_i] + 
      sample.int(dt, sum(mask_i), replace = TRUE)
  }
  
  # Reorder columns
  pc = pc[, c("ID", "EVENT_DT", "EVENT_CD")]
  
  
  # Generate emergency department records =================
  ed = data.frame(ID = sample(hc_reg$ID, 
                              number_of_ed_records, 
                              replace=TRUE),
                  REASON_CD = sample(diagnostic_codes$CD1, 
                                     number_of_ed_records, 
                                     replace=TRUE))
  # Event dates must be within the registered period
  ed$ATTEND_DT = as.Date('9999-01-01')
  for (ID_i in unique(ed$ID)){
    mask_i = ed$ID == ID_i
    ID_i = 1
    dt = difftime(min(end_dt, hc_reg$END_DT[hc_reg$ID == ID_i]), 
                  hc_reg$START_DT[hc_reg$ID == ID_i])
    ed$ATTEND_DT[mask_i] = 
      hc_reg$START_DT[hc_reg$ID == ID_i] + 
      sample.int(dt, sum(mask_i), replace = TRUE)
  }
  
  # Reorder columns
  ed = ed[, c("ID", "ATTEND_DT", "REASON_CD")]
  
  # Generate hospital records =================
  
  ha = data.frame(ID = sample(hc_reg$ID, 
                              number_of_ha_records, 
                              replace=TRUE),
                  DIAG_CD = sample(diagnostic_codes$CD1, 
                                   number_of_ha_records, 
                                   replace=TRUE))
  # Event dates must be within the registered period
  ha$ADMIS_DT = as.Date('9999-01-01')
  ha$DISCH_DT = as.Date('9999-01-01')
  for (ID_i in unique(ha$ID)){
    mask_i = ha$ID == ID_i
    ID_i = 1
    dt = difftime(min(end_dt, hc_reg$END_DT[hc_reg$ID == ID_i]), 
                  hc_reg$START_DT[hc_reg$ID == ID_i])
    ha$ADMIS_DT[mask_i] = 
      hc_reg$START_DT[hc_reg$ID == ID_i] + 
      sample.int(dt, sum(mask_i), replace = TRUE)
    # Truncate hospital stays to 100 days max
    ha$DISCH_DT[mask_i] = 
      ha$ADMIS_DT[mask_i] + 
      sample.int(100, sum(mask_i), replace = TRUE) 
    mask_ij = ha$DISCH_DT[mask_i] > min(end_dt,
                                        hc_reg$END_DT[hc_reg$ID == ID_i])
    ha$DISCH_DT[mask_i][mask_ij] = min(end_dt,
                                       hc_reg$END_DT[hc_reg$ID == ID_i])
  }
  
  # Reorder columns
  ha = ha[, c("ID", "ADMIS_DT", "DISCH_DT", "DIAG_CD")]
  
  return(list("hc_reg"=hc_reg, 
              "diagnostic_codes"=diagnostic_codes, 
              "pc"=pc, 
              "ed"=ed, 
              "ha"=ha))
}

res = generate_data()
write.csv(res$hc_reg, "healthcare_registration_table.csv", row.names=F)
write.csv(res$diagnostic_codes, "diagnostic_codes_table.csv", row.names=F)
write.csv(res$pc, "primary_care_table.csv", row.names=F)
write.csv(res$ed, "emergency_department_table.csv", row.names=F)
write.csv(res$ha, "hospital_admissions_table.csv", row.names=F)
