incidence <- function(year, diag, washout, REG, PC, ED, HA) {
 
  # Specifying variables
  N_ID = nrow(REG)
  pyr = 0
  ndiag = 0
  ID_i = 1
  row_i = REG$ID[ID_i]
  
  # Creating a while loop
  while (ID_i < N_ID + 1) {
    
    # Excluding the record if unregistered at the start of the washout period  
    if (REG$START_DT[ID_i] > (as.Date(paste0(year, "/01/01")) - washout)) {
      ID_i = ID_i + 1
      next
    }
  
    # Excluding the record if unregistered at the start of year
    if (REG$END_DT[ID_i] < as.Date(paste0(year, "/01/01"))) {
      ID_i = ID_i + 1
      next
    }
  
    # Creating a table of IDs and diagnostic codes from the PC table
    PC_i = filter(PC, ID == ID_i, EVENT_CD == diag)
    
    # Finding the first diagnosis
    first_dt = min(PC_i[,2], as.Date('9999/01/01'))
    
    # Excluding the record if the diagnosis was before the start of the year
    if (first_dt < as.Date(paste0(year, "/01/01"))) {
      ID_i = ID_i + 1
      next
    }
    
    # Creating a table of IDs and diagnostic codes from the ED table
    ED_i = filter(ED, ID == ID_i, REASON_CD == diag)
    
    # Finding the first diagnosis
    first_dt = min(ED_i[,2], as.Date('9999/01/01'))
    
    # Excluding the record if the diagnosis was before the start of the year
    if (first_dt < as.Date(paste0(year, "/01/01"))) {
      ID_i = ID_i + 1
      next
    }
    
    # Creating a table of IDs and diagnostic codes from the HA table
    HA_i = filter(HA, ID == ID_i, DIAG_CD == diag)
    
    # Finding the first diagnosis
    first_dt = min(HA_i[,2], as.Date('9999/01/01'))
    
    # Excluding the record if the diagnosis was before the start of the year
    if (first_dt < as.Date(paste0(year, "/01/01"))) {
      ID_i = ID_i + 1
      next
    }
    
    # Calculating person years at risk for current record
    start_fup = as.Date(paste0(year, "/01/01"))
    end_fup = min(as.Date(paste0(year, "/12/31")), REG$END_DT[ID_i], first_dt)
    # pyr_i = (end_fup - start_fup) / 365
    
    # Adding to total person years at risk
    pyr = pyr + pyr_i
    
    # Calculating number of new diagnoses
    if (first_dt < as.Date(paste0(year, "/12/31"))) {
      ndiag = ndiag + 1
      next
    }
    ID_i = ID_i + 1
  } 
  # Calculating annual incidence
  return(ndiag / pyr)
}