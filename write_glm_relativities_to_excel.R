# This function takes relativites and writes them to an Excel sheet

write_glm_relativities_to_excel <- function(tables, factor_coefficients, factor_strength, filename){
  
  # takes a set of exported tables and the model coefficients and writes them to Excel
  
  # define a white text on blue background style to use for all header rows
  headerStyle <- createStyle(bgFill = "#000000", fontColour = "#FFFFFF", halign = 'center')
  
  # write the coefficients table to Excel
  wb <- createWorkbook()
  addWorksheet(wb, "factor summary")
  writeData(wb, "factor summary", factor_coefficients)
  writeData(wb, "factor summary", factor_strength, startCol = 8)
  setColWidths(wb, "factor summary", cols = c(1,8), widths = 60)
  addStyle(wb, sheet = "factor summary", cols = 2:10, rows = 1:1000, style = createStyle(halign = 'right'), gridExpand = TRUE)
  #addStyle(wb, sheet = "factor summary", style=createStyle(numFmt="0.0%"), cols=5, rows=2:100)
  addStyle(wb, sheet = "factor summary", style=headerStyle, cols=c(1:5,8:10), rows=1)
  addStyle(wb, sheet = "factor summary", style = createStyle(halign = 'left',bgFill = "#000000", fontColour = "#FFFFFF"), cols=1, rows=1)
  
  # format the index table
  # index_table <- vector('list', length(tables))
  index_table <- data.table(index = integer(), table = character(), dimensions = integer())[1:length(tables)]
  
  # fill in the base table
  index_table[1,index := 1]
  index_table[1,table := 'base']
  index_table[1,dimensions := 0]
  
  for (i in 2:length(tables)){
    #index_table[[i]] <- colnames(tables[[i]])[1]
    index_table[i,index := i]
    index_table[i,table := colnames(tables[[i]])[1]]
    index_table[i,dimensions := ifelse(dim(tables[[i]])[2]>2,2,1)]
  }
  
  #index_table[1] <- 'base'
  #index_table <- as.data.frame(as.character(index_table))
  #index_table <- cbind(1:nrow(index_table), index_table)
  #colnames(index_table)[1] <- 'index'
  #colnames(index_table)[2] <- 'table'
  
  # write the index worksheet to Excel
  addWorksheet(wb, "index")
  writeData(wb, "index", index_table)
  setColWidths(wb, "index", cols = 1:3, widths = 30)
  addStyle(wb = wb, sheet = "index", cols = 1:3, rows = 1:100, style = createStyle(halign = 'left'), gridExpand = TRUE)
  addStyle(wb, sheet = "index", style=createStyle(bgFill = "#000000", fontColour = "#FFFFFF", halign = 'left'), cols=1:3, rows=1)
  
  # write the tables to Excel
  for (i in 1:length(tables)){
    
    if (index_table[i,dimensions]==0){ # 0 dimensions means the base level - a single number
      
      # add worksheet and format
      addWorksheet(wb,as.character(i))
      setColWidths(wb, as.character(i), cols = 1:3, widths = 30)
      addStyle(wb = wb, sheet = as.character(i), cols = 1L, rows = 1:100, style = createStyle(halign = 'left'))
      addStyle(wb, sheet = as.character(i), cols = 2, rows = 1:100, style = createStyle(halign = 'center'))
      addStyle(wb, sheet = as.character(i), style=headerStyle, cols=1:2, rows=1)
      addStyle(wb, sheet = as.character(i), style = createStyle(halign = 'left',bgFill = "#000000", fontColour = "#FFFFFF"), cols=1, rows=1)
      
      table_to_write <- data.table(base = character(), value = double())[1:2]
      table_to_write[1, base := 'technical']
      table_to_write[2, base := 'implementable']
      table_to_write[1, value := tables[[i]]]
      table_to_write[2, value := tables[[i]]]
      
      writeData(wb, as.character(i), table_to_write)
      
    } else if (index_table[i,dimensions]==1) {
      
      # add worksheet and format
      addWorksheet(wb,as.character(i))
      setColWidths(wb, as.character(i), cols = 1:3, widths = 30)
      addStyle(wb = wb, sheet = as.character(i), cols = 1L, rows = 1:100, style = createStyle(halign = 'left'))
      addStyle(wb, sheet = as.character(i), cols = 2:3, rows = 1:100, style = createStyle(halign = 'center'), gridExpand = TRUE)
      addStyle(wb, sheet = as.character(i), style=headerStyle, cols=1:3, rows=1)
      addStyle(wb, sheet = as.character(i), style = createStyle(halign = 'left',bgFill = "#000000", fontColour = "#FFFFFF"), cols=1, rows=1)
      
      # create table in correct format
      table_to_write <- tables[[i]]
      names(table_to_write)[2] <- 'technical'
      table_to_write$implementable <- table_to_write$technical
      
      # write table
      writeData(wb, as.character(i), table_to_write)
      
    } else if (index_table[i,dimensions]==2) {
      
      # get the length of the second dimension
      second_dim_length <- dim(tables[[i]])[2] - 1
      
      # going to write the table twice, once called tech and once called imp
      table_to_write <- tables[[i]]
      
      # write technical table
      w_name <- paste(as.character(i), '_tech', sep = '')
      addWorksheet(wb,w_name)
      setColWidths(wb, w_name, cols = 1:(second_dim_length+1), widths = 30)
      addStyle(wb, sheet = w_name, cols = 2:(second_dim_length+1), rows = 1:100, style = createStyle(halign = 'center'), gridExpand = TRUE)
      addStyle(wb = wb, sheet = w_name, cols = 1L, rows = 1:100, style = createStyle(halign = 'left'))
      addStyle(wb, sheet = w_name, style=headerStyle, cols=1:(second_dim_length+1), rows=1)
      addStyle(wb, sheet = w_name, style = createStyle(halign = 'left',bgFill = "#000000", fontColour = "#FFFFFF"), cols=1, rows=1)
      
      writeData(wb, w_name, table_to_write)
      
      # write implementable table
      w_name <- paste(as.character(i), '_imp', sep = '')
      addWorksheet(wb,w_name)
      setColWidths(wb, w_name, cols = 1:30, widths = 30)
      addStyle(wb, sheet = w_name, cols = 2:(second_dim_length+1), rows = 1:100, style = createStyle(halign = 'center'), gridExpand = TRUE)
      addStyle(wb = wb, sheet = w_name, cols = 1L, rows = 1:100, style = createStyle(halign = 'left'))
      addStyle(wb, sheet = w_name, style=headerStyle, cols=1:(second_dim_length+1), rows=1)
      addStyle(wb, sheet = w_name, style = createStyle(halign = 'left',bgFill = "#000000", fontColour = "#FFFFFF"), cols=1, rows=1)
      writeData(wb, w_name, table_to_write)
      
    }
  }
  
  saveWorkbook(wb, filename, overwrite = TRUE)
  
}
