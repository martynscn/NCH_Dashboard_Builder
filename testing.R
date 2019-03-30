csv_data <- read.csv(file = "C:/Users/totus tuus/Documents/R_projects/NCH_Training_Dashboard/NCH_Training_Dashboard_2/www/NCH_Training_2019 - Summary.csv", stringsAsFactors = FALSE, na.strings = "N/A", check.names = FALSE)
original_names <- names(csv_data)
cleaned_names <- make.names(original_names)
resolution_names <-  cleaned_names[2:length(cleaned_names)]
names(resolution_names) <- original_names[2:length(original_names)]
csv_data_list <- lapply(1:ncol(csv_data), function(x) {
  if(x == 1) {csv_data[,x]
  } else {as.numeric(csv_data[,x])}})

  names_lookup_table <- data.frame(original_names = original_names, cleaned_names = cleaned_names)
  csv_data_df <- as.data.frame(x = csv_data_list,row.names = NULL, stringsAsFactors = FALSE,
                               col.names = cleaned_names)

NigeriaStates <- data.frame(code = c("Abia", "Adamawa", 
                                     "Akwa Ibom", "Anambra", "Bauchi", "Bayelsa", "Benue", "Borno", 
                                     "Cross River", "Delta", "Ebonyi", "Edo", "Ekiti", "Enugu", "Federal Capital Territory", 
                                     "Gombe", "Imo", "Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", 
                                     "Kogi", "Kwara", "Lagos", "Nassarawa", "Niger", "Ogun", "Ondo", 
                                     "Osun", "Oyo", "Plateau", "Rivers", "Sokoto", "Taraba", "Yobe", 
                                     "Zamfara"), MNHStateNames = c('Abia','Adamawa','Akwa Ibom','Anambra','Bauchi','Bayelsa','Benue','Borno','Cross River','Delta','Ebonyi','Edo','Ekiti','Enugu','FCT','Gombe','Imo','Jigawa','Kaduna','Kano','Katsina','Kebbi','Kogi','Kwara','Lagos','Nasarawa','Niger','Ogun','Ondo','Osun','Oyo','Plateau','Rivers','Sokoto','Taraba','Yobe','Zamfara'))
df <- csv_data_df
my_map_data <- left_join(x = NigeriaStates, y = df, by = c("MNHStateNames" = "States"),copy = TRUE)
res <- "X02..The.adoption.and.implementation.of.the.Guidelines.for.Quantification.of.Narcotic.Medicines.by.all.the.States.of.the.Federation.and.the.FCT"
filtered_map_data <- my_map_data[!is.na(my_map_data[,res]),c("code",res)]
hc <- hcmap("countries/ng/ng-all",download_map_data = TRUE, data = filtered_map_data, value = res,
            joinBy = c("woe-name", "code"), name = res,
            dataLabels = list(enabled = TRUE, format = "{point.name}"),
            borderColor = "#FAFAFA", borderWidth = 0.1,
            tooltip = list(valueDecimals = 2, valuePrefix = "",
                           valueSuffix = ""))
hc

library(dplyr)
library(tibble)
library(formattable)
csv_data_df$R1 <- cut(csv_data_df$X01..The.adoption.and.implementation.of.the.National.Policy.for.Controlled.Medicines..NPCM..and.its.Strategies.in.all.the.36.States.of.the.Federation.and.the.Federal.Capital.Territory..FCT..,
                      breaks = c(-Inf,0.01,0.25,0.50,0.75,0.99,Inf),
                      labels = c("No action taken","Slow progress","Moderate progress","Significant progress","Near completion","Completed"))
csv_data_df$R1 <- NULL

csv_data_interval <- sapply(csv_data_df[2:ncol(csv_data_df)], function(x) {
  cut(x, breaks = c(-Inf,0.01,0.25,0.50,0.75,0.99,Inf),
      labels = c("No action taken","Slow progress","Moderate progress","Significant progress","Near completion","Completed"))
})
csv_data_int_df <- as.data.frame(csv_data_interval)
csv_data_int_df <- add_column(csv_data_int_df,States = csv_data_df$States, .before = 1)
names(csv_data_int_df)[2:ncol(csv_data_int_df)] <- paste0("R",c(1:(ncol(csv_data_int_df)-1)))

formatted_table <- 
  formattable(x = csv_data_int_df,
              formatters = 
                list(
                  States = 
                    formatter(
                      "span",
                      style = x ~ ifelse(
                        x == "FCT",
                        style(color = "green", font.weight = "bold"),NA
                      )
                    )
                  ,area(col = c(R2,R3)) ~ formatter(
                    "div",
                    style = x ~ ifelse(
                      x == "Significant progress",
                      style(color = "green", font.weight = 2),NA
                    )
                  )
                  ,area(col = 2:ncol(csv_data_int_df)) ~ formatter(
                    "div",
                    style = x ~ 
                      ifelse(
                        x == "Near completion",
                        style(color = "white", font.weight = "bold",border = "black", background = "green"),
                      ifelse(
                      x == "No action taken",
                      style(color = "white", font.weight = 5,border = "black", background = "red"),
                      ifelse(
                      x == "Slow progress",
                      style(color = "black", font.weight = 5,border = "black", background = "orange"),
                      ifelse(
                      x == "Moderate progress",
                      style(color = "black", font.weight = 5,border = "black", background = "yellow"),
                      ifelse(
                      x == "Significant progress",
                      style(color = "white", font.weight = 2,background = "green"),
                      ifelse(
                        x == "Completed",
                        style(color = "black", font.weight = "bold",background = "green"),
                        # NA
                        style(color = "blue", font.weight = "bold",background = "green")
                    ))))))
                  )
                ),align = c("l","r","r"))

formatted_table
