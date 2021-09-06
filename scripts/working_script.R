## install libraries
packages <- c("tidyverse", "dplyr")

lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }  
})

the_data <- read_csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"),
                     col_names = FALSE)

colnames(the_data) <- c("age", "workclass", "fnlwgt", "education",
                        "education_num", "maritial_status", "occupation", "relationship", "race",
                        "sex", "capital_gain", "capital_loss",
                        "hours_per_week", "native_country", "income")

##convert characters to factors
the_data <- the_data %>% mutate(across(where(is.character), as.factor))
