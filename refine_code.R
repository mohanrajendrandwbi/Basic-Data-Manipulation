# install.packages("stringr")
# library("stringr")
install.packages("stringdist")
library("stringdist")
install.packages("dplyr")
library("dplyr")
install.packages("tidyr")
library("tidyr")
install.packages("dummies")
library("dummies")

# 0: Load the data in RStudio
dfOriginal <- read.csv("refine_original.csv")
df <- dfOriginal
# View(df)

# 1: Clean up brand names
# closeMatch function will replace the observations in the vector 
# with the closest match in the control vector
closeMatch <- function(x, y) {
  wt <- c()
    for (i in 1:length(y)) {
    wt[i] <- stringdist(x, y[i])
    }
  which.min(wt)
  return(y[which.min(wt)])
}
brandName <- c( "philips", "akzo", "van houten",  "unilever")
df$company <- sapply(df$company, closeMatch, y = brandName)
# View(df)

# 2: Separate product code and number
colnames(df)[which(names(df) == "Product.code...number")] <- "Productcode_number"
df <- separate(df, Productcode_number, c("Productcode", "number"), sep = "-", remove = FALSE)
# View(df)

# 3: Add product categories
Productcode <- c("p", "v", "x", "q")
Productcategory <- c("Smartphone", "TV", "Laptop", "Tablet")
prodCatLookup <- data.frame(Productcode, Productcategory)
df <- left_join(df, prodCatLookup)
# View(df)

# 4: Add full address for geocoding
full_address <- paste(df$address, df$city, df$country, sep = ", ")
df <- cbind(df, full_address)
# View(df)

# 5: Create dummy variables for company and product category
company <- gsub(" ", "_",df$company)
df2 <- dummy(company, sep = "_")
df <- cbind(df, df2)

product <- tolower(df$Productcategory)
df3 <- dummy(product, sep = "_")
df <- cbind(df, df3)
# View(df)

#6 Write back to CSV file as refine_clean.csv
write.csv(df, file = "refine_clean.csv")