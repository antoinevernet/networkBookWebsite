
# Lazega data exercice

# Set file paths
ELwork_path <- "/Users/antoinevernet/Dropbox/Teaching/social_networks_pm/data_assignment/LazegaLawyers/ELwork.dat"
ELfriend_path <- "/Users/antoinevernet/Dropbox/Teaching/social_networks_pm/data_assignment/LazegaLawyers/ELfriend.dat"
ELadv_path <- "/Users/antoinevernet/Dropbox/Teaching/social_networks_pm/data_assignment/LazegaLawyers/ELadv.dat"
ELattr_path <- "/Users/antoinevernet/Dropbox/Teaching/social_networks_pm/data_assignment/LazegaLawyers/ELattr.dat"

# Load network data as adjacency matrices
ELwork <- as.matrix(read.table(ELwork_path, header = FALSE, as.is = TRUE))
ELfriend <- as.matrix(read.table(ELfriend_path, header = FALSE, as.is = TRUE))
ELadv <- as.matrix(read.table(ELadv_path, header = FALSE, as.is = TRUE))

# Load attribute data as dataframe
ELattr <- read.table(ELattr_path, header = FALSE, col.names = c("ID", "Status", "Gender", "Office", "YearsWithFirm", "Age", "Practice", "LawSchool"))

# Print first few rows of each to verify
head(ELwork)
head(ELfriend)
head(ELadv)
head(ELattr)

# Transform into network data
library(network)
library(ergm)
library(sna)

# Convert adjacency matrices to network objects
netWork <- network(ELwork, directed = TRUE, matrix.type = "adjacency")
netFriend <- network(ELfriend, directed = TRUE, matrix.type = "adjacency")
netAdv <- network(ELadv, directed = TRUE, matrix.type = "adjacency")

#Recoding attributes

# Recode Status
ELattr$Status <- factor(ELattr$Status, levels = c(1, 2), labels = c("Partner", "Associate"))

# Recode Gender
ELattr$Gender <- factor(ELattr$Gender, levels = c(1, 2), labels = c("Man", "Woman"))

# Recode Office
ELattr$Office <- factor(ELattr$Office, levels = c(1, 2, 3), labels = c("Boston", "Hartford", "Providence"))

# Recode Practice
ELattr$Practice <- factor(ELattr$Practice, levels = c(1, 2), labels = c("Litigation", "Corporate"))

# Recode Law School
ELattr$LawSchool <- factor(ELattr$LawSchool, levels = c(1, 2, 3), labels = c("Harvard/Yale", "UConn", "Other"))

# Verify changes
head(ELattr)



# Add vertex attributes from ELattr dataframe to each network object
# Assuming ELattr has been recoded and netWork, netFriend, netAdv are defined

# Convert factors to character strings before setting as vertex attributes
for(attr in names(ELattr)[-1]){ # Skip the first column (ID)
  # Convert factor to character for the current attribute
  if (attr %in% c("Age", "YearsWithFirm")){
    # Set vertex attributes
    set.vertex.attribute(netWork, attr, attr_values)
    set.vertex.attribute(netFriend, attr, attr_values)
    set.vertex.attribute(netAdv, attr, attr_values)
  }else{
    attr_values <- as.character(ELattr[[attr]])
    # Set vertex attributes
    set.vertex.attribute(netWork, attr, attr_values)
    set.vertex.attribute(netFriend, attr, attr_values)
    set.vertex.attribute(netAdv, attr, attr_values)
  }
}

for(attr in names(ELattr)[-1]){ # Skip the first column (ID)
  # Convert factor to character for the current attribute
    attr_values <- as.character(ELattr[[attr]])
    # Set vertex attributes
    set.vertex.attribute(netWork, attr, attr_values)
    set.vertex.attribute(netFriend, attr, attr_values)
    set.vertex.attribute(netAdv, attr, attr_values)
  }

# Age
set.vertex.attribute(netWork, "Age", ELattr[["Age"]])
set.vertex.attribute(netFriend, "Age", ELattr[["Age"]])
set.vertex.attribute(netAdv, "Age", ELattr[["Age"]])

# YearsWithFirm
set.vertex.attribute(netWork, "YearsWithFirm", ELattr[["YearsWithFirm"]])
set.vertex.attribute(netFriend, "YearsWithFirm", ELattr[["YearsWithFirm"]])
set.vertex.attribute(netAdv, "YearsWithFirm", ELattr[["YearsWithFirm"]])

# Example: Checking the network object and attributes
summary(netWork)
summary(netFriend)
summary(netAdv)

# Print first few vertex attributes to verify
list.vertex.attributes(netWork)
get.vertex.attribute(netWork, "Status")[1:5]
get.vertex.attribute(netWork, "Age")[1:5]
get.vertex.attribute(netWork, "YearsWithFirm")[1:5]


# ergm test

m1 <- ergm(netWork ~ edges)


m2 <- ergm(netWork ~ edges + nodeicov("Age") + nodematch("Age") + nodematch("Office") + nodematch("Status") + nodematch("Practice") + nodematch("LawSchool"))

m3 <- ergm(netAdv ~ edges + nodeicov("Age") + nodematch("Age") + nodematch("Office") + nodematch("Status") + nodematch("Practice") + nodematch("LawSchool"))

m4 <- ergm(netFriend ~ edges + nodeicov("Age") + nodematch("Age") + nodematch("Office") + nodematch("Status") + nodematch("Practice") + nodematch("LawSchool"))

m_class <- ergm(netAdv ~ edges + nodeocov("YearsWithFirm") + nodematch("Practice") + nodeifactor("LawSchool") + nodematch("LawSchool", diff=TRUE) + nodematch("Office") + nodeifactor("Gender") + nodeifactor("Status"))



