guns <- read.csv("~/Desktop/guns.csv")
plot(guns$lawsRank,guns$gunDeathRate,ylab="Gun Death Rate in 2021",xlab="State Rank in Gun Control in 2021",main="Gun Death Rate vs State Rank in Gun Control in 2021",col="red")
plot(guns$HouseholdIncome,guns$gunDeathRate,ylab="Gun Death Rate in 2021",xlab="State Median Household Income in 2021",main="Gun Death Rate vs State Household Income in 2021",col="red")

guns$Combined <-0
guns[guns$HouseholdIncome<62529, ]$Combined <- guns[guns$HouseholdIncome<62529, ]$grade2019 -1/400*(1-guns[guns$HouseholdIncome<62529, ]$HouseholdIncome)
guns[guns$HouseholdIncome>=62529, ]$Combined <- guns[guns$HouseholdIncome>=62529, ]$grade2019 +1/400*guns[guns$HouseholdIncome>=62529, ]$HouseholdIncome

plot(guns$Combined,guns$gunDeathRate,ylab="Gun Death Rate in 2021",xlab="Combined Score",main="Gun Death Rate in 2021 vs Combined Score",col="red")
median(guns$HouseholdIncome)
