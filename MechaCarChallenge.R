library(jsonlite)
library(tidyverse)

MechaCar_mpg <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_mpg)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_mpg))

library(pastecs)
Suspension_Coil <- read.csv(file='Suspension_coil.csv',check.names=F,stringsAsFactors = F)
Coil_data <- Suspension_Coil$PSI
stat.desc(Coil_data)

Suspension_Coil <- read.csv(file='Suspension_coil.csv',check.names=F,stringsAsFactors = F)
t.test((Suspension_Coil$PSI),mu=1500)


