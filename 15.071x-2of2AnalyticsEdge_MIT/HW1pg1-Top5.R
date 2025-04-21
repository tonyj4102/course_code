


Top5=rbind(Top51,Top52,Top53, Top54, Top55)
> Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription, Top5$Arrest)

table(Top5$LocationDescription, Top5$Weekday)
