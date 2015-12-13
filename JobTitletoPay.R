library(ggplot2)
library(scales)
YearPayEntries = grep('annum', JobData$pay)
#Excludes ones with pay per hour or daily rates
YearPayEntries = YearPayEntries[-grep("€", JobData$pay[YearPayEntries])]
#Excludes any euro job offers
JobRole = as.list(c('senior','principal','chief','junior','lead '))
# words we looking for (senior, pricnipal, chief, junior, lead)
#lead is followed by a space to avoid 'leading technology companies' etc
Dataset = JobData$pay[YearPayEntries]

Dataset = gsub("[^0-9-]", "", Dataset)
Salaries = data.table(MainTableRef = YearPayEntries, salAverage = 0, salRange = 0, roleLevel = 'basic')

for (i in 1:length(Dataset)) {
if (grepl('-', Dataset[i]) == TRUE) {
  x = strsplit(Dataset[i], '-')
  Salaries$salAverage[i] = (as.numeric(x[[1]][1]) + as.numeric(x[[1]][2]))/2
  Salaries$salRange[i] = as.numeric(x[[1]][2]) - as.numeric(x[[1]][1])
  
}
else { 
  Salaries$salAverage[i] = as.numeric(Dataset[i])
  Salaries$salRange[i] = 0
} }

for  (i in 1:length(JobRole)) {
    x = JobRole[i][[1]]
    y = sub('leading', 'top', JobData$title[YearPayEntries], ignore.case = TRUE)
    y = grep(x, y, ignore.case = TRUE)
    Salaries$roleLevel[y] = x
  }
  #Create a logical vector and use that to choose all those 
# numbers in salaries table     to equal each term  

Salaries[,AverageSalperRole := mean(salAverage),by=roleLevel]
Salaries[,RoleText:=JobData$title[YearPayEntries]]
Salaries[,.(RoleText,roleLevel)] # to ensure they the same
Salaries[,roleLevel:= as.factor(Salaries$roleLevel)]

SampleDis = table(Salaries$roleLevel)


#visualize differences between two key roles

qplot(data=Salaries[,.(roleLevel,salAverage)][roleLevel %in% c("senior","basic")],
      x=roleLevel,y=salAverage,geom="boxplot", ylim = c(0,100000), ylab = "Salary")

meanSals = Salaries[,mean(salAverage),by=roleLevel]
meanSals[,Salary:=meanSals$V1]
meanSals[,V1:=NULL]
# Generate plot
c <- ggplot(data=meanSals, x=roleLevel,y=Salary, aes( x=roleLevel,y=Salary,))
c <- c + geom_bar(stat="identity",(aes(fill = roleLevel)))
c <- c + ylab('Av. Salary (in pounds)')
c + scale_y_continuous(labels = comma)
c
#
ggplot(as.data.frame(SampleDis), aes(x=Var1, y = Freq, fill=Var1)) + geom_bar(stat="identity")
#Frequency Bar Chart


Salaries[,Location:='NA']
for (i in 1:length(Salaries$roleLevel)) {
  x = Salaries$MainTableRef[i]
  Salaries$Location[i] =  ChangedDem$county[match(x,ChangedDem$entryNum)]
}

Salaries$Location[-grep('London', Salaries$Location)] = 'Not London'

Salaries[,.(MeanSals4Loc = mean(salAverage)),by=.(Location,roleLevel)]
Salaries[,countr:=1]
Salaries[,.(MeanSals4Loc = sum(countr)),by=.(Location,roleLevel)]

Salaries[,.(MeanSals4Loc = mean(salAverage)),by=.(Location)] #in total it is higher for london
#but london does have the higher paying roles such as cheif, principal and lead

