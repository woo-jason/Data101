getwd()

moody <- read.csv("moodyJanuary31b.csv")
names(moody)

colors <- c('red', 'blue','cyan', 'yellow', 'green')

barplot(table(moody$Grade), main = 'Frequency of Grades', xlab='Grade', ylab='Score', col=colors)

boxplot(moody$Score~moody$Grade, main = 'Distribution of Scores by Grade', xlab = 'Grade', ylab = 'Score', col=colors)

#Asking Attribute
barplot(table(moody$Asking), main = 'Frequency of Asking', xlab='Asking', ylab='Frequency', col=colors)

often_asking <- subset(moody, Asking=="Often")
barplot(table(often_asking$Grade), main ='Frequency of Grades of Students who often ask questions', xlab='Grade', ylab='Frequency', col=colors)

sometimes_asking <- subset(moody, Asking=="Sometimes")
barplot(table(sometimes_asking$Grade), main ='Frequency of Grades of Students who sometimes ask questions', xlab='Grade', ylab='Frequency', col=colors)

never_asking <- subset(moody, Asking=="Never")
barplot(table(never_asking$Grade), main ='Frequency of Grades of Students who never ask questions', xlab='Grade', ylab='Frequency', col=colors)

asking <- tapply(moody$Score, moody$Asking, median)
barplot(asking, main='Median Score for Asking', xlab='Asking', ylab='Median Score', col=colors)

#Texting Attribute

barplot(table(moody$Texting), main ='Frequency of Texting', xlab='Texting', ylab='Frequency', col=colors)

always_texting <- subset(moody,Texting =='Always')
barplot(table(always_texting$Grade), main='Frequency of Grades of Students Who always text', xlab='Grade', ylab='Frequency', col=colors)

never_texting <- subset(moody,Texting =='Never')
barplot(table(never_texting$Grade), main='Frequency of Grades of Students Who never text', xlab='Grade', ylab='Frequency', col=colors)
        
often_texting <- subset(moody,Texting =='Often')
barplot(table(often_texting$Grade), main='Frequency of Grades of Students Who often text', xlab='Grade', ylab='Frequency', col=colors)

sometimes_texting <- subset(moody,Texting =='Sometimes')
barplot(table(sometimes_texting$Grade), main='Frequency of Grades of Students Who sometimes text', xlab='Grade', ylab='Frequency', col=colors)

texting <- tapply(moody$Score, moody$Texting, median)
barplot(texting, main='Median Score for Texting', xlab='Texting', ylab='Median Score', col=colors)

#Dozing Attribute

barplot(table(moody$Dozing), main ='Frequency of Dozing', xlab='Dozing', ylab='Frequency', col=colors)

always_dozing <- subset(moody,Dozing =='Always')
barplot(table(always_dozing$Grade), main='Frequency of Grades of Students who always doze', xlab='Grade', ylab='Frequency', col=colors)

never_dozing <- subset(moody,Dozing =='Never')
barplot(table(never_dozing$Grade), main='Frequency of Grades of Students who never doze', xlab='Grade', ylab='Frequency', col=colors)

sometimes_dozing <- subset(moody,Dozing =='Sometimes')
barplot(table(sometimes_dozing$Grade), main='Frequency of Grades of Students who sometimes doze', xlab='Grade', ylab='Frequency', col=colors)

dozing <- tapply(moody$Score, moody$Dozing, median)
barplot(dozing, main='Median Score for Dozing', xlab='Dozing', ylab='Median Score', col=colors)

#Scores vs GPA

plot(moody$GPA, moody$Score, main='Scores vs GPA', xlab='GPA', ylab='Score', col='red')

#Distribution of GPA by Grade
boxplot(moody$GPA~moody$Grade, main ='Distribution of GPA by Grade', xlab='Grade', ylab='GPA', col = colors)

#Distribution of Scores by Grade for Asking
boxplot(often_asking$Score~often_asking$Grade, main="Distribution of Score by Grade for Those Who often ask questions", xlab='Grade', ylab='Score', col = colors)

boxplot(sometimes_asking$Score~sometimes_asking$Grade, main="Distribution of Score by Grade for Those Who sometimes ask questions", xlab='Grade', ylab='Score', col = colors)

boxplot(never_asking$Score~never_asking$Grade, main="Distribution of Score by Grade for Those Who never ask questions", xlab='Grade', ylab='Score', col = colors)

#Distribution of Scores by Grade for Texting
boxplot(always_texting$Score~always_texting$Grade, main='Distribution of Score by Grade for those who always text', xlab='Grade', ylab='Score', col=colors)

boxplot(never_texting$Score~never_texting$Grade, main='Distribution of Score by Grade for those who never text', xlab='Grade', ylab='Score', col=colors)

boxplot(often_texting$Score~often_texting$Grade, main='Distribution of Score by Grade for those who often text', xlab='Grade', ylab='Score', col=colors)

boxplot(sometimes_texting$Score~sometimes_texting$Grade, main='Distribution of Score by Grade for those who sometimes text', xlab='Grade', ylab='Score', col=colors)

#Distribution of Scores by Grade for Dozing
boxplot(always_dozing$Score~always_dozing$Grade, main='Distribution of Score by Grade for those who always doze', xlab='Grade', ylab='Score', col=colors)

boxplot(never_dozing$Score~never_dozing$Grade, main='Distribution of Score by Grade for those who never doze', xlab='Grade', ylab='Score', col=colors)

boxplot(sometimes_dozing$Score~sometimes_dozing$Grade, main='Distribution of Score by Grade for those who sometimes doze', xlab='Grade', ylab='Score', col=colors)
