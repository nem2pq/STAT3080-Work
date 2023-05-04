library(tidyverse)
library(car)

vocab1<-filter(Vocab, education>12 & vocabulary<5)
select(vocab1, year)



summarize(Vocab, a.education = mean(education),a.vocabulary=mean(vocabulary),
          cor=group_by(cor(education,vocabulary)))

DavisPlot <- ggplot(Davis, aes(x=weight, y=repwt))
DavisPlot

DavisPlot + geom_point(shape=22)