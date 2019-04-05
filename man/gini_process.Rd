\name{gini_process}
\alias{gini_process}
\title{computes Gini index }
\description{computes Gini index }
\usage{gini_process(classes, splitvar = NULL)}
\arguments{
  \item{classes}{vector of factors/categorical vars}
  \item{splitvar}{split variable}
}

\value{Gini index}

\author{Markus Loecher <Markus.Loecher@gmail.com>}




\examples{
#Test binary case:

#50/50split
gini_process(c(rep(0,10),rep(1,10)))#0.5 CORRECT !
#10/90split
gini_process(c(rep(0,1),rep(1,9)))#0.18= CORRECT ! 
#0/100split
gini_process(factor(c(rep(0,0),rep(1,10)), levels=c(0,1)))#0


#Test binary case:

#25/25/25/25 split
gini_process(factor(c(rep(0,5),rep(1,5),rep(2,5),
                      rep(3,5)), levels=c(0:3)))#0.75 = 4*0.25*0.75 CORRECT !
#10/10/10/70 split
gini_process(factor(c(rep(0,1),rep(1,1),rep(2,1),
                      rep(3,7)), levels=c(0:3)))#0.48 = 3*0.1*0.9+0.7*0.3  CORRECT !
#0/0/0/100 split
gini_process(factor(c(rep(0,0),rep(1,0),rep(2,0),
                      rep(3,20)), levels=c(0:3)))#0. CORRECT !

}
