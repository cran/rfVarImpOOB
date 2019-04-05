
\docType{data}
\name{arabidopsis}
\alias{arabidopsis}
\title{Arabidopsis thaliana}
\format{Data frame with columns
\describe{
\item{edit}{binary:the response at the site of interest}
\item{X.k}{nucleotides at positions -k, relative to the edited site (4 categories)}
\item{Xk}{nucleotides at positions k, relative to the edited site (4 categories)}
\item{cp}{ the codon position (4 categories),}
\item{fe}{the estimated folding energy (continuous)}
\item{dfe}{the difference in estimated folding energy between pre-
edited and edited sequences (continuous)}
}}
 
\source{
[1] Cummings, Michael P, and Daniel S Myers. Simple Statistical Models Predict C-to-U Edited Sites in Plant Mitochondrial RNA. BMC Bioinformatics, 2004, 7.
}
\usage{
arabidopsis
}
\description{
RNA editing is the process whereby RNA is modified from
the sequence of the corresponding DNA template [1].
For instance, cytidine-to-uridine conversion (abbreviated
C-to-U conversion) is common in plant mitochondria.
The mechanisms of this conversion remain largely
unknown, although the role of neighboring nucleotides is
emphasized. Cummings and Myers [1] suggest to use
information from sequence regions flanking the sites of
interest to predict editing in Arabidopsis thaliana, Brassicanapus and Oryza sativa based on random forests. The Arabidopsis thaliana data of [1] can be loaded from the journal Web site.

For each of the 876 observations, the
data set gives

 the response at the site of interest (binary: edited/not
edited) and as potential predictor variables
 the 40 nucleotides at positions -20 to 20, relative to the
edited site (4 categories),
 cp: the codon position (4 categories),
  fe: the estimated folding energy (continuous) and
 dfe: the difference in estimated folding energy between pre-
edited and edited sequences (continuous).


}
\examples{
arabidopsis
}
\keyword{datasets}

