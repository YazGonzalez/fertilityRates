% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frts_3yrs.R
\name{frts_3yrs}
\alias{frts_3yrs}
\title{Apply fertility rates methodology for last four years to three-year periods}
\usage{
frts_3yrs(m.intvw, y.intvw, y.first, y.second, y.third, m.wmn, y.wmn,
  age.wmn, m.child, y.child, wmn.dummy, id.wmn, ids, strata, weights, data)
}
\arguments{
\item{m.intvw}{A numeric or a vector which indicates the month in which the woman were interviewed.}

\item{y.intvw}{A numeric which indicate the year in which the woman were interviewed.}

\item{y.first}{A numeric which indicate the first year retrospectively of three-year periods.}

\item{y.second}{A numeric which indicate the second year retrospectively of three-year periods.}

\item{y.third}{A numeric which indicate third year retrospectively of three-year periods.}

\item{m.wmn}{A vector specifying the woman’s month of birth (mother or not mother).}

\item{y.wmn}{A vector specifying the woman’s year of birth (mother or not mother).}

\item{age.wmn}{A vector specifying the woman's age at the time of the interview (mother or not mother).}

\item{m.child}{A vector specifying the child’s month of birth (if the woman doesn't have child, NA).}

\item{y.child}{A vector specifying the child’s year of birth (if the woman doesn't have children, NA).}

\item{wmn.dummy}{A vector which indicate TRUE if the woman isn't duplicate or FALSE if the woman is duplicate.}

\item{id.wmn}{A vector wich indicate the woman's identification.}

\item{ids}{A vector specifying cluster ids from largest level to smallest level, ~0 or ~1 is a formula for no clusters.}

\item{strata}{A vector specifying strata.}

\item{weights}{A vetor pecifying sampling weights as an alternative to prob (1/weights).}

\item{data}{A data frame containing the above variables.}
}
\value{
An object of class frts_3yrs containing a data.frame, a list and a vector.
}
\description{
Applies a given fertility rates methodology rates
case-by-case to a data set and returns another data set
with the estimates by observation.
}
\examples{

## Information from ENADID 2014, INEGI

mg3 <- frts_3yrs(m.intvw=ENTREV_M , y.intvw=2014, y.first=2013, y.second=2012, y.third=2011,
 m.wmn=FEC_MUJ_M, y.wmn=FEC_MUJ_A, age.wmn=EDAD_M, m.child=FEC_HIJ_M, y.child=FEC_HIJ_A,
 wmn.dummy=MUJER, id.wmn=ID_1, ids=UPM, strata=ESTRATO, weights = FACTOR, data = enadid_2014)

summary(mg3, level = 0.9)

}
