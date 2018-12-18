#' Apply fertility rates methodology for year of interview
#'
#' Applies a given fertility rates methodology rates
#' case-by-case to a data set and returns another data set
#' with the estimates by observation.
#'
#' @param m.intvw A numeric or a vector which indicates the month in which the woman were interviewed.
#' @param y.intvw A numeric which indicate the year in which the woman were interviewed.
#' @param m.wmn A vector specifying the woman’s month of birth (mother or not mother).
#' @param y.wmn A vector specifying the woman’s year of birth (mother or not mother).
#' @param age.wmn A vector specifying the woman's age at the time of the interview (mother or not mother).
#' @param m.child A vector specifying the child’s month of birth (if the woman doesn't have child, NA).
#' @param y.child A vector specifying the child’s year of birth (if the woman doesn't have children, NA).
#' @param child.dummy A vector which indicate 0 if the woman doesn't have a child or 1 if the woman has a child.
#' @param wmn.dummy A vector which indicate TRUE if the woman isn't duplicate or FALSE if the woman is duplicate.
#' @param id.wmn A vector wich indicate the woman's identification.
#' @param ids A vector specifying cluster ids from largest level to smallest level.
#' @param strata A vector specifying strata.
#' @param weights A vetor pecifying sampling weights as an alternative to prob (1/weights).
#' @param data A data frame containing the above variables.
#'
#' @return An object of class frts_intvw containing a data.frame and a list.
#'
#' @examples
#'
#' ## Information from ENADID 2014, INEGI
#'
#' mg1 <- frts_intvw(m.intvw=ENTREV_M, y.intvw=2014, m.wmn=FEC_MUJ_M, y.wmn=FEC_MUJ_A, age.wmn=EDADD, m.child=FEC_HIJ_M,
#' y.child=FEC_HIJ_A, child.dummy=CONT,wmn.dummy=MUJER, id.wmn=ID_1, ids=UPM, strata=ESTRATO,
#' weights = FACTOR, data = enadid_2014)
#'
#' summary(mg1, level = 0.9)
#'
#' @export
#'

frts_intvw<- function(m.intvw, y.intvw, m.wmn,
                      y.wmn, age.wmn, m.child, y.child,
                      child.dummy, wmn.dummy, id.wmn, ids,
                      strata, weights, data){
  if(is.data.frame(data)){
    if (is.element("survey", installed.packages()[,1])) {
      if (!("package:survey" %in% search())) library ("survey")
      attach(data)
      database <- data.frame(m.intvw, y.intvw,
                             m.wmn, y.wmn,
                             m.child, y.child,
                             child.dummy,
                             wmn.dummy, id.wmn)



      intvw.age <-  ifelse(m.wmn >= m.intvw, age.wmn,  age.wmn-1)
      age.wmn <- NULL

      age.wmn <- intvw.age
      age2.wmn <- intvw.age +1

      database$age.wmn <- age.wmn
      database$age2.wmn <- age2.wmn
      database$intvw.age <- intvw.age

      database$expo1 <- 0
      database$expo2 <- 0
      database$expo3 <- 0

      for (i in 1:dim(database)[1]) {

        if(database$wmn.dummy[i]==TRUE){
          if(database$m.intvw[i] > database$m.wmn[i]){
            database$expo1[i] <- ((database$m.wmn[i]-0.5)/12)
            database$expo2[i] <- ((database$m.intvw[i]-database$m.wmn[i]-0.5)/12)
          }else{
            database$expo3[i] <- ((database$m.intvw[i]-1)/12)
          }
        }

      }
      database$m.intvw <- NULL


      auxiliary <- function(age, age.wmn, age2.wmn, exposition1, exposition2, exposition3){
        expo1 <- rep(0,length(exposition1))
        aux<-grep(age, age.wmn, value=FALSE)
        expo1[aux] <- exposition1[aux]
        aux2<-grep(age, age2.wmn, value=FALSE)
        expo1[aux2] <- exposition2[aux2]
        expo2 <- rep(0,length(exposition2))
        aux3<-grep(age, age.wmn, value=FALSE)
        expo2[aux3] <- exposition3[aux3]
        expo <- expo1 + expo2
        return(expo)
      }

      nom <- sprintf("exposition_%s", 15:49)
      data <- data.frame(matrix(ncol =(49- 15)+1, nrow = length(database$age.wmn)))
      colnames(data) <- nom
      for(j in 1:((49 - 15)+1)){
        data[,j]<- auxiliary(j+15-1, database$age.wmn, database$age2.wmn, database$expo1,
                             database$expo2, database$expo3)
      }


      database$expo1 <- NULL
      database$expo2 <- NULL
      database$expo3 <- NULL

      database <- cbind(database,data)

      estimate_age <- function(m.child, y.child, m.wmn, intvw.age, y.intvw){
        age <- NULL
        for(i in 1:length(m.child)){
          if(is.na(m.child[i]) == F){
            if(m.child[i] > m.wmn[i] | m.child[i] < m.wmn[i]){
              if(m.child[i] > m.wmn[i] ){
                age = c(age, intvw.age[i]-(y.intvw-y.child[i])+1)
              }
              else{
                age = c(age, intvw.age[i]-(y.intvw-y.child[i]))
              }
            }
            else{
              age = c(age, intvw.age[i]-(y.intvw-y.child[i])+rbinom(1,1,0.5))
            }
          }
          else{age = c(age,0)}
        }
        return(age)
      }


      database$age.mother <- estimate_age(database$m.child, database$y.child, database$m.wmn, database$intvw.age, y.intvw)
      database$age.group <- cut(database$age.mother, c(-1, 14, 19, 24, 29, 34, 39, 44, 49, 60))
      levels(database$age.group) <- c('0_14', '15_19', '20_24', '25_29', '30_34', '35_39', '40_44', '45_49', '50_60')

      database$expo15_19 <- apply(database[,paste('exposition_',15:19, sep = '')],1,sum)
      database$expo20_24 <- apply(database[,paste('exposition_',20:24, sep = '')],1,sum)
      database$expo25_29 <- apply(database[,paste('exposition_',25:29, sep = '')],1,sum)
      database$expo30_34 <- apply(database[,paste('exposition_',30:34, sep = '')],1,sum)
      database$expo35_39 <- apply(database[,paste('exposition_',35:39, sep = '')],1,sum)
      database$expo40_44 <- apply(database[,paste('exposition_',40:44, sep = '')],1,sum)
      database$expo45_49 <- apply(database[,paste('exposition_',45:49, sep = '')],1,sum)

      database$y.wmn <- NULL
      database$m.child <- NULL


      database$ids <- ids
      database$strata <- strata
      database$weights <- weights

      detach(data)
      attach(database)


      ds <- svydesign(id = ~ids,
                      strata = ~strata,
                      weights = ~weights,
                      data = database, nest=TRUE) #JUST NEST

      value <- list(df = database, ds = ds)

      attr(value, 'class') <- 'frts_intvw'
      value


    } else {
      stop("Library 'survey' must be installed")
    }
  } else {
    stop('There is not some data.frame')

  }

}
