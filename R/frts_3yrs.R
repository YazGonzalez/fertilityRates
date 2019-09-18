#' Apply fertility rates methodology for last five years to three-year periods
#'
#' Applies a given fertility rates methodology rates
#' case-by-case to a data set and returns another data set
#' with the estimates by observation.
#'
#' @param m.intvw A numeric or a vector which indicates the month in which the woman were interviewed.
#' @param y.intvw A numeric which indicate the year in which the woman were interviewed.
#' @param y.first A numeric which indicate the first year retrospectively of three-year periods.
#' @param y.second A numeric which indicate the second year retrospectively of three-year periods.
#' @param y.third A numeric which indicate third year retrospectively of three-year periods.
#' @param m.wmn A vector specifying the woman’s month of birth (mother or not mother).
#' @param y.wmn A vector specifying the woman’s year of birth (mother or not mother).
#' @param age.wmn A vector specifying the woman's age at the time of the interview (mother or not mother).
#' @param m.child A vector specifying the child’s month of birth (if the woman doesn't have child, NA).
#' @param y.child A vector specifying the child’s year of birth (if the woman doesn't have children, NA).
#' @param wmn.dummy A vector which indicate TRUE if the woman isn't duplicate or FALSE if the woman is duplicate.
#' @param id.wmn A vector wich indicate the woman's identification.
#' @param ids A vector specifying cluster ids from largest level to smallest level.
#' @param strata A vector specifying strata.
#' @param weights A vetor pecifying sampling weights as an alternative to prob (1/weights).
#' @param data A data frame containing the above variables.
#'
#' @return An object of class frts_3yrs containing a data.frame, a list and a vector.
#'
#' @examples
#'
#' ## Information from ENADID 2014, INEGI
#'
#' mg3 <- frts_3yrs(m.intvw=ENTREV_M , y.intvw=2014, y.first=2013, y.second=2012, y.third=2011,
#'  m.wmn=FEC_MUJ_M, y.wmn=FEC_MUJ_A, age.wmn=EDAD_M, m.child=FEC_HIJ_M, y.child=FEC_HIJ_A,
#'  wmn.dummy=MUJER, id.wmn=ID_1, ids=UPM, strata=ESTRATO, weights = FACTOR, data = enadid_2014)
#'
#' summary(mg3, level = 0.9)
#'
#' @export
#'
frts_3yrs<- function(m.intvw, y.intvw, y.first, y.second, y.third, m.wmn,
                     y.wmn, age.wmn, m.child, y.child,
                     wmn.dummy, id.wmn, ids,
                     strata, weights, data){

  if(is.data.frame(data)){
    if (requireNamespace("survey", quietly = TRUE)) {

      attach(data)
      database <- data.frame(m.wmn, y.wmn,
                             m.child, y.child,
                             wmn.dummy, id.wmn)

      database$child.dummy <- ifelse(is.na(y.child)==FALSE, 1, 0)


      intvw.age <-  ifelse(m.wmn >= m.intvw, age.wmn,  age.wmn-1)
      age.wmn <- NULL



      age.wmn <- intvw.age -(y.intvw - y.first)
      age2.wmn <- age.wmn + 1

      age3.wmn <- intvw.age -(y.intvw - y.second)
      age4.wmn <- age3.wmn + 1

      age5.wmn <- intvw.age -(y.intvw - y.third)
      age6.wmn <- age5.wmn + 1

      database$age.wmn <- age.wmn
      database$age2.wmn <- age2.wmn
      database$age3.wmn <- age3.wmn
      database$age4.wmn <- age4.wmn
      database$age5.wmn <- age5.wmn
      database$age6.wmn <- age6.wmn
      database$intvw.age <- intvw.age


      database$expo1 <- 0
      database$expo2 <- 0


      for (i in 1:dim(database)[1]) {

        if(database$wmn.dummy[i]==TRUE){
          database$expo1[i] <- ((database$m.wmn[i]-0.5)/12)
          database$expo2[i] <- (1-database$expo1[i])
        }

      }



      auxiliary <- function(age, age.wmn, age2.wmn, exposition1, exposition2){
        expo1 <- rep(0,length(exposition1))
        aux<-grep(age, age.wmn, value=FALSE)
        expo1[aux] <- exposition1[aux]
        aux2<-grep(age, age2.wmn, value=FALSE)
        expo1[aux2] <- exposition2[aux2]

        return(expo1)
      }



      nom <- sprintf("exposition_%s", min(database$age.wmn):max(database$age2.wmn))
      data <- data.frame(matrix(ncol =(max(database$age2.wmn)- min(database$age.wmn))+1, nrow = length(database$age.wmn)))
      colnames(data) <- nom
      for(j in 1:((max(database$age2.wmn)- min(database$age.wmn))+1)){
        data[,j]<- auxiliary(j+min(database$age.wmn)-1,database$age.wmn,database$age2.wmn,database$expo1,database$expo2)
      }

      temp <- rep(0, nrow(data))
      data2 <- cbind(data[ ,-1], temp)
      data3 <- cbind(data2[ ,-1], temp)
      data <- data + data2 + data3


      database$expo1 <- NULL
      database$expo2 <- NULL


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
            age = c(age, intvw.age[i]-(y.intvw-y.child[i])+stats::rbinom(1,1,0.5))
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


      ds <- survey::svydesign(id = ~ids,
                      strata = ~strata,
                      weights = ~weights,
                      data = database, nest=TRUE)


      period <- c(y.first, y.second, y.third)

      value <- list(df = database, ds = ds, period = period)

      attr(value, 'class') <- 'frts_3yrs'
      value



    } else {
      stop("Library 'survey' must be installed")
    }
  } else {
    stop('There is not some data.frame')

  }

}
