#' Apply fertility rates methodology for last fourteen-years to annual periods
#'
#' Applies a given fertility rates methodology rates
#' case-by-case to a data set and returns a list with the estimates by year.
#'
#' @param m.intvw A numeric or a vector which indicates the month in which the woman were interviewed.
#' @param y.intvw A numeric which indicate the year in which the woman were interviewed.
#' @param m.wmn A vector specifying the woman’s month of birth (mother or not mother).
#' @param y.wmn A vector specifying the woman’s year of birth (mother or not mother).
#' @param age.wmn A vector specifying the woman's age at the time of the interview (mother or not mother).
#' @param m.child A vector specifying the child’s month of birth (if the woman doesn't have child, NA).
#' @param y.child A vector specifying the child’s year of birth (if the woman doesn't have children, NA).
#' @param wmn.dummy A vector which indicate TRUE if the woman isn't duplicate or FALSE if the woman is duplicate.
#' @param id.wmn A vector wich indicate the woman's identification.
#' @param weights A vetor pecifying sampling weights as an alternative to prob (1/weights).
#' @param data A data frame containing the above variables.
#'
#' @return An object of class frts_14yrs containing four arrays and a numeric.
#'
#' @examples
#'
#' ## Information from ENADID 2014, INEGI
#'
#' mg4 <- frts_14yrs(m.intvw=ENTREV_M, y.intvw=2014, m.wmn=FEC_MUJ_M, y.wmn=FEC_MUJ_A,
#'  age.wmn=EDAD_M, m.child=FEC_HIJ_M, y.child=FEC_HIJ_A, wmn.dummy=MUJER, id.wmn=ID_1,
#'  weights = FACTOR, data = enadid_2014)
#'
#' summary(mg4)
#'
#' @export
#'
frts_14yrs<- function(m.intvw, y.intvw, m.wmn,
                     y.wmn, age.wmn, m.child, y.child,
                     wmn.dummy, id.wmn,
                     weights, data){
  if(is.data.frame(data)){

      attach(data)
      db <- data.frame(m.wmn, y.wmn,
                             m.child, y.child,
                             wmn.dummy, id.wmn, weights)

      db$child.dummy <- ifelse(is.na(y.child)==FALSE, 1, 0)

      intvw.age  <-  ifelse(m.wmn >= m.intvw, age.wmn,  age.wmn-1)
      age.wmn <- NULL


      age.wmn <- intvw.age - 1
      age2.wmn <- age.wmn + 1


      db$age.wmn <- age.wmn
      db$age2.wmn <- age2.wmn
      db$intvw.age <- intvw.age
      db$expo1 <- 0
      db$expo2 <- 0

      for (i in 1:dim(db)[1]) {

        if(db$wmn.dummy[i]==TRUE){
          db$expo1[i] <- ((db$m.wmn[i]-0.5)/12)
          db$expo2[i] <- (1-db$expo1[i])
        }

      }

      db$expo1 <- db$expo1 * db$weights
      db$expo2 <- db$expo2 * db$weights


      auxiliary <- function(age, age.wmn, age2.wmn, exposition1, exposition2){
        aux <- grep(age, age.wmn, value=FALSE)
        s1 <- sum(exposition1[aux])
        aux2 <- grep(age, age2.wmn, value=FALSE)
        s2 <- sum(exposition2[aux2])
        val <- s1+s2
        return(val)
      }

      exposition <- matrix(rep(0, 560), nrow = 40, byrow = T )
      temp <- (y.intvw - 14):(y.intvw - 1)
      colnames(exposition) <- as.character(temp)
      temp2 <- 15:54
      rownames(exposition) <- as.character(temp2)


      for (i in c(15:54)) {
        exposition[i-14,14] <- auxiliary(i, db$age.wmn, db$age2.wmn, db$expo1, db$expo2)
      }

      for(i in 40:1){
        for(j in 14:1){
          exposition[i-1,j-1] <- exposition[i,j]
        }
      }

      db$expo1 <- NULL
      db$expo2 <- NULL

      gqe <- as.matrix(rbind(apply(exposition[c(1:5),c(1:14)],2,sum),
                                    apply(exposition[c(6:10),c(1:14)],2,sum),
                                    apply(exposition[c(11:15),c(1:14)],2,sum),
                                    apply(exposition[c(16:20),c(1:14)],2,sum),
                                    apply(exposition[c(21:25),c(1:14)],2,sum),
                                    apply(exposition[c(26:30),c(1:14)],2,sum),
                                    apply(exposition[c(31:35),c(1:14)],2,sum)))


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


      db$age.mother <- estimate_age(db$m.child, db$y.child, db$m.wmn, db$intvw.age, y.intvw)

      auxiliary_2 <- function(date, age, data){
        aux <- grep(date, data[ ,'y.child'], value=FALSE)
        aux2 <- data[aux, c('weights', 'age.mother')]
        aux3<-grep(age,aux2[,2],value=FALSE)
        aux4<-aux2[aux3,1:2]
        aux5<-rep(aux4[,2],aux4[,1])
        return(length(aux5))
      }


      birth <- matrix(rep(0,560), nrow = 40, byrow = T )
      temp <- (y.intvw - 14):(y.intvw - 1)
      colnames(birth) <- as.character(temp)
      temp2 <- 15:54
      rownames(birth) <- as.character(temp2)


      for (j in (y.intvw - 14):(y.intvw - 1) ) {

        for (i in c(15:54)) {

          birth[i-14, j-(y.intvw - 15)]<- auxiliary_2(j, i, db)
        }
      }

      gqb<-as.matrix(rbind(apply(birth[c(1:5),c(1:14)],2,sum),
                           apply(birth[c(6:10),c(1:14)],2,sum),
                           apply(birth[c(11:15),c(1:14)],2,sum),
                           apply(birth[c(16:20),c(1:14)],2,sum),
                           apply(birth[c(21:25),c(1:14)],2,sum),
                           apply(birth[c(26:30),c(1:14)],2,sum),
                           apply(birth[c(31:35),c(1:14)],2,sum)))


      value <- list(expo = exposition, birth = birth, gqe = gqe, gqb = gqb, y.intvw = y.intvw)

      attr(value, 'class') <- 'frts_14yrs'
      value




} else {
  stop('There is not some data.frame')

}
  }
