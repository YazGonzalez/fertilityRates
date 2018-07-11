#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'




frts_yrly<- function(y.ref, m.wmn,
                      y.wmn, m.child, y.child, children,
                      child.dummy, wmn.dummy, id.wmn, ids,
                      strata, weights, data, ...){
  if(is.data.frame(data)){
    if (is.element("survey", installed.packages()[,1])) {
      attach(data)
      database <- data.frame(m.wmn, y.wmn, #PENDIENTE
                             m.child, y.child,
                             children, child.dummy,
                             wmn.dummy, id.wmn)


      age.wmn <- y.ref-y.wmn-1
      age2.wmn <- y.ref-y.wmn
      database$age.wmn <- age.wmn
      database$age2.wmn <- age2.wmn
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

      database$expo1 <- NULL
      database$expo2 <- NULL


      database <- cbind(database,data)

      estimate_age <- function(m.child, y.child, m.wmn, y.wmn){
        random <- rbinom(length(m.child),1,0.5)
        age <- NULL
        for(i in 1:length(m.child)){
          if(is.na(m.child[i]) == F){
            if(m.child[i] > m.wmn[i] | m.child[i] < m.wmn[i]){
              age=c(age, ceiling((12*(y.child[i]-y.wmn[i]) + (m.child[i]-m.wmn[i]))/12))
            }
            else{
              age=c(age, ceiling((12*(y.child[i]-y.wmn[i]) + (m.child[i]-m.wmn[i]-rbinom(1,1,0.5)))/12))
            }
          }
          else{age=c(age,0)}
        }
        return(age)
      }

      database$age.mother <- estimate_age(database$m.child, database$y.child, database$m.wmn, database$y.wmn)
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

      attr(value, 'class') <- 'frts_yrly'
      value



    } else {
      stop("Library 'survey' must be installed")
    }
  } else {
    stop('There is not some data.frame')

  }

}








