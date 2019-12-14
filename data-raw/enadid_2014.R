# These dbfs (TMMujer1.dbf y TFec_Hemb.dbf) are available from:
# https://www.inegi.org.mx/programas/enadid/2014/default.html#Microdatos

library(foreign)
library(dplyr)

# read the dbf files
temporal <- tempfile()
download.file("http://www.inegi.org.mx/contenidos/programas/enadid/2014/microdatos/base_datos_enadid14_dbf.zip",temporal)
files = unzip(temporal, list=TRUE)$Name
unzip(temporal, files=files[grepl("dbf",files)])
muj_14 <- data.frame(read.dbf("TMMujer1.dbf"))
hist_14 <- data.frame(read.dbf('TFec_Hemb.dbf'))

# delete the abortion and stillbirth s's registro
hist_14 <- hist_14[which(is.na(hist_14[,'P5_20'])==T &  is.na(hist_14[,"P5_24"])==T), ]


# change the data types
muj_14 <- transform(muj_14, ID_1 = LLAVE_MUJ, FACTOR = as.numeric(as.character(FAC_PER)),
                    UPM = as.numeric(as.character(UPM_DIS)), ESTRATO = as.numeric(as.character(EST_DIS)),
                    FEC_MUJ_M = as.numeric(as.character(P5_1_1)), FEC_MUJ_A = as.numeric(as.character(P5_1_2)),
                    EDAD_M = as.numeric(as.character(P5_2)))
hist_14 <- transform(hist_14, ID_2 = LLAVE_MUJ, FEC_HIJ_M = as.numeric(as.character(P5_17_1)),
                     FEC_HIJ_A = as.numeric(as.character(P5_17_2)))


# select variables
muj_14 <- muj_14[,c('ID_1',"FACTOR",'UPM','ESTRATO','FEC_MUJ_M','FEC_MUJ_A', 'EDAD_M')]
hist_14 <- hist_14[,c('ID_2','FEC_HIJ_M','FEC_HIJ_A')]


# remove unspecified values in birth years
table(muj_14$FEC_MUJ_A); table(is.na(muj_14$FEC_MUJ_A))
table(hist_14$FEC_HIJ_A); table(is.na(hist_14$FEC_HIJ_A)) # 1031 (9999 is unspecified value )

hist_14 <- hist_14[which(hist_14$FEC_HIJ_A!="9999"), ] # remove the values


# apportion unspecified values in birth months
table(muj_14$FEC_MUJ_M); table(is.na(muj_14$FEC_MUJ_M)) #159 (99 is unspecified value)
table(hist_14$FEC_HIJ_M); table(is.na(hist_14$FEC_HIJ_M)) # 2014 (99 is unspecified value)

hist_14$FEC_HIJ_M[which(hist_14$FEC_HIJ_M == '99')] <- round(runif(2014, 1, 12), 0) # uniform
muj_14$FEC_MUJ_M[which(muj_14$FEC_MUJ_M=='99')] <- round(runif(159, 1, 12), 0) # uniform


# set interview month
muj_14$ENTREV_M <- as.numeric(09)



# full join database and which elements of data frame are duplicates
enadid_2014 <- data.frame()
enadid_2014 <- full_join(muj_14, hist_14, by = c('ID_1' = 'ID_2')) # where there are not matching values, returns NA
enadid_2014$MUJER <- !duplicated(enadid_2014$ID_1)



save(enadid_1994, file = 'enadid_1992.RData', compress = "xz")
usethis::use_data(enadid_2014, overwrite = TRUE)
