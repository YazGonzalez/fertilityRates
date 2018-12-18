mg1 <- frts_intvw(m.intvw=ENTREV_M, y.intvw=2014, m.wmn=FEC_MUJ_M, y.wmn=FEC_MUJ_A, age.wmn=EDADD, m.child=FEC_HIJ_M,
                   y.child=FEC_HIJ_A, child.dummy=CONT,wmn.dummy=MUJER, id.wmn=ID_1, ids=UPM, strata=ESTRATO,
                   weights = FACTOR, data = enadid_2014)

summary(mg1, level = 0.9)


mg3 <- frts_3yrs(m.intvw=ENTREV_M , y.intvw=2014, m.wmn=FEC_MUJ_M, y.wmn=FEC_MUJ_A, age.wmn=EDADD,
                 m.child=FEC_HIJ_M, y.child=FEC_HIJ_A, child.dummy=CONT, wmn.dummy=MUJER, id.wmn=ID_1,
                 ids=UPM, strata=ESTRATO, weights=FACTOR, data=enadid_2014)

mg3.s=summary(mg3, level = 0.9)
View(mg3.s$as_fr_g)
