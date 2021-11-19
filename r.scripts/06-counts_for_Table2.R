# for table 2

# Source
iw <- aggregate(ID~indig_or_wstrn, df, FUN="length")
iw$ID[1]/nrow(df) # 0.12

ipn <- aggregate(ID~is_oipn, df, FUN="length")
ipn$ID[6]/nrow(df) # 0.0455

nrow(df[(df$is_oipn %in% c("yes", "yes but new feature")) & df$is_natural=="Natural",])
107/nrow(df)


# Derogatory
der <- aggregate(ID~derog, df, FUN="length")
der

# Erasure
era <- aggregate(ID~erasure, df, FUN="length")
era

# Dimensions of colonialism and racism
dim <- aggregate(ID~problem, df, FUN="length")
dim

no <- df[(df$derog=="No info - not likely, no evidence") &
           ((df$erasure==era[2,1]) | (df$erasure==era[1,1])) &
           ((df$problem==dim[5,1]) | (df$problem==dim[6,1]) | (df$problem==dim[7,1])),]

nrow(no)/nrow(df)  # 0.209
