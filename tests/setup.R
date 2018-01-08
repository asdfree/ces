if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
this_sample_break <- Sys.getenv( "this_sample_break" )
ces_cat <- get_catalog( "ces" , output_dir = file.path( getwd() ) )
record_categories <- ceiling( seq( nrow( ces_cat ) ) / ceiling( nrow( ces_cat ) / 3 ) )
ces_cat <- ces_cat[ record_categories == this_sample_break , ]
lodown( "ces" , ces_cat )
if( any( ces_cat$year == 2016 ) ){
library(lodown)
# examine all available CES microdata files
ces_cat <-
	get_catalog( "ces" ,
		output_dir = file.path( getwd() ) )

# 2016 only
ces_cat <- subset( ces_cat , year == 2016 )
# download the microdata to your local computer


options( survey.replicates.mse = TRUE )

library(survey)
library(mitools)

# read in the five quarters of family data files (fmli)

fmli161x <- readRDS( file.path( getwd() , "2016/fmli161x.rds" ) )
fmli162 <- readRDS( file.path( getwd() , "2016/fmli162.rds" ) )
fmli163 <- readRDS( file.path( getwd() , "2016/fmli163.rds" ) )
fmli164 <- readRDS( file.path( getwd() , "2016/fmli164.rds" ) )
fmli171 <- readRDS( file.path( getwd() , "2016/fmli171.rds" ) )

fmli161x$qtr <- 1
fmli162$qtr <- 2
fmli163$qtr <- 3
fmli164$qtr <- 4
fmli171$qtr <- 5

fmli171 <- fmli171[ , names( fmli161x ) ]

fmly <- rbind( fmli161x , fmli162 , fmli163 , fmli164 , fmli171 )

rm( fmli161x , fmli162 , fmli163 , fmli164 , fmli171 )

wtrep <- c( paste0( "wtrep" , stringr::str_pad( 1:44 , 2 , pad = "0" ) ) , "finlwt21" )

for ( i in wtrep ) fmly[ is.na( fmly[ , i ] ) , i ] <- 0

# create a new variable in the fmly data table called 'totalexp'
# that contains the sum of the total expenditure from the current and previous quarters
fmly$totalexp <- rowSums( fmly[ , c( "totexppq" , "totexpcq" ) ] , na.rm = TRUE )

# immediately convert missing values (NA) to zeroes
fmly[ is.na( fmly$totalexp ) , "totalexp" ] <- 0

# annualize the total expenditure by multiplying the total expenditure by four,
# creating a new variable 'annexp' in the fmly data table
fmly <- transform( fmly , annexp = totalexp * 4 )

# add a column of ones
fmly$one <- 1

# create a vector containing all of the multiply-imputed variables
# (leaving the numbers off the end)
mi_vars <- gsub( "5$" , "" , grep( "[a-z]5$" , names( fmly ) , value = TRUE ) )

# loop through each of the five variables..
for ( i in 1:5 ){

	# copy the 'fmly' table over to a new temporary data frame 'x'
	x <- fmly

	# loop through each of the multiply-imputed variables..
	for ( j in mi_vars ){
	
		# copy the contents of the current column (for example 'welfare1')
		# over to a new column ending in 'mi' (for example 'welfaremi')
		x[ , paste0( j , 'mi' ) ] <- x[ , paste0( j , i ) ]
		
		# delete the all five of the imputed variable columns
		x <- x[ , !( names( x ) %in% paste0( j , 1:5 ) ) ]

	}
	
	# save the current table in the sqlite database as 'imp1' 'imp2' etc.
	assign( paste0( 'imp' , i ) , x )

	# remove the temporary table
	rm( x )
	
}

	
# containing the five multiply-imputed data tables - imp1 through imp5
ces_design <- 
	svrepdesign( 
		weights = ~finlwt21 , 
		repweights = "wtrep[0-9]+" , 
		data = imputationList( list( imp1 , imp2 , imp3 , imp4 , imp5 ) ) , 
		type = "BRR" ,
		combined.weights = TRUE
	)

rm( imp1 , imp2 , imp3 , imp4 , imp5 )
ces_design <- 
	update( 
		ces_design , 
		
		any_food_stamp = as.numeric( jfs_amtmi > 0 )
		
	)
MIcombine( with( ces_design , svyby( ~ one , ~ one , unwtd.count ) ) )

MIcombine( with( ces_design , svyby( ~ one , ~ bls_urbn , unwtd.count ) ) )
MIcombine( with( ces_design , svytotal( ~ one ) ) )

MIcombine( with( ces_design ,
	svyby( ~ one , ~ bls_urbn , svytotal )
) )
MIcombine( with( ces_design , svymean( ~ annexp ) ) )

MIcombine( with( ces_design ,
	svyby( ~ annexp , ~ bls_urbn , svymean )
) )
MIcombine( with( ces_design , svymean( ~ sex_ref ) ) )

MIcombine( with( ces_design ,
	svyby( ~ sex_ref , ~ bls_urbn , svymean )
) )
MIcombine( with( ces_design , svytotal( ~ annexp ) ) )

MIcombine( with( ces_design ,
	svyby( ~ annexp , ~ bls_urbn , svytotal )
) )
MIcombine( with( ces_design , svytotal( ~ sex_ref ) ) )

MIcombine( with( ces_design ,
	svyby( ~ sex_ref , ~ bls_urbn , svytotal )
) )
MIcombine( with( ces_design , svyquantile( ~ annexp , 0.5 , se = TRUE ) ) )

MIcombine( with( ces_design ,
	svyby( 
		~ annexp , ~ bls_urbn , svyquantile , 0.5 ,
		se = TRUE , keep.var = TRUE , ci = TRUE 
) ) )
MIcombine( with( ces_design ,
	svyratio( numerator = ~ annexp , denominator = ~ fincbtxmi )
) )
sub_ces_design <- subset( ces_design , state == '06' )
MIcombine( with( sub_ces_design , svymean( ~ annexp ) ) )
this_result <-
	MIcombine( with( ces_design ,
		svymean( ~ annexp )
	) )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	MIcombine( with( ces_design ,
		svyby( ~ annexp , ~ bls_urbn , svymean )
	) )

coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( ces_design$designs[[1]] )
MIcombine( with( ces_design , svyvar( ~ annexp ) ) )
# SRS without replacement
MIcombine( with( ces_design ,
	svymean( ~ annexp , deff = TRUE )
) )

# SRS with replacement
MIcombine( with( ces_design ,
	svymean( ~ annexp , deff = "replace" )
) )
lodown:::MIsvyciprop( ~ any_food_stamp , ces_design ,
	method = "likelihood" )
lodown:::MIsvyttest( annexp ~ any_food_stamp , ces_design )
lodown:::MIsvychisq( ~ any_food_stamp + sex_ref , ces_design )
glm_result <- 
	MIcombine( with( ces_design ,
		svyglm( annexp ~ any_food_stamp + sex_ref )
	) )
	
summary( glm_result )

}
