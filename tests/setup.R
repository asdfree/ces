# price indices and
# you spent how much on beans, jack?
# pocketbook issues
library(httr)

tf_prior_year <- tempfile()

this_url_prior_year <- "https://www.bls.gov/cex/pumd/data/stata/intrvw21.zip"

dl_prior_year <- GET( this_url_prior_year , user_agent( "email@address.com" ) )

writeBin( content( dl_prior_year ) , tf_prior_year )

unzipped_files_prior_year <- unzip( tf_prior_year , exdir = tempdir() )

tf_current_year <- tempfile()

this_url_current_year <- "https://www.bls.gov/cex/pumd/data/stata/intrvw22.zip"

dl_current_year <- GET( this_url_current_year , user_agent( "email@address.com" ) )

writeBin( content( dl_current_year ) , tf_current_year )

unzipped_files_current_year <- unzip( tf_current_year , exdir = tempdir() )

unzipped_files <- c( unzipped_files_current_year , unzipped_files_prior_year )
library(haven)

fmli_files <- grep( "fmli2[2-3]" , unzipped_files , value = TRUE )

fmli_tbls <- lapply( fmli_files , read_dta )

fmli_dfs <- lapply( fmli_tbls , data.frame )

fmli_dfs <- 
	lapply( 
		fmli_dfs , 
		function( w ){ names( w ) <- tolower( names( w ) ) ; w }
	)

fmli_cols <- lapply( fmli_dfs , names )

intersecting_cols <- Reduce( intersect , fmli_cols )

fmli_dfs <- lapply( fmli_dfs , function( w ) w[ intersecting_cols ] )

ces_df <- do.call( rbind , fmli_dfs )
ces_df[ , c( 'qintrvyr' , 'qintrvmo' ) ] <-
	sapply( ces_df[ , c( 'qintrvyr' , 'qintrvmo' ) ] , as.numeric )

weight_columns <- grep( 'wt' , names( ces_df ) , value = TRUE )

ces_df <-
	transform(
		ces_df ,
		mo_scope =
			ifelse( qintrvyr %in% 2022 & qintrvmo %in% 1:3 , qintrvmo - 1 ,
			ifelse( qintrvyr %in% 2023 , 4 - qintrvmo , 3 ) )
	)

for ( this_column in weight_columns ){
	ces_df[ is.na( ces_df[ , this_column ] ) , this_column ] <- 0
	
	ces_df[ , paste0( 'popwt_' , this_column ) ] <-
		( ces_df[ , this_column ] * ces_df[ , 'mo_scope' ] / 12 )	
	
}

expenditure_variables <- 
	gsub( "pq$" , "" , grep( "pq$" , names( ces_df ) , value = TRUE ) )

# confirm that for every variable ending in pq,
# there's the same variable ending in cq
stopifnot( all( paste0( expenditure_variables , 'cq' ) %in% names( ces_df ) ) )

# confirm none of the variables without the pq or cq suffix exist
if( any( expenditure_variables %in% names( ces_df ) ) ) stop( "variable conflict" )

for( this_column in expenditure_variables ){

	ces_df[ , this_column ] <-
		rowSums( ces_df[ , paste0( this_column , c( 'pq' , 'cq' ) ) ] , na.rm = TRUE )
	
	# annualize the quarterly spending
	ces_df[ , this_column ] <- 4 * ces_df[ , this_column ]
	
	ces_df[ is.na( ces_df[ , this_column ] ) , this_column ] <- 0

}
ucc_exp <- c( "450110" , "450210" )

mtbi_files <- grep( "mtbi2[2-3]" , unzipped_files , value = TRUE )

mtbi_tbls <- lapply( mtbi_files , read_dta )

mtbi_dfs <- lapply( mtbi_tbls , data.frame )

mtbi_dfs <- 
	lapply( 
		mtbi_dfs , 
		function( w ){ names( w ) <- tolower( names( w ) ) ; w }
	)

mtbi_dfs <- lapply( mtbi_dfs , function( w ) w[ c( 'newid' , 'cost' , 'ucc' , 'ref_yr' ) ] )

mtbi_df <- do.call( rbind , mtbi_dfs )

mtbi_df <- subset( mtbi_df , ( ref_yr %in% 2022 ) & ( ucc %in% ucc_exp ) )

mtbi_agg <- aggregate( cost ~ newid , data = mtbi_df , sum )

names( mtbi_agg ) <- c( 'newid' , 'new_car_truck_exp' )

before_nrow <- nrow( ces_df )

ces_df <-
	merge(
		ces_df ,
		mtbi_agg ,
		all.x = TRUE
	)

stopifnot( nrow( ces_df ) == before_nrow )

ces_df[ is.na( ces_df[ , 'new_car_truck_exp' ] ) , 'new_car_truck_exp' ] <- 0
# ces_fn <- file.path( path.expand( "~" ) , "CES" , "this_file.rds" )
# saveRDS( ces_df , file = ces_fn , compress = FALSE )
# ces_df <- readRDS( ces_fn )
library(survey)
library(mitools)

# create a vector containing all of the multiply-imputed variables
# (leaving the numbers off the end)
mi_vars <- gsub( "5$" , "" , grep( "[a-z]5$" , names( ces_df ) , value = TRUE ) )

# loop through each of the five variables..
for ( i in 1:5 ){

	# copy the 'ces_df' table over to a new temporary data frame 'x'
	x <- ces_df

	# loop through each of the multiply-imputed variables..
	for ( j in mi_vars ){
	
		# copy the contents of the current column (for example 'welfare1')
		# over to a new column ending in 'mi' (for example 'welfaremi')
		x[ , paste0( j , 'mi' ) ] <- x[ , paste0( j , i ) ]
		
		# delete the all five of the imputed variable columns
		x <- x[ , !( names( x ) %in% paste0( j , 1:5 ) ) ]

	}
	
	assign( paste0( 'imp' , i ) , x )

}

ces_design <- 
	svrepdesign( 
		weights = ~ finlwt21 , 
		repweights = "^wtrep[0-9][0-9]$" , 
		data = imputationList( list( imp1 , imp2 , imp3 , imp4 , imp5 ) ) , 
		type = "BRR" ,
		combined.weights = TRUE ,
		mse = TRUE
	)
ces_design <- 
	update( 
		ces_design , 
		
		one = 1 ,
		
		any_food_stamp = as.numeric( jfs_amtmi > 0 ) ,
		
		bls_urbn = factor( bls_urbn , levels = 1:2 , labels = c( 'urban' , 'rural' ) ) ,
		
		sex_ref = factor( sex_ref , levels = 1:2 , labels = c( 'male' , 'female' ) )
		
	)
MIcombine( with( ces_design , svyby( ~ one , ~ one , unwtd.count ) ) )

MIcombine( with( ces_design , svyby( ~ one , ~ bls_urbn , unwtd.count ) ) )
MIcombine( with( ces_design , svytotal( ~ one ) ) )

MIcombine( with( ces_design ,
	svyby( ~ one , ~ bls_urbn , svytotal )
) )
MIcombine( with( ces_design , svymean( ~ totexp ) ) )

MIcombine( with( ces_design ,
	svyby( ~ totexp , ~ bls_urbn , svymean )
) )
MIcombine( with( ces_design , svymean( ~ sex_ref ) ) )

MIcombine( with( ces_design ,
	svyby( ~ sex_ref , ~ bls_urbn , svymean )
) )
MIcombine( with( ces_design , svytotal( ~ totexp ) ) )

MIcombine( with( ces_design ,
	svyby( ~ totexp , ~ bls_urbn , svytotal )
) )
MIcombine( with( ces_design , svytotal( ~ sex_ref ) ) )

MIcombine( with( ces_design ,
	svyby( ~ sex_ref , ~ bls_urbn , svytotal )
) )
MIcombine( with( ces_design ,
	svyquantile(
		~ totexp ,
		0.5 , se = TRUE 
) ) )

MIcombine( with( ces_design ,
	svyby(
		~ totexp , ~ bls_urbn , svyquantile ,
		0.5 , se = TRUE ,
		ci = TRUE 
) ) )
MIcombine( with( ces_design ,
	svyratio( numerator = ~ totexp , denominator = ~ fincbtxmi )
) )
sub_ces_design <- subset( ces_design , state == '06' )
MIcombine( with( sub_ces_design , svymean( ~ totexp ) ) )
this_result <-
	MIcombine( with( ces_design ,
		svymean( ~ totexp )
	) )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	MIcombine( with( ces_design ,
		svyby( ~ totexp , ~ bls_urbn , svymean )
	) )

coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( ces_design$designs[[1]] )
MIcombine( with( ces_design , svyvar( ~ totexp ) ) )
# SRS without replacement
MIcombine( with( ces_design ,
	svymean( ~ totexp , deff = TRUE )
) )

# SRS with replacement
MIcombine( with( ces_design ,
	svymean( ~ totexp , deff = "replace" )
) )
# MIsvyciprop( ~ any_food_stamp , ces_design ,
# 	method = "likelihood" )
# MIsvyttest( totexp ~ any_food_stamp , ces_design )
# MIsvychisq( ~ any_food_stamp + sex_ref , ces_design )
glm_result <- 
	MIcombine( with( ces_design ,
		svyglm( totexp ~ any_food_stamp + sex_ref )
	) )
	
summary( glm_result )
result <-
	MIcombine( with( ces_design , svytotal( ~ as.numeric( popwt_finlwt21 / finlwt21 ) ) ) )

stopifnot( round( coef( result ) , -3 ) == 134090000 )

results <- 
	sapply( 
		weight_columns , 
		function( this_column ){
			sum( ces_df[ , 'new_car_truck_exp' ] * ces_df[ , this_column ] ) / 
			sum( ces_df[ , paste0( 'popwt_' , this_column ) ] )
		}
	)

stopifnot( round( results[1] , 2 ) == 2195.30 )

standard_error <- sqrt( ( 1 / 44 ) * sum( ( results[-1] - results[1] )^2 ) )

stopifnot( round( standard_error , 2 ) == 174.02 )

# note the minor differences
MIcombine( with( ces_design , svymean( ~ cartkn ) ) )
