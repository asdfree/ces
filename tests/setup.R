if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)

ces_cat <-
	get_catalog( "ces" ,
		output_dir = file.path( getwd() ) )

# sample 75% of the records
which_records <- sample( seq( nrow( ces_cat ) ) , round( nrow( ces_cat ) * 0.75 ) )

# always sample year == 2016
ces_cat <- unique( rbind( ces_cat[ which_records , ] , subset( ces_cat , year == 2016 ) ) )

lodown( "ces" , ces_cat )
