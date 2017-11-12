if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

ces_cat <-
	get_catalog( "ces" ,
		output_dir = file.path( getwd() ) )

record_categories <- ceiling( seq( nrow( ces_cat ) ) / ceiling( nrow( ces_cat ) / 3 ) )

ces_cat <- unique( rbind( ces_cat[ record_categories == this_sample_break , ] , ces_cat[ ces_cat$year == 2016 , ] ) )

lodown( "ces" , ces_cat )
