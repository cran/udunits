#include <stdio.h>
#include <udunits.h>
#include <string.h>

#include <R.h> 
#include <Rinternals.h>

/******************************************************************/
/* Initializes the udunits package.
 * Does NOT support the 'path' argument -- set it using UDUNITS_PATH
 * environmental variable instead!
 * Return value is 0 on success, not zero otherwise.
 */
void R_utInit( int *retval )
{
	*retval = utInit(NULL);
	if( *retval == 0 )
		return;

	if( *retval == UT_ENOFILE ) {
		fprintf( stderr, "utInit (R version): error: units file not found!\n");
		fprintf( stderr, "Set environmental variable UDUNITS_PATH to the fully\n");
		fprintf( stderr, "qualified filename of the udunits units file\n");
		fprintf( stderr, "(usually named udunits.dat, often found in /usr/local/lib)\n");
		return;
		}

	if( *retval == UT_EALLOC ) {
		fprintf( stderr, "utInit (R version): memory allocation error!!\n");
		return;
		}

	if( *retval == UT_EIO ) {
		fprintf( stderr, "utInit (R version): I/O error occurred while processing\n");
		fprintf( stderr, "the udunits file!\n" );
		return;
		}

	if( *retval == UT_EUNKNOWN ) {
		fprintf( stderr, "utInit (R version): error: udunits file contains an\n");
		fprintf( stderr, "unknown specification!\n" );
		return;
		}

	if( *retval == UT_ESYNTAX ) {
		fprintf( stderr, "utInit (R version): error: I found a syntax error\n");
		fprintf( stderr, "in the udunits file!\n" );
		return;
		}

	fprintf( stderr, "utInit (R version): error with unknown code: %d!\n",
		*retval );
}

/******************************************************************
 * Converts from the udunits library-style unit structure to the
 * R-style version of the unit structure.
 * Input:
 * 	unit_in: the udunits-library style unit to be converted
 * 		to R-style
 * Outputs:
 * 	origin_factor, hasorigin_powers: describe the udunits-processed
 * 		unit in R style.
 */
void R_ututil_utUnit_to_Rstyle( utUnit *unit_in, double *origin_factor, 
	int *hasorigin_powers )
{
	int i;

	origin_factor[0] = (double)unit_in->origin;
	origin_factor[1] = (double)unit_in->factor;

	hasorigin_powers[0] = (int)unit_in->hasorigin;
	for( i=0; i<UT_MAXNUM_BASE_QUANTITIES; i++ )
		hasorigin_powers[i+1] = (int)unit_in->power[i];
}

/******************************************************************
 * Converts from the R-style version of the unit structure to the
 * udunits library style version of the unit structure.
 * Inputs:
 * 	origin_factor, hasorigin_powers: describe the udunits-processed
 * 		unit in R style.
 * Outputs:
 * 	unit_out: the udunits-library style unit corresponding to the
 * 		passed R-style unit.
 *
 */
void R_ututil_Rstyle_to_utUnit( double *origin_factor, int *hasorigin_powers, 
	utUnit *unit_out )
{
	int i;

	unit_out->origin    = (UtOrigin)origin_factor[0];
	unit_out->factor    = (UtFactor)origin_factor[1];
	unit_out->hasorigin = (int)hasorigin_powers[0];
	for( i=0; i<UT_MAXNUM_BASE_QUANTITIES; i++ )
		unit_out->power[i] = (short)hasorigin_powers[i+1];
}

/******************************************************************/
/* Converts a formatted units string into a group of two double
 * precisions (origin, factor) and UT_MAXNUM_BASE_QUANTITIES+1
 * integers (hasorigin, then the UT_MAXNUM_BASE_QUANTITIES powers).
 * Return value is 0 on success, not zero otherwise.
 */
void R_utScan( char **spec, double *origin_factor, int *hasorigin_powers,
		int *retval )
{
	utUnit 	u;

	if( (*retval = utScan( spec[0], &u )) != 0 ) {
		if( *retval == UT_ENOINIT ) {
			fprintf( stderr, "utScan (R version): error: udunits package not initialized yet!\n");
			fprintf( stderr, "You must call utInit() first.\n" );
			return;
			}

		if( *retval == UT_EINVALID ) {
			fprintf( stderr, "utScan (R version): error: invalid unit argument!\n");
			return;
			}

		if( *retval == UT_EUNKNOWN ) {
			fprintf( stderr, "utScan (R version): error: your specification contains an unknown unit!\n" );
			return;
			}

		if( *retval == UT_ESYNTAX ) {
			fprintf( stderr, "utScan (R version): error: your specification contains a syntax error!\n" );
			return;
			}

		fprintf( stderr, "utScan (R version): unknown error %d!\n", *retval );
		return;
		}

	/* Convert unit to R-style for return */
	R_ututil_utUnit_to_Rstyle( &u, origin_factor, hasorigin_powers );
}

/******************************************************************/
/* Inputs:
 *	sx_value: guaranteed to be a double (could be >1)
 *		calendar date.
 *	sx_origin_factor, sx_hasorigin_powers: describe the 
 * 		udunits-processed time unit.
 *
 * Return value:
 * 	If only 1 value is passed, then returns an object of class "utDate"
 *	If more than 1 value passed, the returned value depends on the 'style' arg.
 *		style=1: returns a list of n objects of class "utDate"
 *		style=2: returns a list with year, month, day, hour, minute, second, each
 *			of which is an array of length n,
 *		where n is the number of values passed in.
 */
SEXP R_utCalendar_v1p2( SEXP sx_value, SEXP sx_origin_factor, SEXP sx_hasorigin_powers, SEXP sx_style )
{
	utUnit 	u;
	int 	i, nvals, retval, *hasorigin_powers, year, month, day, hour, minute, *style;
	float	second;
	double	*origin_factor, *value;
	SEXP	sx_big_retval, sx_single_retval, sx_year, sx_month, sx_day, sx_hour, sx_minute, 
		sx_name, sx_second, sx_retarr_year, sx_retarr_month, sx_retarr_day, sx_retarr_hour,
		sx_retarr_minute, sx_retarr_second;

	nvals = length( sx_value );
	if( nvals < 1 ) 
		error( "utCalendar_v1p2 (R version): error: passed funny # of vals to convert: %d\n", nvals );

	value            = REAL(sx_value);
	origin_factor    = REAL(sx_origin_factor);
	hasorigin_powers = INTEGER(sx_hasorigin_powers);
	style            = INTEGER(sx_style);

	R_ututil_Rstyle_to_utUnit( origin_factor, hasorigin_powers, &u );

	/* If we are doing multiple entries, make our return object(s) */
	if( nvals > 1 ) {

		if( *style == 1 ) 
			PROTECT( sx_big_retval = allocVector( VECSXP, nvals )); /* a list that will hold utDate objects */

		else if(*style == 2 ) {
			PROTECT( sx_retarr_year   = allocVector( INTSXP,  nvals ));
			PROTECT( sx_retarr_month  = allocVector( INTSXP,  nvals ));
			PROTECT( sx_retarr_day    = allocVector( INTSXP,  nvals ));
			PROTECT( sx_retarr_hour   = allocVector( INTSXP,  nvals ));
			PROTECT( sx_retarr_minute = allocVector( INTSXP,  nvals ));
			PROTECT( sx_retarr_second = allocVector( REALSXP, nvals ));

			PROTECT( sx_single_retval = allocVector( VECSXP, 6 )); /* a list that has year, mo, day, etc, each of which are arrays */

			SET_VECTOR_ELT( sx_single_retval, 0, sx_retarr_year   );
			SET_VECTOR_ELT( sx_single_retval, 1, sx_retarr_month  );
			SET_VECTOR_ELT( sx_single_retval, 2, sx_retarr_day    );
			SET_VECTOR_ELT( sx_single_retval, 3, sx_retarr_hour   );
			SET_VECTOR_ELT( sx_single_retval, 4, sx_retarr_minute );
			SET_VECTOR_ELT( sx_single_retval, 5, sx_retarr_second );

			/* Elements are now part of a protected element, so they don't need protection */
			UNPROTECT(6);	/* sx_retarr_year, sx_retarr_month, sx_retarr_day, sx_retarr_hour, sx_retarr_minute, sx_retarr_second */

			/* Set our names */
			PROTECT( sx_name = allocVector( STRSXP, 6 ));
			SET_STRING_ELT( sx_name, 0, mkChar("year"  ) );
			SET_STRING_ELT( sx_name, 1, mkChar("month" ) );
			SET_STRING_ELT( sx_name, 2, mkChar("day"   ) );
			SET_STRING_ELT( sx_name, 3, mkChar("hour"  ) );
			SET_STRING_ELT( sx_name, 4, mkChar("minute") );
			SET_STRING_ELT( sx_name, 5, mkChar("second") );
			setAttrib( sx_single_retval, R_NamesSymbol, sx_name );
			UNPROTECT(1);	/* done with sx_name */
			}

		else
			error( "utCalendar_v1p2 (R version): error: passed unknown style, only 1 or 2 recognized!\n" );
		}

	for( i=0; i<nvals; i++ ) {
		if( (retval = utCalendar( *(value+i), &u, &year, &month,
				&day, &hour, &minute, &second )) != 0 ) {
			if( retval == UT_ENOINIT ) 
				error( "utCalendar (R version): error: udunits package not initialized yet!  You must call utInit() first." );

			else if( retval == UT_EINVALID )
				error( "utCalendar (R version): error: units are not time-like!" );

			else
				error( "utCalendar (R version): unknown error %d!\n", retval );
			}

		if( (nvals==1) || (*style==1) ) {
			/* Make this individual entry's utDate object */
			PROTECT( sx_single_retval = allocVector( VECSXP, 6 ));

			PROTECT( sx_year   = allocVector( INTSXP,  1 )); INTEGER(sx_year  )[0] = year;
			PROTECT( sx_month  = allocVector( INTSXP,  1 )); INTEGER(sx_month )[0] = month;
			PROTECT( sx_day    = allocVector( INTSXP,  1 )); INTEGER(sx_day   )[0] = day;
			PROTECT( sx_hour   = allocVector( INTSXP,  1 )); INTEGER(sx_hour  )[0] = hour;
			PROTECT( sx_minute = allocVector( INTSXP,  1 )); INTEGER(sx_minute)[0] = minute;
			PROTECT( sx_second = allocVector( REALSXP, 1 )); REAL   (sx_second)[0] = second;

			SET_VECTOR_ELT( sx_single_retval, 0, sx_year   );
			SET_VECTOR_ELT( sx_single_retval, 1, sx_month  );
			SET_VECTOR_ELT( sx_single_retval, 2, sx_day    );
			SET_VECTOR_ELT( sx_single_retval, 3, sx_hour   );
			SET_VECTOR_ELT( sx_single_retval, 4, sx_minute );
			SET_VECTOR_ELT( sx_single_retval, 5, sx_second );

			/* Elements are now part of a protected element, so they don't need protection */
			UNPROTECT(6);	/* sx_year, sx_month, sx_day, sx_hour, sx_minute, sx_second */

			/* Set our names */
			PROTECT( sx_name = allocVector( STRSXP, 6 ));
			SET_STRING_ELT( sx_name, 0, mkChar("year"  ) );
			SET_STRING_ELT( sx_name, 1, mkChar("month" ) );
			SET_STRING_ELT( sx_name, 2, mkChar("day"   ) );
			SET_STRING_ELT( sx_name, 3, mkChar("hour"  ) );
			SET_STRING_ELT( sx_name, 4, mkChar("minute") );
			SET_STRING_ELT( sx_name, 5, mkChar("second") );
			setAttrib( sx_single_retval, R_NamesSymbol, sx_name );
			UNPROTECT(1);	/* done with sx_name */

			/* Set our class name */
			PROTECT( sx_name = allocVector( STRSXP, 1 ));
			SET_STRING_ELT( sx_name, 0, mkChar("utDate"));
			setAttrib( sx_single_retval, R_ClassSymbol, sx_name );
			UNPROTECT(1);   /* done with sx_name */

			/* If only 1 value to convert, we are all done now! */
			if( nvals == 1 ) {
				UNPROTECT(1);
				return(sx_single_retval);
				}

			/* ...otherwise, add this new utDate to the list we are accumulating */
			SET_VECTOR_ELT( sx_big_retval, i, sx_single_retval );
			UNPROTECT(1);	/* sx_single_retval is now protected by its parent */
			}
		else
			{
			/* style == 2 */
			INTEGER(sx_retarr_year  )[i] = year;
			INTEGER(sx_retarr_month )[i] = month;
			INTEGER(sx_retarr_day   )[i] = day;
			INTEGER(sx_retarr_hour  )[i] = hour;
			INTEGER(sx_retarr_minute)[i] = minute;
			REAL   (sx_retarr_second)[i] = second;
			}
		}

	/* NOTE! if nvals==1, we exit ABOVE, not here */
	if( *style == 1 ) {
		UNPROTECT(1);
		return( sx_big_retval );
		}
	else
		{
		UNPROTECT(1);
		return( sx_single_retval );
		}
}

/******************************************************************/
/* Inputs:
 *	value: the amount of time units we want to convert to a
 *		calendar date.
 *	nvals: number of values that are being input.
 *	origin_factor, hasorigin_powers: describe the udunits-processed
 *		time unit.
 * Outputs:
 *	ymdhm: integer year, month, day, hour, and minute.
 *	second: seconds
 *	retval: 0 on success, not 0 otherwise.
 */
void R_utCalendar( double *value, int *nvals, double *origin_factor, int *hasorigin_powers,
	int *year, int *month, int *day, int *hour, int *minute, double *second, int *retval )
{
	utUnit 	u;
	float	f_sec;
	int 	i;

	if( *nvals < 1 ) {	
		fprintf( stderr, "utCalendar (R version): error: passed funny # of vals to convert: %d\n", *nvals );
		*retval = -1;
		return;
		}

	R_ututil_Rstyle_to_utUnit( origin_factor, hasorigin_powers, &u );

	for( i=0; i<(*nvals); i++ ) {
		if( (*retval = utCalendar( *(value+i), &u, (year+i), (month+i),
				(day+i), (hour+i), (minute+i), &f_sec )) != 0 ) {
			if( *retval == UT_ENOINIT ) {
				fprintf( stderr, "utCalendar (R version): error: udunits package not initialized yet!\n");
				fprintf( stderr, "You must call utInit() first.\n" );
				return;
				}

			if( *retval == UT_EINVALID ) {
				fprintf( stderr, "utCalendar (R version): error: units are not time-like!\n" );
				return;
				}

			fprintf( stderr, "utCalendar (R version): unknown error %d!\n",
				*retval );

			return;
			}
		*(second+i) = (double)f_sec;
		}
}

/******************************************************************/
/* Note this is not the same thing as being a calendar.  A calendar
 * has BOTH a time unit and an origin.
 */
void R_utIsTime( double *origin_factor, int *hasorigin_powers, int *retval )
{
	utUnit u;

	R_ututil_Rstyle_to_utUnit( origin_factor, hasorigin_powers, &u );
	*retval = utIsTime( &u );
}

/******************************************************************/
void R_utHasOrigin( double *origin_factor, int *hasorigin_powers, int *retval )
{
	utUnit u;

	R_ututil_Rstyle_to_utUnit( origin_factor, hasorigin_powers, &u );
	*retval = utHasOrigin( &u );
}

/******************************************************************/
/* Converts a given year, month, day, hour, minute, and second
 * into a value, given an appropriate time unit to work with.
 * Inputs:
 *	ndates: number of dates to convert
 *	year: array of years
 *	month: array of months
 *	day: array of days
 *	hour: array of hours
 *	minute: array of minutes
 *	second: array of seconds (double precision value)
 *	origin_factor, hasorigin_powers: describe the udunit
 * Outputs:
 * 	value: what that date corresponds to, in the described units.
 *	retval: 0 if no error, not 0 if an error.
 */
void R_utInvCalendar( int *ndates, int *year, int *month, int *day, int *hour,
	int *minute, double *second, double *origin_factor, 
	int *hasorigin_powers, double *value, int *retval )
{
	utUnit 	u;
	int i;

	R_ututil_Rstyle_to_utUnit( origin_factor, hasorigin_powers, &u );

	for( i=0; i<*ndates; i++ ) {
		if( (*retval = utInvCalendar( year[i], month[i], day[i],
				hour[i], minute[i], second[i], &u, (value+i) )) != 0 ) {
			if( *retval == UT_ENOINIT ) {
				fprintf( stderr, "utInvCalendar (R version): error: udunits package not initialized yet!\n");
				fprintf( stderr, "You must call utInit() first.\n" );
				return;
				}

			if( *retval == UT_EINVALID ) {
				fprintf( stderr, "utInvCalendar (R version): error: units are not temporal!\n" );
				return;
				}

			fprintf( stderr, "utInvCalendar (R version): unknown error %d!\n",
				*retval );
			return;
			}
		}
}

/******************************************************************
 * Returns the coefficients for the linear transformation between
 * the "from" units and the "to" units.
 * Inputs:
 * 	from_origin_factor, from_hasorigin_powers: describe the udunit
 * 		we are converting *from*
 * 	to_origin_factor, to_hasorigin_powers: describe the udunit we
 * 		are converting *to*
 * Outputs:
 * 	slope, intercept: values for transforming the units
 * 	retval: 0 if no error, not 0 if an error
 *
 */
void R_utConvert( double *origin_factor_from, int *hasorigin_powers_from,
	double *origin_factor_to, int *hasorigin_powers_to,
	double *slope, double *intercept, int *retval )
{
	utUnit	u_from, u_to;

	R_ututil_Rstyle_to_utUnit( origin_factor_from, hasorigin_powers_from, &u_from );
	R_ututil_Rstyle_to_utUnit( origin_factor_to, hasorigin_powers_to, &u_to );

	if( (*retval = utConvert( &u_from, &u_to, slope, intercept )) != 0 ) {
		if( *retval == UT_ENOINIT ) {
			fprintf( stderr, "utConvert (R version): error: udunits package not initialized yet!\n");
			fprintf( stderr, "You must call utInit() first.\n" );
			return;
			}

		if( *retval == UT_EINVALID ) {
			fprintf( stderr, "utConvert (R version): error: passed an invalid unit structure!\n" );
			return;
			}

		if( *retval == UT_ECONVERT ) {
			fprintf( stderr, "utConvert (R version): error: units are incompatible, cannot convert between them!\n" );
			return;
			}

		fprintf( stderr, "utConvert (R version): unknown error %d!\n", *retval );
		return;
		}
}

