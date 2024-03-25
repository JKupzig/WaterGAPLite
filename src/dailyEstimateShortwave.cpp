#include <Rcpp.h>
#include <math.h>

using namespace Rcpp;

//' @title Calculate shortwave radiation (after Kaspar 2004)
//' @description rcpp function to estimate Shortwave radiation when not given as measured input
//' @param dates_of_simulation Datevector of Simulation period
//' @param temperature Temperatur as NumericMatrix in degree
//' @param sunshine_duration Sunshine duration duration as NumericMatrix  in hours
//' @param row_information_GR information of row for cells
//' @param corrected_row_for_continent information of correction of rows for continental grid
//' @return shortwave_downward_radiation Matrix with estimated shortwave radiation in W/m²
//' @export
// [[Rcpp::export(rng = false)]]
NumericMatrix dailyEstimateShortwave(
	DateVector dates_of_simulation,
	NumericMatrix temperature,
	NumericMatrix sunshine_duration,
	IntegerVector row_information_GR,
	int corrected_row_for_continent)
	{

	// constants
	const int cells_in_degree = 12;
	const double a_s = 0.25;	// a_s: fraction of extraterrestrial radiation on overcast days
	const double b_s = 0.5;	    // a_s + b_s: fraction of extraterrestrial radiation on clear days
	const double pi = 3.141592653589793;
	const double pi_180 = pi / 180.0;
	const double pi2_365 = 2. * pi / 365.0;
	int array_size = sunshine_duration.ncol();
	int ndays = sunshine_duration.nrow();

	NumericMatrix shortwave_downward_radiation(ndays, array_size); // mm/d
	double daily_sunshine_duration; // hour
	Date date_of_simulation; // date
	int DOY; // 1 - 365
	double solar_declination_angle; // in radians
	double latitude_of_the_site; // in radians
	double sunset_hour_angle; // in radians
	double possible_sunshine_duration; // after Forsythe 1995 hours
	double distance_earth_sun;
	double extraterrestrial_radiation; // [mm/day]
	double shortwave_downward; // Wm2
	double daily_temperature; // degree celsius
	double lat_heat; // [MJ/kg]
	double convert_Wm2_to_mmd ; // -


	for (int col = 0; col < array_size; col++)
	{
		int row = row_information_GR[col];

		for (int day=0; day < ndays; day++)
		{

			daily_sunshine_duration = sunshine_duration(day,col);
			daily_temperature = temperature(day,col);
			date_of_simulation = dates_of_simulation[day];

			// small differences in computed PET will arrive when leap year (29.02)
			// is neglected in model settings and long/shortwave downward radiation is estimated
			DOY = std::min(date_of_simulation.getYearday(), 365);

			lat_heat = 2.835; // latent heat of sublimation
			if (daily_temperature > 0)
			{
				// latent heat of vaporization of water
				lat_heat = 2.501 - 0.002361 * daily_temperature;
			}
			convert_Wm2_to_mmd = 0.0864 / lat_heat;

			solar_declination_angle = asin(0.39795 *
										 cos(0.2163108 +
										 	2. * atan(
												0.9671396 * tan(
													0.00860 * (DOY - 186)))));

			latitude_of_the_site =  pi_180 * -(
				(row + corrected_row_for_continent) / cells_in_degree -
				90. -
				1. / (2.*cells_in_degree)
				);

			sunset_hour_angle = pi - acos(std::max(
				std::min(
					(
						(sin(latitude_of_the_site) * sin(solar_declination_angle)) /
						(cos(latitude_of_the_site) * cos(solar_declination_angle))),
					 1.),
					-1.
				));

			possible_sunshine_duration = 24 / pi * sunset_hour_angle;
			distance_earth_sun = 1. + 0.033 * cos(pi2_365 * DOY);

			extraterrestrial_radiation = 15.392 *
										 distance_earth_sun *
										 (sunset_hour_angle *
										 	sin(latitude_of_the_site) * sin(solar_declination_angle) +
											cos(latitude_of_the_site) * cos(solar_declination_angle) * sin(sunset_hour_angle)
										);

			shortwave_downward = extraterrestrial_radiation *
								(a_s +
								 b_s * std::min(daily_sunshine_duration / possible_sunshine_duration, 1.));

			shortwave_downward_radiation(day, col) = shortwave_downward / convert_Wm2_to_mmd;
		}
	}
	return(shortwave_downward_radiation);
}