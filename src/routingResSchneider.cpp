#include <Rcpp.h>
#include <math.h>
#include "ModelTools.h"
#include "initModel.h"
#include "routingResSchneider.h"

using namespace Rcpp;
using namespace std;

// Optimisation function which computes the target every first day of the month
// (copy pasted and adapted from WaterGAP 3)
long optimiseDamOperation(
	int res_type,
	int yearDate, // for leap years only
	double mean_inflow,
	double Smax,
	double Qmin_7day,
	double Qmax_7day,
	double Qbf,
	double Qflood,
	double Sstart,
	NumericVector inflow_forecast, // [km3/month]
	int month_current,
	NumericVector PET_forecast,	 // [mm.km2/d]
	NumericVector Prec_forecast, // [mm.km2/d]
	bool eFlow,
	int cell)
{
	long int DELTA_T = 86400; // (24h * 60min * 60sec)

	short februaryDays = 28;
	if (GapYearType != 1)
	{
		februaryDays = numberOfDaysInMonth(2, yearDate);
	}
	const short DAYSPERMONTH[12] = {31, februaryDays, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

	///--- Series of forecasted monthly inflow for the next 12 month---------------------------------------------
	double inflow_series[12];
	double evapo_series[12];
	double prec_series[12];

	int month_next = 0;

	for (int m = 0; m < 12; m++)
	{
		month_next = m + month_current;
		if (month_next > 12)
		{
			month_next = month_next - 12;
		}
		inflow_series[m] = inflow_forecast(month_next - 1) / DAYSPERMONTH[month_next - 1] / DELTA_T * 1000. * 1000. * 1000.; // [km3/month] => [m3/s]
		evapo_series[m] = PET_forecast(month_next - 1) * 1000. / DELTA_T;													 // [mm.km2/d] => [m3/s]
		prec_series[m] = Prec_forecast(month_next - 1) * 1000. / DELTA_T;													 // [mm.km2/d] => [m3/s]
	}
	///----  Definition of variables  ---------------------------------------------------------------------------
	int nr_of_classes = 80;
	short int Smin = 0;
	long int Smean = 0;
	double evapo_reduction = 1.;

	///---- Calculation of mean annual inflow -------------------------------------------------------------------
	double annual_inflow = 0.0;
	double Qmean = 0.0;

	for (int month = 0; month < 12; month++)
	{
		annual_inflow = annual_inflow + (inflow_forecast(month) * 1000. * 1000. * 1000.);
	}
	Qmean = annual_inflow / (365. * DELTA_T);
	// Qmean = mean_inflow * 12. * 1000. * 1000. * 1000. / (365. * DELTA_T); // km3/month => m3/s
	// Rcout << "Qmean : " << Qmean << " m3/s" << ", mean_inflow was : " << mean_inflow * 12. * 1000. * 1000. * 1000. / (365. * DELTA_T) << endl;
	///---- Discretisation of storage into n classes (Savarenskiy) -----------------------------------------
	short int class_id = 0;
	double class_mid_value = 0.;
	double class_width = 0.;
	double class_border_low = 0.;
	double class_border_high = 0.;

	double **class_table = new double *[nr_of_classes + 1];
	for (class_id = 1; class_id <= (nr_of_classes + 1); class_id++)
	{
		class_table[class_id - 1] = new double[4];
		if (!class_table[class_id - 1])
		{
			Rcerr << "memory error class_table\n";
			exit(1);
		}
	}

	class_width = (0. + Smax - Smin) / nr_of_classes;
	class_border_low = Smin;
	class_border_high = 0. + Smin + (0.5 * class_width);

	for (class_id = 1; class_id <= (nr_of_classes + 1); class_id++)
	{
		class_mid_value = (class_border_low + class_border_high) / 2.;
		class_table[class_id - 1][0] = class_id;
		class_table[class_id - 1][1] = roundf(class_border_low);
		class_table[class_id - 1][2] = roundf(class_border_high);
		class_table[class_id - 1][3] = roundf(class_mid_value);
		class_border_low = class_border_high;
		if (class_id < nr_of_classes)
		{
			class_border_high = class_border_high + class_width;
		}
		else
		{
			class_border_high = class_border_high + (0.5 * class_width);
		}
	}

	///---- Optimisation -----------------------------------------------------------------------------------------
	short int month = 12;
	short int penalties = 0;
	short int penalties_total = 0;
	short int penalties_optimised = 0;
	short int exclusion = 0;
	int Sm_class_start = 1;
	int Sm_class_end = 1;
	int Sm_class_optimised = 0;
	int row_counter = 0;
	int rows_opt_table = (nr_of_classes + 1) * (nr_of_classes + 1);
	double Rm = 0.0;
	double benefit = 0.0;
	double benefit_optimised = 0.0;
	double benefit_previous = 0.0;
	double benefit_total = 0.0;
	double water_level_start = 0.0;
	double water_level_end = 0.0;
	double water_level_mean = 0.0;

	double ***previous_benefit_table = new double **[12];
	for (int counter_slides = 1; counter_slides <= 12; counter_slides++)
	{
		previous_benefit_table[counter_slides - 1] = new double *[nr_of_classes + 1];
		for (int counter_rows = 1; counter_rows <= (nr_of_classes + 1); counter_rows++)
		{
			previous_benefit_table[counter_slides - 1][counter_rows - 1] = new double[4];
		}
	}

	double ***optimisation_table = new double **[12];
	for (int counter_slides = 1; counter_slides <= 12; counter_slides++)
	{
		optimisation_table[counter_slides - 1] = new double *[rows_opt_table];
		for (int counter_rows = 1; counter_rows <= rows_opt_table; counter_rows++)
		{
			optimisation_table[counter_slides - 1][counter_rows - 1] = new double[8];
		}
	}

	for (month = 12; month > 0; month--)
	{

		row_counter = 0;

		for (Sm_class_start = 1; Sm_class_start <= (nr_of_classes + 1); Sm_class_start++)
		{
			for (Sm_class_end = 1; Sm_class_end <= (nr_of_classes + 1); Sm_class_end++)
			{

				// ET: Calculate reduction factor depending on storage level
				Smean = (class_table[Sm_class_start - 1][3] + class_table[Sm_class_end - 1][3]) / 2;

				if (Smean >= Smax)
				{
					evapo_reduction = 1.;
				}
				else
				{
					evapo_reduction = 1. - pow((double)(Smax - Smean) / Smax, 2.81383);
				}

				// Calculate monthly release Rm
				Rm = ((double)(class_table[Sm_class_start - 1][3] - class_table[Sm_class_end - 1][3]) / (DAYSPERMONTH[month - 1] * DELTA_T)) + inflow_series[month - 1] + prec_series[month - 1] - (evapo_series[month - 1] * evapo_reduction);

				// Benefit (Water supply, Navigation, Recreation)
				if (res_type == 2 || res_type == 4 || res_type == 5 || res_type == 6 || res_type == 7)
				{
					benefit = pow(fabs(Rm - Qmean), 2);
				}

				// Benefit (Hydropower)
				if (res_type == 3)
				{
					water_level_start = pow((6.0 * class_table[Sm_class_start - 1][3]) / pow(19.45, 2), 0.3333333333);
					water_level_end = pow((6.0 * class_table[Sm_class_end - 1][3]) / pow(19.45, 2), 0.3333333333);
					water_level_mean = 2. / ((1. / water_level_start) + (1. / water_level_end)); // Harmonic mean
					benefit = 1. / (Rm * 1000. * 9.81 * water_level_mean);
				}

				// Benefit (Flood control)
				// if (res_type == 4){
				//	if (Rm > Qflood) {
				//		benefit         = pow(Rm - Qflood,2);
				//	}
				//	else{
				//		benefit          = 0.;
				//	}
				// }

				// Benefit of previous month
				if (month == 12)
				{
					benefit_previous = 0.0;
				}
				else
				{
					benefit_previous = previous_benefit_table[month][Sm_class_end - 1][2];
				}

				// Total benefit
				benefit_total = benefit + benefit_previous;

				// Constraint 1:Sm+1 <= Sm + Qm - Em
				if (class_table[Sm_class_end - 1][3] > (class_table[Sm_class_start - 1][3] + (inflow_series[month - 1] * DAYSPERMONTH[month - 1] * DELTA_T) -
														(evapo_series[month - 1] * evapo_reduction * DAYSPERMONTH[month - 1] * DELTA_T) + (prec_series[month - 1] * DAYSPERMONTH[month - 1] * DELTA_T)))
				{
					exclusion = 1;
				}

				// Constraint 2: Minimum flow provision: Reserve storage to provide Qmin for 30 days
				if (class_table[Sm_class_end - 1][3] < (30 * Qmin_7day * DELTA_T))
				{
					penalties = penalties + 1;
				}

				// Constraint 3: Flood protection: Reserve storage capacity to absorb Qmax for 7 days
				if (class_table[Sm_class_end - 1][3] > (Smax - (7 * Qmax_7day * DELTA_T)))
				{
					penalties = penalties + 1;
				}

				// Constraint 4: Rm <= Qbf
				if (Rm > Qbf)
				{
					penalties = penalties + 1;
				}

				// Constraint 5: eflow provisions: Changes < +-20%
				if (eFlow)
				{
					if (Rm > 1.2 * inflow_series[month - 1])
						penalties = penalties + 1;
					if (Rm < 0.8 * inflow_series[month - 1])
						penalties = penalties + 1;
				}

				// Penalties total
				if (month == 12)
				{
					penalties_total = penalties;
				}
				else
				{
					penalties_total = penalties + previous_benefit_table[month][Sm_class_end - 1][3];
				}

				// Insert new row into optimisation table
				optimisation_table[month - 1][row_counter][0] = Sm_class_start;
				optimisation_table[month - 1][row_counter][1] = Sm_class_end;
				optimisation_table[month - 1][row_counter][2] = benefit;
				optimisation_table[month - 1][row_counter][3] = benefit_previous;
				optimisation_table[month - 1][row_counter][4] = benefit_total;
				optimisation_table[month - 1][row_counter][5] = penalties;
				optimisation_table[month - 1][row_counter][6] = penalties_total;
				optimisation_table[month - 1][row_counter][7] = exclusion;

				row_counter = row_counter + 1;
				exclusion = 0;
				penalties = 0;

			} // for Sm+1 end
		} // for Sm end

		int switch_start;

		for (Sm_class_start = 1; Sm_class_start <= (nr_of_classes + 1); Sm_class_start++)
		{
			benefit_optimised = 0.0;
			Sm_class_optimised = 0;
			penalties_optimised = 0;
			switch_start = 1;

			for (int counter = 1; counter <= rows_opt_table; counter++)
			{
				if ((optimisation_table[month - 1][counter - 1][0] == Sm_class_start) && (optimisation_table[month - 1][counter - 1][7] == 0))
				{
					if (switch_start == 1)
					{
						Sm_class_optimised = optimisation_table[month - 1][counter - 1][1];
						benefit_optimised = optimisation_table[month - 1][counter - 1][4];
						penalties_optimised = optimisation_table[month - 1][counter - 1][6];
						switch_start = 0;
					}
					else
					{
						if (optimisation_table[month - 1][counter - 1][6] < penalties_optimised)
						{
							Sm_class_optimised = optimisation_table[month - 1][counter - 1][1];
							benefit_optimised = optimisation_table[month - 1][counter - 1][4];
							penalties_optimised = optimisation_table[month - 1][counter - 1][6];
						}
						else if ((optimisation_table[month - 1][counter - 1][6] == penalties_optimised) && (optimisation_table[month - 1][counter - 1][4] < benefit_optimised))
						{
							Sm_class_optimised = optimisation_table[month - 1][counter - 1][1];
							benefit_optimised = optimisation_table[month - 1][counter - 1][4];
							penalties_optimised = optimisation_table[month - 1][counter - 1][6];
						}
					}
				}
			}

			// If no optimal storage class was found: maintain the old storage class
			if (Sm_class_optimised == 0)
			{
				for (int counter = 1; counter <= rows_opt_table; counter++)
				{
					if ((optimisation_table[month - 1][counter - 1][0] == Sm_class_start) && (optimisation_table[month - 1][counter - 1][1] == Sm_class_start))
					{
						Sm_class_optimised = optimisation_table[month - 1][counter - 1][1];
						benefit_optimised = optimisation_table[month - 1][counter - 1][4];
						penalties_optimised = optimisation_table[month - 1][counter - 1][6];
					}
				}
			}

			previous_benefit_table[month - 1][Sm_class_start - 1][0] = Sm_class_start;
			previous_benefit_table[month - 1][Sm_class_start - 1][1] = Sm_class_optimised;
			previous_benefit_table[month - 1][Sm_class_start - 1][2] = benefit_optimised;
			previous_benefit_table[month - 1][Sm_class_start - 1][3] = penalties_optimised;
		}

	} // for month end

	// Determine the class of Storage at the beginning (Sstart)
	short int class_start = 0;

	for (class_id = 1; class_id <= (nr_of_classes + 1); class_id++)
	{
		if ((Sstart >= class_table[class_id - 1][1]) && (Sstart <= class_table[class_id - 1][2]))
		{
			class_start = class_table[class_id - 1][0];
		}
	}

	// Forward-moving: Calculation of the optimal monthly releases

	// Determine target storage for end of month
	// short int class_next		   = 0;
	long int S_target = 0;

	if (class_start == 0)
	{
		if (Sstart <= class_table[nr_of_classes][2] * 1.01)
		{ // Had to change class table to type double rather than long because it was overflowing,
		  // so need to account for floating point error
			class_start = class_table[nr_of_classes][0];
		}
		else
		{
			S_target = class_table[0][3];
			Rcerr << "optimiseDamOperation() ERROR: S_target = " << S_target << ", class_start: " << class_start << ", Sstart: " << Sstart << ", class max: " << class_table[nr_of_classes][2] << endl;
			for (int i = 1; i <= (nr_of_classes + 1); i++)
			{
				Rcerr << "class_table[" << i << "][1] : " << class_table[i - 1][1] << ", class_table[" << i << "][2] : " << class_table[i - 1][2] << endl;
			}
		}
	}
	else
	{
		short int class_next = previous_benefit_table[0][class_start - 1][1];
		S_target = class_table[class_next - 1][3];
	}

	// Delete arrays
	for (int i = 1; i <= (nr_of_classes + 1); i++)
	{
		delete[] class_table[i - 1];
		class_table[i - 1] = NULL;
	}
	delete[] class_table;
	class_table = NULL;

	for (int i = 1; i <= 12; i++)
	{
		for (int y = 1; y <= (nr_of_classes + 1); y++)
		{
			delete[] previous_benefit_table[i - 1][y - 1];
			previous_benefit_table[i - 1][y - 1] = NULL;
		}
		delete[] previous_benefit_table[i - 1];
		previous_benefit_table[i - 1] = NULL;
	}
	delete[] previous_benefit_table;
	previous_benefit_table = NULL;

	for (int i = 1; i <= 12; i++)
	{
		for (int y = 1; y <= rows_opt_table; y++)
		{
			delete[] optimisation_table[i - 1][y - 1];
			optimisation_table[i - 1][y - 1] = NULL;
		}
		delete[] optimisation_table[i - 1];
		optimisation_table[i - 1] = NULL;
	}
	delete[] optimisation_table;
	optimisation_table = NULL;

	return S_target;
}

// Dam operation computation based on Schneider et al. 2015
// Takes for input forecasted inflow, PET and Precipitation and calls optimiseDamOperation() every first day
// in month to compute the target storage for the reservoir
// (copy pasted and adapted from WaterGAP 3, should be used for irrigation and water supply)
double routingResSchneider(
	int day,
	int cell,
	Date SimDate,
	double PETWater,
	double PrecWater,
	double inflow,
	NumericVector Res_outflow,
	NumericVector Res_overflow,
	NumericVector S_ResStorage,
	NumericVector Res_evapo,
	NumericVector Res_inflow,
	NumericMatrix dailyUse,
	NumericVector MeanDemand,
	NumericVector Prec_forecast,   // [mm.km2/d]
	NumericVector PET_forecast,	   // [mm.km2/d]
	NumericVector inflow_forecast, // [km3/month]
	double &Res_storage_target,
	double &accumulated_daily_month_inflow)
{
	// bool eFlow = true;
	int monthDate = SimDate.getMonth(); // month that is simulated [1:12]
	int dayDate = SimDate.getDay();		// day that is simulated [1:31]
	int yearDate = SimDate.getYear();	// year that is simulated
	int februaryDays = 28;
	if (GapYearType != 1)
	{
		februaryDays = numberOfDaysInMonth(2, yearDate);
	}
	int daysInMonthArray[12] = {31, februaryDays, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

	//  here to put back in case of it failed

	///---- Optimization of the reservoir operation  ---------------------------------------------------------
	double day_max = G_7daymax[cell];	// [m3/s]
	double day_min = G_7daymin[cell];	// [m3/s]
	double bankfull = G_BANKFULL[cell]; // [m3/s]
	long target;

	// Each month we optimise the reservoir release
	if (1 == dayDate)
	{
		// Rcout << "1st day of: " << monthDate << ", optimiseDamOperation() for cell " << cell << " parameters: " << endl;
		// Rcout << "G_STORAGE_CAPACITY[cell] : " << G_STORAGE_CAPACITY[cell] * 1000. * 1000. * 1000. << " m3, Res type: " << G_RES_TYPE[cell] << endl;
		// Rcout << "day_min : " << day_min << " m3/s" << endl;
		// Rcout << "day_max : " << day_max << " m3/s" << endl;
		// Rcout << "bankfull : " << bankfull << " m3/s" << endl;
		// Rcout << "S_ResStorage[cell] : " << S_ResStorage[cell] * 1000 << " m3" << endl;
		// Rcout << "inflow_forecast for the month : " << inflow_forecast[monthDate - 1] / daysInMonthArray[monthDate - 1] / 3600. / 24. * 1000000000. + ( Prec_forecast[monthDate - 1] - PET_forecast[monthDate - 1]) * 1000. / (3600 * 24) << " m3/s" << endl;

		target = optimiseDamOperation(G_RES_TYPE[cell],
									  yearDate,
									  G_MEAN_INFLOW[cell],								// [km3/month]
									  G_STORAGE_CAPACITY[cell] * 1000. * 1000. * 1000., // [km3] => [m3] [m3 expected]
									  day_min,											// [m3/s]
									  day_max,											// [m3/s]
									  bankfull,											// G_BANKFULL is [m3/s] (from initModel.h)
									  bankfull,											//
									  S_ResStorage[cell] * 1000.,						// [mm.km2] => [m3]
									  inflow_forecast,									// [km3/month] conversion is done in optimiseDamOperation
									  monthDate,										// [1:12]
									  PET_forecast,										// [mm.km2/d] conversion is done in optimiseDamOperation
									  Prec_forecast,									// [mm.km2/d] conversion is done in optimiseDamOperation
									  false,
									  cell); // eFlOw										// bool = true
		// Rcout << "start of month :" << monthDate << ", target : " << (double) target << " m3" << ", inflow : " << inflow*1000./3600./24 << " m3/s" << endl;
		// Rcout << "test Inflow forecast" << inflow_forecast[monthDate - 1] / daysInMonthArray[monthDate - 1] / 3600. / 24. * 1000000000. + ( Prec_forecast[monthDate - 1] - PET_forecast[monthDate - 1]) * 1000. / (3600 * 24) << endl;
		Res_storage_target = (double)target / 1000.; // [m3] => [mm.km2]
	}

	double maxStorage;
	double totalInflow;
	double outflow;
	double overflow = 0.;
	double evaporation;
	double gloResEvapoReductionFactor;
	// CALCULATING WATERBALANCE ANALOG TO ALL WATERBODIES

	// inflow from upstream PLUS lake water balance
	totalInflow = inflow + (PrecWater * G_RESAREA[cell]); //[mm km²]

	maxStorage = G_STORAGE_CAPACITY[cell] * 1000 * 1000; // [mm km²] (not taking into account the litterature's 85% limit)

	// ET: same as gloLakeEvapo
	if (S_ResStorage[cell] > maxStorage)
		gloResEvapoReductionFactor = 1.; // []
	else
		gloResEvapoReductionFactor = 1. - pow(fabs(S_ResStorage[cell] - maxStorage) / maxStorage, evapoReductionExpReservoir); // []

	// calculate evaporation from global lakes
	evaporation = (PETWater * gloResEvapoReductionFactor) * G_RESAREA[cell]; // [mm km²]

	// Calculate daily release
	short int days_elapsed;		// elapsed days in the current month
	long int storage_deviation; // Deviation current storage to target storage
	double Qmi;					// expected inflow for the day calculated with the average of what was expected for the remainder of the month
								// and the mean of what has flown this month [mm.km2/d]
	double release;				// daily release [m3/s]
	double losses;

	days_elapsed = dayDate - 1;
	storage_deviation = S_ResStorage[cell] - Res_storage_target; // [mm.km2]
																 // Diff with WG3 : Res_storage is updated here before the calculation of the release

	if (dayDate == 1)
	{
		accumulated_daily_month_inflow = inflow;
	}
	else
	{
		accumulated_daily_month_inflow += inflow; //[mm.km2]
	}

	Qmi = (accumulated_daily_month_inflow + (inflow_forecast(monthDate - 1) * 1000000. / daysInMonthArray[monthDate - 1] * (daysInMonthArray[monthDate - 1] - days_elapsed - 1))) / daysInMonthArray[monthDate - 1];
	// Qmi = totalInflow + (inflow_forecast(monthDate - 1) * 1000000. / daysInMonthArray[monthDate - 1]) /2; // [mm.km2/d]
	// Qmi = totalInflow;
	// Calculate losses due to evaporation and precipitation
	losses = (PrecWater * G_RESAREA[cell]) - evaporation; // [mm.km2/d]

	// Update of daily release
	release = (storage_deviation / (daysInMonthArray[monthDate - 1] - days_elapsed) + (Qmi + losses));				
	// Rcout << "test : " << pow(daysInMonthArray[monthDate - 1] - days_elapsed, 2)  / 30 << endl;											 // [mm.km2/d]
	// release = (storage_deviation / (pow(daysInMonthArray[monthDate - 1] - days_elapsed, 2) / 30) + (Qmi + losses)); // [mm.km2/d] test, maybe it's better

	if (release < 0.)
	{
		release = 0.;
	}
	// The daily release should be within the limits of Qmin (7daymin) and Qmax (bankfull flow)
	bankfull *= 3600. * 24. / 1000.; // [m3/s] => [mm.km2/d]
	if (release > bankfull)			 // [mm.km2/d]
	{
		release = bankfull;
	}
	day_min *= 3600. * 24. / 1000.; // [m3/s] => [mm.km2/d]
	if (release < day_min)			// [mm.km2/d]
	{
		release = day_min;
	}

	// add inflow to storage
	S_ResStorage[cell] += totalInflow; // [mm km²]

	// substract global lake evapo
	S_ResStorage[cell] -= evaporation; //[mm km²]

	// Adjustment of release due to E-flow provisions (daily changes must be within +-20%)
	if (false) // (eFlow)
	{
		if (release > 1.2 * inflow)
			release = 1.2 * inflow;
		else if (release < 0.8 * inflow)
			release = 0.8 * inflow;
	}

	// Adjustment of release due to empty storage:
	if (S_ResStorage[cell] < (G_STORAGE_CAPACITY[cell] * 1000000. * 0.10)) // [mm.km2] (from km3 for G_STORAGE_CAPACITY)
	{
		outflow = 0.1 * release;
	}
	else if (S_ResStorage[cell] < (G_STORAGE_CAPACITY[cell] * 1000000. * 0.15))
	{
		outflow = 0.4 * release;
	}
	else if (S_ResStorage[cell] < (G_STORAGE_CAPACITY[cell] * 1000000. * 0.20))
	{
		outflow = 0.7 * release;
	}
	else if (S_ResStorage[cell] < (G_STORAGE_CAPACITY[cell] * 1000000. * 0.25))
	{
		outflow = 0.9 * release;
	}
	else
	{
		outflow = release;
	}

	// if (dayDate == 1)
	// {
	// 	Rcout << "c = " << G_STORAGE_CAPACITY[cell] / (G_MEAN_INFLOW[cell] * 12) << " outflow : " << outflow * 1000. / 3600. / 24. << " release : " << release * 1000. / 3600. / 24. << endl;
	// 	Rcout << endl;

	// }
	// substract outflow from storage
	S_ResStorage[cell] -= outflow;

	// overflow: reduce G_gloResStorage to maximum storage capacity
	if (S_ResStorage[cell] > maxStorage)
	{
		overflow = (S_ResStorage[cell] - maxStorage);
		S_ResStorage[cell] = maxStorage;
	}

	// empty storage: if G_gloResStorage is below '0' there is no outflow anymore
	if (S_ResStorage[cell] < 0.)
	{
		evaporation += S_ResStorage[cell];
		if (evaporation < 0.)
		{
			outflow += evaporation;
			evaporation = 0.;
		}
		S_ResStorage[cell] = 0.;
	}

	Res_outflow[cell] = outflow;
	Res_overflow[cell] = overflow;
	Res_evapo[cell] = evaporation;
	Res_inflow[cell] = totalInflow;
	// the target is passed as reference so updated in the function at the beginning of each month

	return outflow + overflow;
}
