#include <Rcpp.h>
#include "initModel.h"
#include "initializeModel.h"

using namespace Rcpp;
using namespace std;

NumericVector readFile(const char* file);
NumericMatrix readFile(const char* file, int rows);

void writeFile(const char* file, NumericVector vector2write);
void writeFile(const char* file, NumericMatrix matrix2write);

String combineStrings(String string1, String string2, String String2insert);

String getLastEntry(DateVector vector2examine);
String getFirstEntry(DateVector vector2examine);
String fillWithZeros(String string2fill, int n_zero);

void setStorages(DateVector SimPeriod){
	//beginning of the day
	NumericVector dummy;
	NumericMatrix dummy2;
	String id_string = std::to_string(id);
	String date4Values = getFirstEntry(SimPeriod);
	String prefix = combineStrings(id_string, date4Values, "_");
	
	//canopy
	String nameCanopy = combineStrings(prefix, "G_canopyWaterContent.bin", "_");
	String pathCanopy = combineStrings(SystemValues, nameCanopy, "/");
	const char* pathCanopy_char = pathCanopy.get_cstring();
	dummy = readFile(pathCanopy_char);
	G_canopyWaterContent = dummy; 
	
	//reading snow
	String nameSnow = combineStrings(prefix, "G_snow.bin", "_");
	String pathSnow = combineStrings(SystemValues, nameSnow, "/");
	const char* pathSnow_char = pathSnow.get_cstring();
	dummy = readFile(pathSnow_char);
	G_snow = dummy;
	
	//writing snowMatrix 
	String nameSnowWE = combineStrings(prefix, "G_snowWaterEquivalent.bin", "_");
	String pathSnowWE = combineStrings(SystemValues, nameSnowWE, "/");
	const char* pathSnowWE_char = pathSnowWE.get_cstring();
	dummy2 = readFile(pathSnowWE_char, 25);
	G_snowWaterEquivalent = dummy2;
	
	
	//writing soil
	String nameSoil = combineStrings(prefix, "G_soilWaterContent.bin", "_");
	String pathSoil = combineStrings(SystemValues, nameSoil, "/");
	const char* pathSoil_char = pathSoil.get_cstring();
	dummy = readFile(pathSoil_char);
	G_soilWaterContent = dummy;
	
	
	//writing groundwater
	String nameGW = combineStrings(prefix, "G_groundwater.bin", "_");
	String pathGW = combineStrings(SystemValues, nameGW, "/");
	const char* pathGW_char = pathGW.get_cstring();
	dummy = readFile(pathGW_char);
	G_groundwater = dummy;
	
	//writing river
	String nameRiver = combineStrings(prefix, "S_river.bin", "_");
	String pathRiver = combineStrings(SystemValues, nameRiver, "/");
	const char* pathRiver_char = pathRiver.get_cstring();
	dummy = readFile(pathRiver_char);
	S_river = dummy;
	
	//writing S_locLakeStorage
	String nameLocLak = combineStrings(prefix, "S_locLakeStorage.bin", "_");
	String pathLocLak = combineStrings(SystemValues, nameLocLak, "/");
	const char* pathLocLak_char = pathLocLak.get_cstring();
	dummy = readFile(pathLocLak_char);
	S_locLakeStorage = dummy;
	
	//writing S_locWetlandStorage
	String nameLocWet = combineStrings(prefix, "S_locWetlandStorage.bin", "_");
	String pathLocWet = combineStrings(SystemValues, nameLocWet, "/");
	const char* pathLocWet_char = pathLocWet.get_cstring();
	dummy = readFile(pathLocWet_char);
	S_locWetlandStorage = dummy;
	
	//writing S_gloLakeStorage
	String nameGloLak = combineStrings(prefix, "S_gloLakeStorage.bin", "_");
	String pathGloLak = combineStrings(SystemValues, nameGloLak, "/");
	const char* pathGloLak_char = pathGloLak.get_cstring();
	dummy = readFile(pathGloLak_char);
	S_gloLakeStorage = dummy;
	
	//writing S_ResStorage
	String nameRes = combineStrings(prefix, "S_ResStorage.bin", "_");
	String pathRes = combineStrings(SystemValues, nameRes, "/");
	const char* pathRes_char = pathRes.get_cstring();
	dummy = readFile(pathRes_char);
	S_ResStorage = dummy;
	
	//writing S_gloWetlandStorage
	String nameGloWet = combineStrings(prefix, "S_gloWetlandStorage.bin", "_");
	String pathGloWet = combineStrings(SystemValues, nameGloWet, "/");
	const char* pathGloWet_char = pathGloWet.get_cstring();
	dummy = readFile(pathGloWet_char);
	S_gloWetlandStorage = dummy;
	
}

void writeStorages(DateVector SimPeriod){
	// end of the day -> for beginning of next day
	String id_string = std::to_string(id);
	String date4Values = getLastEntry(SimPeriod);
	String prefix = combineStrings(id_string, date4Values, "_");
	
	//writing canopy
	String nameCanopy = combineStrings(prefix, "G_canopyWaterContent.bin", "_");
	String pathCanopy = combineStrings(SystemValues, nameCanopy, "/");
	const char* pathCanopy_char = pathCanopy.get_cstring();
	writeFile(pathCanopy_char, G_canopyWaterContent);
	
	//writing snow
	String nameSnow = combineStrings(prefix, "G_snow.bin", "_");
	String pathSnow = combineStrings(SystemValues, nameSnow, "/");
	const char* pathSnow_char = pathSnow.get_cstring();
	writeFile(pathSnow_char, G_snow);
	
	//writing snowMatrix 
	String nameSnowWE = combineStrings(prefix, "G_snowWaterEquivalent.bin", "_");
	String pathSnowWE = combineStrings(SystemValues, nameSnowWE, "/");
	const char* pathSnowWE_char = pathSnowWE.get_cstring();
	writeFile(pathSnowWE_char, G_snowWaterEquivalent);
	
	
	//writing soil
	String nameSoil = combineStrings(prefix, "G_soilWaterContent.bin", "_");
	String pathSoil = combineStrings(SystemValues, nameSoil, "/");
	const char* pathSoil_char = pathSoil.get_cstring();
	writeFile(pathSoil_char, G_soilWaterContent);
	
	//writing groundwater
	String nameGW = combineStrings(prefix, "G_groundwater.bin", "_");
	String pathGW = combineStrings(SystemValues, nameGW, "/");
	const char* pathGW_char = pathGW.get_cstring();
	writeFile(pathGW_char, G_groundwater);
	
	//writing river
	String nameRiver = combineStrings(prefix, "S_river.bin", "_");
	String pathRiver = combineStrings(SystemValues, nameRiver, "/");
	const char* pathRiver_char = pathRiver.get_cstring();
	writeFile(pathRiver_char, S_river);
	
	//writing S_locLakeStorage
	String nameLocLak = combineStrings(prefix, "S_locLakeStorage.bin", "_");
	String pathLocLak = combineStrings(SystemValues, nameLocLak, "/");
	const char* pathLocLak_char = pathLocLak.get_cstring();
	writeFile(pathLocLak_char, S_locLakeStorage);
	
	//writing S_locWetlandStorage
	String nameLocWet = combineStrings(prefix, "S_locWetlandStorage.bin", "_");
	String pathLocWet = combineStrings(SystemValues, nameLocWet, "/");
	const char* pathLocWet_char = pathLocWet.get_cstring();
	writeFile(pathLocWet_char, S_locWetlandStorage);
	
	//writing S_gloLakeStorage
	String nameGloLak = combineStrings(prefix, "S_gloLakeStorage.bin", "_");
	String pathGloLak = combineStrings(SystemValues, nameGloLak, "/");
	const char* pathGloLak_char = pathGloLak.get_cstring();
	writeFile(pathGloLak_char, S_gloLakeStorage);
	
	//writing S_ResStorage
	String nameRes = combineStrings(prefix, "S_ResStorage.bin", "_");
	String pathRes = combineStrings(SystemValues, nameRes, "/");
	const char* pathRes_char = pathRes.get_cstring();
	writeFile(pathRes_char, S_ResStorage);
	
	//writing S_gloWetlandStorage
	String nameGloWet = combineStrings(prefix, "S_gloWetlandStorage.bin", "_");
	String pathGloWet = combineStrings(SystemValues, nameGloWet, "/");
	const char* pathGloWet_char = pathGloWet.get_cstring();
	writeFile(pathGloWet_char, S_gloWetlandStorage);
	
}


String getLastEntry(DateVector vector2examine){
  
  int n = vector2examine.size();
  int n_zero = 2;
  Date date2return = vector2examine[n-1];
  date2return = date2return + 1; //increase Date because end of day is beginning of next date
  
  int day = date2return.getDay();
  int month = date2return.getMonth();
  int year = date2return.getYear();
  
  String dayString = std::to_string(day);
  String monthString = std::to_string(month);
  String yearString = std::to_string(year);
  
  String dayStringF = fillWithZeros(dayString, 2);
  String monthStringF = fillWithZeros(monthString, 2);
  String yearStringF = fillWithZeros(yearString, 4);
  
  
  yearStringF += monthStringF;
  yearStringF += dayStringF;
  
  return(yearStringF);
}

String fillWithZeros(String string2fill, int n_zero){
    const char* string2fill_c = string2fill.get_cstring();
    int n = strlen(string2fill_c);
                  
	std::string new_str = std::string(2 - std::min(2, n), '0') + string2fill_c;
	String string2return(new_str);
	
	return(string2return);
}

String getFirstEntry(DateVector vector2examine){
  
  Date date2return = vector2examine[0];
  
  int day = date2return.getDay();
  int month = date2return.getMonth();
  int year = date2return.getYear();
  
  String dayString = std::to_string(day);
  String monthString = std::to_string(month);
  String yearString = std::to_string(year);
  
  String dayStringF = fillWithZeros(dayString, 2);
  String monthStringF = fillWithZeros(monthString, 2);
  String yearStringF = fillWithZeros(yearString, 4);
  
  yearStringF += monthStringF;
  yearStringF += dayStringF;
  
  return(yearStringF);
}


void writeFile(const char* file, NumericVector vector2write){
  
  const static int n = vector2write.size();
  FILE *file_ptr;
  file_ptr = fopen(file, "wb");
  if (file_ptr == NULL) { Rcpp::stop("File Error: %s not found", file);}
  
  std::vector<double> vector2write_std(n);
  vector2write_std = as<std::vector<double> >(vector2write);
  
  fwrite(&vector2write_std[0], sizeof(double), vector2write_std.size(), file_ptr);
  fclose(file_ptr);
}

//for matrix
void writeFile(const char* file, NumericMatrix matrix2write){
  
  NumericVector vector2write = NumericVector(matrix2write);
  const static int n = vector2write.size();
  
  std::vector<double> vector2write_std(n);
  vector2write_std = as<std::vector<double> >(vector2write);
  
  FILE *file_ptr;
  file_ptr = fopen(file, "wb");
  if (file_ptr == NULL) { Rcpp::stop("File Error: %s not found", file);}
  
  fwrite(&vector2write_std[0], sizeof(double), vector2write_std.size(), file_ptr);
  fclose(file_ptr); 
}



NumericVector readFile(const char* file){
  
  FILE *file_ptr;
  unsigned long lSize;

  //opening file
  file_ptr = fopen(file, "rb");
  if (file_ptr == NULL) { Rcpp::stop("File Error: %s not found", file);}
   
  //obtaining number of elements to be read
  fseek (file_ptr , 0 , SEEK_END); //goes to the end
  lSize = ftell (file_ptr); // tells number
  rewind (file_ptr); // goes to the front
  static const size_t elem = lSize/sizeof(double);
  
  //defining vector to store read input
  std::vector<double> vector2read(elem);
 
  //reading & closing
  fread(&vector2read[0], sizeof(double), elem, file_ptr);
  fclose(file_ptr);
  
  NumericVector vector2read_rcpp = wrap(vector2read); // wrap x into an R object
  
  return(vector2read_rcpp);
  
}

//for matrix
NumericMatrix readFile(const char* file, int rows){
  
  FILE *file_ptr;
  unsigned long lSize;
  
  //opening file
  file_ptr = fopen(file, "rb");
  if (file_ptr == NULL) { Rcpp::stop("File Error: %s not found", file) ;}
  
  //obtaining number of elements to be read
  fseek (file_ptr , 0 , SEEK_END); //goes to the end
  lSize = ftell (file_ptr); // tells number
  rewind (file_ptr); // goes to the front
  static const size_t elem = lSize/sizeof(double);
  
  //defining vector to store read input
  std::vector<double> vector2read(elem);
  
    //reading & closing
  fread(&vector2read[0], sizeof(double), elem, file_ptr);
  fclose(file_ptr);
  
  NumericVector vector2read_rcpp = wrap(vector2read); // wrap x into an R object
  vector2read_rcpp.attr("dim") = Dimension(rows, elem/rows);
  NumericMatrix matrix2read = as<NumericMatrix>(vector2read_rcpp);
  
  return(matrix2read);
  
}


String combineStrings(String string1, String string2, String String2insert){
    String string3;
    string3 += string1;
    string3 += String2insert;
    string3 += string2;

	return(string3);
}

