# grib_ingest

## Introduction

The `grib_ingest` system is an Erlang/OTP compliant application that provides the capability to download GRIB2 files which are frequently used to initialize and force mesoscale weather simulations.  An example of a model that can be used for mesoscale numerical weather prediction is WRF http://wrf-model.org .

The `grib_ingest` application can be configured to retrieve GRIB2 files from various sources and comes with two pre-configured GRIB2 sources:

  * NAM 218 12km products for the CONUS region http://www.nco.ncep.noaa.gov/pmb/products/nam/
  * GFS 1 degree global model http://www.nco.ncep.noaa.gov/pmb/products/gfs/

Notes:  CONUS = Conterminous United States, GFS = Global Forecast Model, WRF = Weather Research And Forecasting model, GRIB2 = GRidded Binary v2, NAM = North American Mesoscale

## API

There are two important functions:

### Simple retrieval: retrieve_gribs_simple/4

    grib_ingest:retrieve_gribs_simple(GribSourceName,FromZulu,ToZulu,AtZulu)

This function attempts to retrieve GRIB2 files from the source identified by `GribSourceName` (must match a name in one of the `etc/` grib source files) that cover the time interval `{FromZulu,ToZulu}` in UTC.  The system behaves as if the current universal time was `AtZulu`.  This is useful when one needs daily runs initialized from the same cycle but on some days the mesoscale model is started later but in this case we do not want to use the latest available cycle but the cycle that's used on other days.

# Example

    erl -pa ebin -s grib_ingest
    > grib_ingest:retrieve_gribs_simple(nam_218,{{2014,2,1},{0,0,0}},{{2014,2,1},{6,0,0}}{{2014,2,1},{3,0,0}}).
    {success,{{2014,2,1},{0,0,0}},
             {{2014,2,1},{6,0,0}},
                 ["stor/nam_218/nam.20140201/nam.t00z.awphys00.grb2.tm00",
                  "stor/nam_218/nam.20140201/nam.t00z.awphys01.grb2.tm00",
                  "stor/nam_218/nam.20140201/nam.t00z.awphys02.grb2.tm00",
                  "stor/nam_218/nam.20140201/nam.t00z.awphys03.grb2.tm00",
                  "stor/nam_218/nam.20140201/nam.t00z.awphys04.grb2.tm00",
                  "stor/nam_218/nam.20140201/nam.t00z.awphys05.grb2.tm00",
              "stor/nam_218/nam.20140201/nam.t00z.awphys06.grb2.tm00"]}

This will attempt to download the 7 hourly GRIB2 files.  If the files are not available, cannot be downloaded or the selected grid source cannot satisfy your request (for nam_218, this would be e.g. forecasting for 120hrs).

### Instruction-based retrieval: retrieve_gribs/4

The reason for having instruction-based retrieval is to try to attain a degree of robustness in the face of various delays and transient network and/or server errors.

        grib_ingest:retrieve_gribs(FromZulu,ToZulu,AtTime,Strategy)
    
The datetimes in this function have the same meaning as in `retrieve_gribs_simple/4`.  The additional `Strategy` parameter tells `grib_ingest` what to do in case the first attempt to retrieve the data fails. `Strategy` contains a list of steps that the application will follow while trying to retrieve the GRIB2 files.  The steps affect the internal state of the retrieval system and are as follows:

  * `{use_grib_source, GS}` switch to the grib source with name `GS`
  * `{wait_for_mins, TimeoutMins}` just wait for `TimeoutMins` minutes before continuing
  * `shift_cycle` try to use GRIB2 files produced by the cycle just before the current cycle
  * `try_retrieve` try to obtain the GRIB2 files in the current state

For example, the following strategy:

    [{use_grib_source, nam_218},try_retrieve,{wait_for_mins,5},try_retrieve,shift_cycle,try_retrieve,{use_grib_source,gfs_1deg}]
    
will try to retrieve GRIB2 files for our interval from the NAM 218 source, if that fails, it will try again in 5 minutes, if that fails again, it will try to use a previous cycle (i.e. instead of 12Z, the 6Z cycle will be used in NAM 218) and if this fails, the system will turn to another GRIB2 source: gfs_1deg.



## Configuration

The root of the storage directory can be directly set in the .app file by modifying the line

    mod,{grib_ingest_app,["stor"]}}
    
and replacing `stor` with the new directory.

Information required by the `grib_ingest` system to retrieve data from these sources is stored in files in the `etc` directory.  Consequently, it is easy to add more sources.  See below for an example.



## Example GRIB2 source configuration file

The file contains a single `grib_source` record, for example the following record allows the system to retrieve

```erlang
{grib_source,
 % atom or string identifying the source to the system
 nam_218,
 % module handling manifest construction/data retrieval (typically grib_source_server)
 grib_source_server,
 % start hour in GMT or "zulu time" of each daily cycle
 [0, 6, 12, 18],
 % minimal delay in hours before requesting files from cycle,
 % here, the system won't try to request files from e.g. the 12Z cycle until 14Z on same day
 2,
 % fixed part of the URL, does not change with requested GRIB2 file
 "http://nomads.ncep.noaa.gov/pub/data/nccf/com/nam/prod",
 % variable part of the URL, this function identifies the GRIB2 file for the system
 % on local storage (relative to the storage directory and domain, see below), the file is located at exactly
 % the same location as relative to the fixed part of the URL on the remote server
 fun({{Y, M, D}, {H, _M, _S}}, FcHr) -> io_lib:format("nam.~4..0B~2..0B~2..0B/nam.t~2..0Bz.awphys~2..0B.grb2.tm00", [Y, M, D, H, FcHr]) end,
 % a domain identifier, all GRIB2 files are locally stored under storage_directory/domain/location_relative_to_domain
 "nam_218",
 % list of forecast hours, assumed same for each cycle
 lists:flatten([lists:seq(0, 36), lists:map(fun(X) -> X * 3 end, lists:seq(13, 28))]),
 % maximum number of parallel downloads allowed for this source
 1,
 % this field can be used to store information for use with your favorite weather model, grib_ingest disregards this field
 [{vtable_file,"ungrib/Variable_Tables/Vtable.NAM"},{wps_namelist_keys,[{"domains", [{"num_metgrid_levels", [40]}]}]}],
 }.
```

## 



## TODO

  * scheduled downloads (local caching)
  * optional MD5 sums
  * concurrent downloads
  * `retrieve_grib_smart` that analyzes results of retrieval attempts and selects the next move

