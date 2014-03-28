
-record(grib_source,{
    % name of the grib source
    name :: atom()|string(),
    % daily cycles recorded as hours since midnight, e.g. [0,6,15,18] in correct timezone
    cycles :: [non_neg_integer()],
    % delay between cycle time and forecast availability [hrs]
    delay :: non_neg_integer(),
    % the fixed part of the URL where the grib2 files are located
    url_prefix :: string(),
    % naming function, constructs the variable part of the URL and also functions as
    % a relative path on the local filesystem
    name_fun :: fun(),
    % the vtable file that identifies the fields in the GRIB2 files from this source for WPS
    domain :: string(),
    % the hours [since cycle time] for which grib files are available every cycle
    file_hours :: [non_neg_integer()],
    % the method of determining which grib files to download (single_cycle vs. multi_cycle)
    grib_resolution_method :: single_cycle|multi_cycle,
    % maximum concurrent downloads allowed for this source
    max_downloads :: pos_integer(),
    % extra entries that must be added to the WPS namelist to ensure correct function
    grib_info :: [{any(),any()}]
}).

