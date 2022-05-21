── R CMD check results ─────────────────────────────────── cubble 0.1.0 ────
Duration: 2m 3.1s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

R CMD check succeeded

* The glyph examples take ~8s to run, which is beyond the 5s limit of CRAN check, hence it is wrapped in \dontrun.
* This is a submission addressing Prof Brian Ripley's email, which is copyed below:
    
    Checking cubble creates ~/Library/Caches/R/noaa_ghcnd/ on macOS, not an
    allowed area under the CRAN policy.
    
    Likely this comes from rnoaa which has
    
       hh <- hoardr::hoard()
       hh$cache_path_set("noaa_ghcnd")
       ghcnd_cache <<- hh
    
    where
    
     > rnoaa:::ghcnd_cache
    <hoard>
       path: noaa_ghcnd
       cache path: ~/Library/Caches/R/noaa_ghcnd
    
    although rnoaa does not itself use the area it seems to use hoardr for
    other similar purposes.
    
    rnoaa needs an urgent update to correct these potential policy
    violations: before 2022-05-25 or it, cubble and its other revdeps Z10
    and wildviz will have to be removed from CRAN.
    
    Further, neither cubble nor rnoaa are cleaning up after use:
    
    % ls -l  ~/.cache/R/noaa_ghcnd/
    total 12156
    -rw-r--r-- 1 ripley bdr 2019110 May 14 20:55 ASN00066037.dly
    -rw-r--r-- 1 ripley bdr  326035 May 14 20:55 ASN00066059.dly
    -rw-r--r-- 1 ripley bdr 2944659 May 14 20:55 ASN00066062.dly
    -rw-r--r-- 1 ripley bdr 1034884 May 14 20:55 ASN00066124.dly
    -rw-r--r-- 1 ripley bdr  483139 May 14 20:55 ASN00066194.dly
    -rw-r--r-- 1 ripley bdr 1053429 May 14 20:55 ASN00086038.dly
    -rw-r--r-- 1 ripley bdr  409317 May 14 20:55 ASN00086068.dly
    -rw-r--r-- 1 ripley bdr 1060235 May 14 20:56 ASN00086077.dly
    -rw-r--r-- 1 ripley bdr 1201999 May 14 20:56 ASN00086282.dly
    -rw-r--r-- 1 ripley bdr 1903365 May 14 20:55 ASN00087031.dly
    
    as required by the policy.
    
  This submission remove the package `rnoaa` as a dependency (suggest). 
