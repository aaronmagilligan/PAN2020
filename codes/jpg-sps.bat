copy %1-sps.eps gs\
cd gs
gs -sDEVICE=jpeg -r200x200 -dEPSCrop -o %1-sps.jpg %1-sps.eps
cd ..
copy gs\%1-sps.jpg ..\results-levels\%1-sps.jpg
rem del eps\%1-sps.eps
rem del eps\%1-sps.pdf
rem del %1-sps.eps