copy %1-den.eps gs\
cd gs
gs -sDEVICE=jpeg -r200x200 -dEPSCrop -o %1-den.jpg %1-den.eps
cd ..
copy gs\%1-den.jpg ..\results-density\%1-den.jpg
rem del eps\%1-den.eps
rem del eps\%1-den.pdf
rem del %1-den.eps