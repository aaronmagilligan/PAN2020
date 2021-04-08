copy %1-pot.eps gs\
cd gs
gs -sDEVICE=jpeg -r200x200 -dEPSCrop -o %1-pot.jpg %1-pot.eps
cd ..
copy gs\%1-pot.jpg ..\results-pot\%1-pot.jpg
rem del eps\%1-pot.eps
rem del eps\%1-pot.pdf
rem del %1-pot.eps