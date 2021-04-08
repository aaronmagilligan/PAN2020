copy %1.eps gs\%1.eps
cd gs
gs -sDEVICE=jpeg -r200x200 -dEPSCrop -o %1.jpg %1.eps
cd ..
copy gs\%1.jpg ..\results\%1.jpg
del gs\%1.eps
del gs\%1.jpg