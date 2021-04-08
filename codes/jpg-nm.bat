copy nm.eps gs\
cd gs
gs -sDEVICE=jpeg -r200x200 -dEPSCrop -o nm.jpg nm.eps
cd ..
copy gs\nm.jpg ..\results-nm\nm.jpg
rem del eps\nm.eps
rem del eps\nm.pdf
rem del nm.eps