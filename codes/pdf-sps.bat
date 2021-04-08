copy %1-sps.eps eps\
cd eps
call ps2pdf %1-sps.eps %1-sps.pdf
cd ..
copy eps\%1-sps.pdf ..\results-levels\%1-sps.pdf
rem del eps\%1-sps.eps
rem del eps\%1-sps.pdf
rem del %1-sps.eps