copy %1-den.eps eps\
cd eps
call ps2pdf %1-den.eps %1-den.pdf
cd ..
copy eps\%1-den.pdf ..\results-density\%1-den.pdf
rem del eps\%1-den.eps
rem del eps\%1-den.pdf
rem del %1-den.eps