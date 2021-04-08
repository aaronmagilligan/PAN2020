copy %1.eps eps\%1.eps
cd eps
call ps2pdf %1.eps %1.pdf
cd ..
copy eps\%1.pdf ..\results\%1.pdf
rem del eps\%1.eps
rem del eps\%1.pdf
rem del %1.eps