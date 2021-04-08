copy %1-pot.eps eps\
cd eps
call ps2pdf %1-pot.eps %1-pot.pdf
cd ..
copy eps\%1-pot.pdf ..\results-pot\%1-pot.pdf
rem del eps\%1-pot.eps
rem del eps\%1-pot.pdf
rem del %1-pot.eps