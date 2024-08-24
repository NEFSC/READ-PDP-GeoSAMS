% Adding a leap year every 4 years adds 1/4 to the 365-day year 
% or increases the average year-length by 0.25 days.
% Omitting a leap year every 100 years subtracts 1/100 = 0.01 days from the 365-day year.
% Adding a leap year every 400 years adds 1/400 = 0.0025 days to the 365-day year.
% Omitting a leap year every 4000 years subtracts 1/4000 = 0.000 25 days from the 365-day year.
%    (admittedly not concerned about this one, but for completeness)
% When these are combined, the corrected average year-length is 
%     365 + 0.25 - 0.01 + 0.0025 - 0.00025 = 365.24225 days/year.
function x = LeapYear(year)

if (DivBy(year,400) && ~DivBy(year,4000) || ~DivBy(year,100)) ...
    && DivBy(year,4) 
    x = 1;
else
    x = 0;
end
end

function x = DivBy(y,val)
   x = mod(y,val) == 0;
end