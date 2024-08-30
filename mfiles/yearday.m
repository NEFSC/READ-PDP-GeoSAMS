function yd=yearday(mon,day,h,leapyr)
% YEARDAY: converts calender month, day, and hour into
%  fractional value of days since beginning of the year (0-364.9999)
% yd = YEARDAY(mon,day,leapyr) converts calender month and day into yearday.
% If year is not a leap year, then omit the third input variable. If year
% is a leap year, then enter leapyr=1.
%
%   INPUT:  mon    - month
%           day    - day
%           leapyr - set to 1 if it is a leap year, otherwise omit
%
%   OUTPUT: yd - year day
%
%   Examples:  15 March 1995 = yearday(3,15) = 74
%              15 March 1996 = yearday(3,15,1) = 75
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3/8/97: version 1.0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
m=mon;
d=day - 1;
% compute yd for non-leap year
if m==1,yd=d;end
if m==2,yd=31+d;end
if m==3,yd=59+d;end
if m==4,yd=90+d;end
if m==5,yd=120+d;end
if m==6,yd=151+d;end
if m==7,yd=181+d;end
if m==8,yd=212+d;end
if m==9,yd=243+d;end
if m==10,yd=273+d;end
if m==11,yd=304+d;end
if m==12,yd=334+d;end
if nargin > 2, yd=yd+h/24;end
if nargin == 2, yd=yd+1;end
% adjust for leap year
if nargin > 3
  if (leapyr==1 && m>=3)
     yd=yd+1;
  end 
end


