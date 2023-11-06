  function yd=yearday(mon,day,h,leapyr)
% YEARDAY: converts calender month and day into yearday.
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
[T,r]=size(mon);
yd=zeros(T,1);
for t=1:T,
	m=mon(t);
	d=day(t);
	% compute yd for non-leap year
	if m==1,y=d;end;
	if m==2,y=31+d;end;
	if m==3,y=59+d;end;
	if m==4,y=90+d;end;
	if m==5,y=120+d;end;
	if m==6,y=151+d;end;
	if m==7,y=181+d;end;
	if m==8,y=212+d;end;
	if m==9,y=243+d;end;
	if m==10,y=273+d;end;
	if m==11,y=304+d;end;
	if m==12,y=334+d;end;
	yd(t)=y+h(t)/24;
	% adjust for leap year
	if nargin > 3,
	  if (leapyr==1 & m>=3)
	     yd(t)=yd(t)+1;
	  end 
  end
end


