## Copyright (C) 2022 keston
## 
## This program is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see
## <https://www.gnu.org/licenses/>.

## -*- texinfo -*- 
## @deftypefn {} {@var{retval} =} InterpSurvn (@var{input1}, @var{input2})
##
## @seealso{}
## @end deftypefn

## Author: keston <keston@findbox>
## Created: 2022-04-26

function fi = InterpSurvn (xg,yg,fg,xi,yi);
n=length(xi);
fi=0*xi;
for k=1:n
  D=(xg-xi(k)).^2+(yg-yi(k)).^2;
  [m,j]=min(D);
  fi(k)=fg(j);
  
endfor

endfunction
