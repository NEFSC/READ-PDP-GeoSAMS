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
## @deftypefn {} {@var{retval} =} std90 (@var{input1}, @var{input2})
##
## @seealso{}
## @end deftypefn

## Author: keston <keston@findbox>
## Created: 2022-04-19

function s = stdOL(X,lambda)
  [n,m]=size(X);
  m1=floor(m*lambda);
  m2=m-m1;
  %s=zeros(n,1);
  Xs=sort(X,2);
  s=std(Xs(:,m1:m2)')';
  %for j=1:n,
  %  x=sort(X(j,:));
  %  s(j)=std(x(m1:m2));
  %endfor

endfunction
