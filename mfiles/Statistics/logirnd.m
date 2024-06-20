% Copyright (C) 2012 Rik Wehbring
% Copyright (C) 1995-2016 Kurt Hornik
% Copyright (C) 2023 Andreas Bertsatos <abertsatos@biol.uoa.gr>
%
% This file is part of the statistics package for GNU Octave.
%
% This program is free software: you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation, either version 3 of the
% License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful, but
% WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; see the file COPYING.  If not, see
% <http://www.gnu.org/licenses/>.

% -*- texinfo -*-
% @deftypefn  {statistics} {@var{r} =} logirnd (@var{mu}, @var{s})
% @deftypefnx {statistics} {@var{r} =} logirnd (@var{mu}, @var{s}, @var{rows})
% @deftypefnx {statistics} {@var{r} =} logirnd (@var{mu}, @var{s}, @var{rows}, @var{cols}, @dots{})
% @deftypefnx {statistics} {@var{r} =} logirnd (@var{mu}, @var{s}, [@var{sz}])
%
% Random arrays from the logistic distribution.
%
% @code{@var{r} = logirnd (@var{mu}, @var{s})} returns an array of
% random numbers chosen from the logistic distribution with location parameter
% @var{mu} and scale parameter @var{s}.  The size of @var{r} is the common size
% of @var{mu} and @var{s}.  A scalar input functions as a constant matrix of
% the same size as the other inputs.
%
% Both parameters must be reals and @qcode{@var{s} > 0}.
% For @qcode{@var{s} <= 0}, @qcode{NaN} is returned.
%
% When called with a single size argument, @code{logirnd} returns a square
% matrix with the dimension specified.  When called with more than one scalar
% argument, the first two arguments are taken as the number of rows and columns
% and any further arguments specify additional matrix dimensions.  The size may
% also be specified with a row vector of dimensions, @var{sz}.
%
% Further information about the logistic distribution can be found at
% @url{https://en.wikipedia.org/wiki/Logistic_distribution}
%
% @seealso{logcdf, logiinv, logipdf, logifit, logilike, logistat}
% @end deftypefn

function r = logirnd (mu, s, varargin)

  % Check for valid number of input arguments
  if (nargin < 2)
    error ("logirnd: function called with too few input arguments.");
  end

  % Check for common size of MU, and S
  if (~ isscalar (mu) || ~ isscalar (s))
    [retval, mu, s] = common_size (mu, s);
    if (retval > 0)
      error ("logirnd: MU and S must be of common size or scalars.");
    end
  end

  % Check for X, MU, and S being reals
  if (iscomplex (mu) || iscomplex (s))
    error ("logirnd: MU and S must not be complex.");
  end

  % Parse and check SIZE arguments
  if (nargin == 2)
    sz = size (mu);
  elseif (nargin == 3)
    if (isscalar (varargin{1}) && varargin{1} >= 0 ...
                               && varargin{1} == fix (varargin{1}))
      sz = [varargin{1}, varargin{1}];
    elseif (isrow (varargin{1}) && all (varargin{1} >= 0) ...
                                && all (varargin{1} == fix (varargin{1})))
      sz = varargin{1};
    elseif error (strcat (["logirnd: SZ must be a scalar or a row vector"], ...
                     [" of non-negative integers."]));
    end
  elseif (nargin > 3)
    posint = cellfun (@(x) (~ isscalar (x) || x < 0 || x ~= fix (x)), varargin);
    if (any (posint))
      error ("logirnd: dimensions must be non-negative integers.");
    end
    sz = [varargin{:}];
  end

  % Check that parameters match requested dimensions in size
  if (~ isscalar (mu) && ~ isequal (size (mu), sz))
    error ("logirnd: MU and S must be scalars or of size SZ.");
  end

  % Check for class type
  if (isa (mu, "single") || isa (s, "single"))
    is_type = "single";
  else
    is_type = "double";
  end

  % Generate random sample from logistic distribution
  r = - log (1 ./ rand (sz, is_type) - 1) .* s + mu;

  % Force output to NaN for invalid parameter S <= 0
  k = (s <= 0);
  r(k) = NaN;

end

% Test output
%~assert (size (logirnd (1, 1)), [1 1])
%~assert (size (logirnd (1, ones (2,1))), [2, 1])
%~assert (size (logirnd (1, ones (2,2))), [2, 2])
%~assert (size (logirnd (ones (2,1), 1)), [2, 1])
%~assert (size (logirnd (ones (2,2), 1)), [2, 2])
%~assert (size (logirnd (1, 1, 3)), [3, 3])
%~assert (size (logirnd (1, 1, [4, 1])), [4, 1])
%~assert (size (logirnd (1, 1, 4, 1)), [4, 1])
%~assert (size (logirnd (1, 1, 4, 1, 5)), [4, 1, 5])
%~assert (size (logirnd (1, 1, 0, 1)), [0, 1])
%~assert (size (logirnd (1, 1, 1, 0)), [1, 0])
%~assert (size (logirnd (1, 1, 1, 2, 0, 5)), [1, 2, 0, 5])

% Test class of input preserved
%~assert (class (logirnd (1, 1)), "double")
%~assert (class (logirnd (1, single (1))), "single")
%~assert (class (logirnd (1, single ([1, 1]))), "single")
%~assert (class (logirnd (single (1), 1)), "single")
%~assert (class (logirnd (single ([1, 1]), 1)), "single")

% Test input validation
%~error<logirnd: function called with too few input arguments.> logirnd ()
%~error<logirnd: function called with too few input arguments.> logirnd (1)
%~error<logirnd: MU and S must be of common size or scalars.> ...
%~ logirnd (ones (3), ones (2))
%~error<logirnd: MU and S must be of common size or scalars.> ...
%~ logirnd (ones (2), ones (3))
%~error<logirnd: MU and S must not be complex.> logirnd (i, 2, 3)
%~error<logirnd: MU and S must not be complex.> logirnd (1, i, 3)
%~error<logirnd: SZ must be a scalar or a row vector of non-negative integers.> ...
%~ logirnd (1, 2, -1)
%~error<logirnd: SZ must be a scalar or a row vector of non-negative integers.> ...
%~ logirnd (1, 2, 1.2)
%~error<logirnd: SZ must be a scalar or a row vector of non-negative integers.> ...
%~ logirnd (1, 2, ones (2))
%~error<logirnd: SZ must be a scalar or a row vector of non-negative integers.> ...
%~ logirnd (1, 2, [2 -1 2])
%~error<logirnd: SZ must be a scalar or a row vector of non-negative integers.> ...
%~ logirnd (1, 2, [2 0 2.5])
%~error<logirnd: dimensions must be non-negative integers.> ...
%~ logirnd (1, 2, 2, -1, 5)
%~error<logirnd: dimensions must be non-negative integers.> ...
%~ logirnd (1, 2, 2, 1.5, 5)
%~error<logirnd: MU and S must be scalars or of size SZ.> ...
%~ logirnd (2, ones (2), 3)
%~error<logirnd: MU and S must be scalars or of size SZ.> ...
%~ logirnd (2, ones (2), [3, 2])
%~error<logirnd: MU and S must be scalars or of size SZ.> ...
%~ logirnd (2, ones (2), 3, 2)
