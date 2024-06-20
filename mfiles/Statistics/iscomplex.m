% Unlike Octave, Matlab does not have an intrinsic iscomplex
function r = iscomplex(a)
r = ~isreal(a);
return
end
