
#logistic
forget()
from sage.symbolic.integration.integral import definite_integral

clear_vars()

var('x,l')
assume(l>0)
f=exp(-(x/l)^2)

forget()
from sage.symbolic.integration.integral import definite_integral

clear_vars()

var('x,l')

assume(x,'real')
assume(l,'real')
assume(l>0)
f=1/(1+exp( x / l ) )

f1=derivative(f, x)
f1.full_simplify()
f2=derivative(f1, x)
f2.full_simplify()
fp=f2^2
fp.full_simplify()
p=definite_integral(fp,x,-oo,oo)
p.full_simplify()


#Gaussian
forget()
from sage.symbolic.integration.integral import definite_integral

clear_vars()

var('x,l')
assume(l>0)
f=exp(-(x/l)^2)

f1=derivative(f, x)
f1.full_simplify()
f2=derivative(f1, x)
f2.full_simplify()
f22=f2^2
f22.full_simplify()
a=definite_integral(f22,x,-oo,oo)
a.full_simplify()


forget()
from sage.symbolic.integration.integral import definite_integral

clear_vars()
var('x,l')

assume(x,'real')
assume(l,'real')
assume(l>0)

f=exp(-(x/l)^2) * sin(x/l)

f1=derivative(f, x)
f1.full_simplify()
f2=derivative(f1, x)
f2.full_simplify()
fp=f2^2
fp.full_simplify()
p=definite_integral(fp,x,-oo,oo)
p.full_simplify()



forget()
from sage.symbolic.integration.integral import definite_integral

clear_vars()
var('x,l')

assume(x,'real')
assume(l,'real')
assume(l>0)

f=exp(-(x/(2*l))^2) * cos(x/l)

f1=derivative(f, x)
f1.full_simplify()
f2=derivative(f1, x)
f2.full_simplify()
fp=f2^2
fp.full_simplify()
p=definite_integral(fp,x,-oo,oo)
p.full_simplify()

1/32*sqrt(2)*sqrt(pi)*(43*e^2 + 3)*e^(-2)/l^3
