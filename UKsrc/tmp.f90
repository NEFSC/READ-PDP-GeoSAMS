!
! Generate random samples from N(mu,Cbeta)+N(0,Ceps) where Ceps is the
! covariance matrix coresponding to the variogram with
! parameters(c0,c1,alpha,form)
!
!A=0.
!do n=1,nn
! Veps(n)=Ceps(n,n)
!enddo
!if(IsClimEst)grid%f(1:nn)=grid%f(1:nn)+ClimEst(1:nn)
!logmu(1:nn)=grid%f(1:nn)
!if(IsLogT) grid%f(1:nn)=SF*exp( grid%f(1:nn) + Veps(1:nn)/2. ) - A  ! adjusted inverse log(A+f)
!if(IsHiLimit)call limit(nn,grid%f,0.D0,fmax)
!if(IsHiLimit)call limitz(nn,grid%f,grid%z,fmax,DomainName)
!MuY=sum( grid%f(1:nn) )/float(nn)
!write(*,*)'DA0,DA1',MuY,DomainAverage,DomainAverage/SF

!if(IsMatchMean)grid%f(1:nn)=DomainAverage*grid%f(1:nn)/MuY
!
!if(IsHiLimit)call limit(nn,grid%f,0.D0,fmax)
!if(IsHiLimit)call limitz(nn,grid%f,grid%z,fmax,DomainName)
!call UK_RandomField(grid,nf,beta,Cbeta,eps,Ceps,Nsim,nlsf,IsLogT,grid%f,A,SF,&
 !               0.D0,fmax,grid%z,logmu)
!-------------------------------------------------------------------------------------------                
!output variance                 
!call write_csv(nf,nf,Cbeta,'CovBeta.csv',nf)

!do n=1,nn
! Veps(n)=Ceps(n,n)
!enddo
!if(.not.IsLogT)call WriteScalarField(nn,Veps,'Veps.txt')
!if(IsLogT)call WriteScalarField(nn,Veps,'VepsLog.txt')
!if(IsLogT)Veps(1:nn)=SF*exp(sqrt(Veps(1:nn)))-A 
!call WriteScalarField(nn,Veps,'Veps.txt')
!if(IsLogT)call WriteScalarField(nn,SF*exp(sqrt(Veps(1:nn)))-A,'EpsSTD.txt')

!call spatial_function(grid,Fg,nf,nn,nlsf)

!Ceps(1:nn,1:nn)=Ceps(1:nn,1:nn)+matmul( Fg(1:nn,1:nf) ,matmul(Cbeta(1:nf,1:nf),transpose(Fg(1:nn,1:nf)) ) )
!do n=1,nn
! VSpFn(n)=Ceps(n,n)-Veps(n)
!enddo

 !call dpotrf('L',nn,Ceps,nn,k )
 !write(*,*)'dpotrf in RandomSample, info=',k
!Ceps(1:nn,1:nn)=SF*exp( Ceps(1:nn,1:nn))-A
! do k=1,nn-1
!  Ceps(k,k+1:nn)=0.
! enddo
!call IndStdNRV(nn,NRand,RandomField)
!RandomField(1:nn,1:NRand)=matmul(Ceps(1:nn,1:nn),RandomField(1:nn,1:NRand))
!do k=1,nn
!    RandomField(k,1:NRand)=grid%f(k)+RandomField(k,1:NRand)
!enddo

!call RandomSampleF(nn,nn,NRand,logmu(1:n),Ceps,RandomField)
!RandomField(1:nn,1:NRand)=SF*exp( RandomField(1:nn,1:NRand) )-A
!do k=1,nn
!    call limitz(Nsim,RandomField(k,1:NRand),grid%z(k)+0.*RandomField(k,1:NRand),fmax,DomainName)
!enddo
!call write_csv(nn,NRand,RandomField,'RandomField.csv',nn)
!do k=1,nn
!    call limitz(Nsim,RandomField(k,1:NRand),grid%z(k)+0.*RandomField(k,1:NRand),fmax,DomainName)
!    EnsMu=sum( RandomField(k,1:Nrand) )/float(Nrand)
!    if(EnsMu.gt.0.D0)then
!        adj=min( grid%f(k) / EnsMu, 1.D0)
!        RandomField(k,1:Nrand) = RandomField(k,1:Nrand)*adj
!    endif
!enddo
!call write_csv(nn,NRand,RandomField,'RandomFieldadj.csv',nn)
do k=1,nn
    adj=SF*exp(sqrt(VSpFn(k)+Veps(k)))-A
    EnsMu=sum(  RandomField(k,1:Nrand) )/float(Nrand)
    EnsSTD=sqrt( sum( ( RandomField(k,1:Nrand)-EnsMu )**2 )/float(Nrand) )
    if(EnsSTD.gt.0.)then
        RandomField(k,1:Nrand) = grid%f(k) +( RandomField(k,1:Nrand) - EnsMu )*adj/EnsSTD
    else
        RandomField(k,1:Nrand) = grid%f(k) + RandomField(k,1:Nrand) - EnsMu 
    endif
    call limitz(Nsim,RandomField(k,1:NRand),grid%z(k)+0.*RandomField(k,1:NRand),fmax,DomainName)
        
enddo
call write_csv(nn,NRand,RandomField,'RandomField.csv',nn)




call WriteScalarField(nn,VSpFn,'VSpFn.txt')
if(IsLogT)call WriteScalarField(nn,VSpFn,'VSpFnLog.txt')
!if(IsLogT)VSpFn(1:nn)=SF*exp(sqrt(VSpFn(1:nn)))-A 
if(IsLogT)call WriteScalarField(nn,SF*exp(sqrt(VSpFn(1:nn)))-A,'VSpFn.txt')
if(IsLogT)call WriteScalarField(nn,SF*exp(sqrt(VSpFn(1:nn)+Veps(1:nn)))-A,'KrigSTD.txt')

call WriteScalarField(nn,grid%f,'KrigingEstimate.txt')

trend(1:nn)=matmul( Fg(1:nn,1:nf),Beta(1:nf)) 

if(IsClimEst)trend(1:nn)=trend(1:nn)+ClimEst(1:nn)
if(IsLogT)trend(1:nn)=SF*exp(trend(1:nn))-A
if(IsMatchMean)trend(1:nn)=DomainAverage*trend(1:nn)/MuY
call WriteScalarField(nn,trend,'SpatialTrend.txt')

call WriteScalarField(nf,Beta,'beta.txt')
call WriteScalarField(nn,eps,'epsilon.txt')

Vtotal(1:nn)=VSpFn(1:nn)+Veps(1:nn)

!call WriteScalarField(nn,Vtotal,'KrigSTD.txt')

!call InterpFromGrid(grid,grid%f,obs,ClimEstObs)
!call WriteScalarField(no,ClimEstObs,'KEatObs.txt')
