program newt_raphson	  
implicit none

	real:: error, k, D, Re, tol, f, df, x0, xi, fr
	write(*,*)"=============== "
	write(*,*)"    TIPO I     "
	write(*,*)"==============="
	write(*,*)" Ingrese Datos: "
	write(*,*)"->              "

	write(*,*)"Rugosidad [mm]"
	read(*,*)k

	write(*,*)"Diametro [m]"
	read(*,*)D

	write(*,*)"Reynolds [unit.]"
	read(*,*)Re

	write(*,*)"Tolerancia [unit]"
	read(*,*)tol

	write(*,*)"Error inicial [unit]"
	read(*,*)error

	write(*,*)"Semilla [unit]"
	read(*,*)x0

	

	do while (error>=Tol)
		!f=-(2*log((k/(3.7*D))+((2.51*x0)/(Re))))-x0
		f=-2*log10(k / (3.7 * D) + 2.51 * x0 / Re) - x0

		!df=-((2/2.30258509299)*((2.51/Re)/((k/(3.7*D))+((2.51*x0)/Re))))-1
		df=-2 / 2.30258509299 * ((2.51 / Re) / (k / (3.7 * D) + 2.51 * x0 / Re)) - 1
		
			
		xi=x0-(f/df)	
	    error=abs((xi-x0)/xi)
		fr=1/(xi*xi)
		x0=xi
		write(*,*)fr
	end do
	

	write (*,*)"Valor de friccion = ", fr
	pause
	stop
 end program
