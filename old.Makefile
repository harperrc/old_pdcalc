PROGRAM=	main.e
OBJECTS=	main.o errmsg.o etcalc.o intgf.o lncalc.o pdcalc.o wrcalc.o wrclcy.o \
		wrcrtr.o wrpers.o acon.o ranf.o 

F77    =	lf95
FLAGS  =	--chk aesu
LIBS   =	

F77    =	gfortran
FLAGS  =	-Wall -C
LIBS   =	

F77    =	pgf77
FLAGS  =	-Wall -Msave -Ktrap=divz,denorm,fp,ovf -C -g
LIBS   =	


$(PROGRAM):	$(OBJECTS)
	$(F77) $(FLAGS) -o $(PROGRAM) $(OBJECTS) $(LIBS)

main.o:	main.f pdcalc.f acon.f \
		basicd.h const.h files.h real8.h 
	$(F77) $(FLAGS) -c $<

errmsg.o:	errmsg.f const.h files.h real8.h 
	$(F77) $(FLAGS) -c $<

etcalc.o:	etcalc.f wrcalc.f wrcrtr.f \
		const.h files.h real8.h 
	$(F77) $(FLAGS) -c $<

intgf.o:	intgf.f const.h real8.h 
	$(F77) $(FLAGS) -c $<

lncalc.o:	lncalc.f intgf.f \
		const.h files.h real8.h 
	$(F77) $(FLAGS) -c $<

pdcalc.o:	pdcalc.f errmsg.f etcalc.f lncalc.f wrcalc.f wrclcy.f wrpers.f \
		const.h files.h real8.h 
	$(F77) $(FLAGS) -c $<

wrcalc.o:	wrcalc.f const.h files.h real8.h 
	$(F77) $(FLAGS) -c $<

wrclcy.o:	wrclcy.f const.h files.h real8.h 
	$(F77) $(FLAGS) -c $<

wrcrtr.o:	wrcrtr.f const.h real8.h 
	$(F77) $(FLAGS) -c $<

wrpers.o:	wrpers.f const.h files.h real8.h 
	$(F77) $(FLAGS) -c $<

acon.o:	acon.f const.h files.h real8.h 
	$(F77) $(FLAGS) -c $<

ranf.o:	ranf.f basicd.h real8.h 
	$(F77) $(FLAGS) -c $<

clean:
	rm *.o $(PROGRAM)
