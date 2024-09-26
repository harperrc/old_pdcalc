PROGRAM=	main

OBJECTS=	main.o \
		acon.o \
		dypres.o \
		errmsg.o \
		etcalc.o \
		intgf.o \
		lncalc.o \
		ogh.o \
		overp.o \
		pdcalc.o \
		ranf.o \
		wrcalc.o \
		wrclcy.o \
		wrcrtr.o \
		wrpers.o

F77=	pgf77
FLAGS=	-Wall -Msave -Ktrap=divz,denorm,fp,ovf -C -g
LIBS=	
INCLUDES=	

F77=	gfortran
FLAGS=	-O3 -Wall -Wunused-parameter -fno-range-check -std=legacy -ffpe-trap=zero,denormal,invalid,overflow -C -g
LIBS=	
INCLUDES=	

$(PROGRAM):	$(OBJECTS)
	$(F77) $(INCLUDES) $(FLAGS) -o $(PROGRAM) $(OBJECTS) $(LIBS)

main.o:	main.for\
	acon.f \
	dypres.f \
	errmsg.f \
	etcalc.f \
	intgf.f \
	lncalc.f \
	ogh.f \
	overp.f \
	pdcalc.f \
	ranf.f \
	wrcalc.f \
	wrclcy.f \
	wrcrtr.f \
	wrpers.f \
	drv1.x \
	drv2.x \
	drv3.x \
	drv4.x \
	drv5.x \
	drv6.x \
	drv7.x \
	drv8.x
	$(F77) $(INCLUDES) $(FLAGS) -c $<

lib:	$(OBJECTS)
	ar rv libpd.a $(OBJECTS)
	ranlib libpd.a

acon.o:	acon.f
	$(F77) $(INCLUDES) $(FLAGS) -c $<

dypres.o:	dypres.f \
	real8.h
	$(F77) $(INCLUDES) $(FLAGS) -c $<

errmsg.o:	errmsg.f
	$(F77) $(INCLUDES) $(FLAGS) -c $<

etcalc.o:	etcalc.f
	$(F77) $(INCLUDES) $(FLAGS) -c $<

intgf.o:	intgf.f
	$(F77) $(INCLUDES) $(FLAGS) -c $<

lncalc.o:	lncalc.f
	$(F77) $(INCLUDES) $(FLAGS) -c $<

ogh.o:	ogh.f \
	cdkpd.h cdkwr.h
	$(F77) $(INCLUDES) $(FLAGS) -c $<

overp.o:	overp.f\
	real8.h
	$(F77) $(INCLUDES) $(FLAGS) -c $<

pdcalc.o:	pdcalc.f
	$(F77) $(INCLUDES) $(FLAGS) -c $<

ranf.o:	ranf.f\
	basicd.h 
	$(F77) $(INCLUDES) $(FLAGS) -c $<

wrcalc.o:	wrcalc.f\
	cdkwr.h 
	$(F77) $(INCLUDES) $(FLAGS) -c $<

wrclcy.o:	wrclcy.f
	$(F77) $(INCLUDES) $(FLAGS) -c $<

wrcrtr.o:	wrcrtr.f
	$(F77) $(INCLUDES) $(FLAGS) -c $<

wrpers.o:	wrpers.f
	$(F77) $(INCLUDES) $(FLAGS) -c $<

clean:
	rm *.o $(PROGRAM) libpd.a
