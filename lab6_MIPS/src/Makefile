buildbase = ../build
bdir = $(buildbase)/bdir
simdir = $(buildbase)/simdir
libbase = ../lib
proclib	= $(libbase)/processor
memlib = $(libbase)/memory
comlib = $(libbase)/common

all:
	rm -rf $(bdir)/* $(simdir)/* $(buildbase)/*.so $(buildbase)/*cyc* $(buildbase)/*stg* code.mem out.tex
	bsc -u -sim -aggressive-conditions -simdir $(simdir) -bdir $(bdir) -p $(proclib):$(memlib):$(comlib):+ $(comlib)/TestBench.bsv
	bsc -u -sim -aggressive-conditions -simdir $(simdir) -bdir $(bdir) -p $(proclib):$(memlib):$(comlib):+ -o $(buildbase)/test -e mkTB

decode:
	rm -rf $(bdir)/* $(simdir)/* $(buildbase)/*.so $(buildbase)/*cyc* $(buildbase)/*stg* code.mem out.tex
	bsc -u -sim -aggressive-conditions -simdir $(simdir) -bdir $(bdir) -p $(proclib):$(memlib):$(comlib):+ $(comlib)/TestBench.bsv
	bsc -u -sim -aggressive-conditions -simdir $(simdir) -bdir $(bdir) -p $(proclib):$(memlib):$(comlib):+ -o $(buildbase)/test -e mkTB


clean:
	rm -rf $(bdir)/* $(simdir)/* $(buildbase)/*.so $(buildbase)/*cyc* $(buildbase)/*stg* $(buildbase)/test code.mem out.tex

.PHONY: a.out
