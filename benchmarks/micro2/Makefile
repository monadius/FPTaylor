FPTAYLOR_HOME=../..
ROSA_HOME=~/Work/Solvers/rosa
GAPPA_HOME=~/Work/Solvers/gappa-1.0.0/src

ROSA=$(ROSA_HOME)/rosa --real --specGen --z3Timeout=1000

GAPPA=$(GAPPA_HOME)/gappa

gappa:
	time $(GAPPA) Gappa/f1.g

gappa-hints:
	time $(GAPPA) Gappa/f1_hints_abs.g
	time $(GAPPA) Gappa/f1_hints_rel.g

generated:
	mkdir -p generated

rosa-f1: generated
	$(ROSA) --functions=f1 rosa/benchmarks.scala

rosa-sqrt-sub: generated
	$(ROSA) --functions=sqrt_sub rosa/benchmarks.scala

rosa-sqrt-add: generated
	$(ROSA) --functions=sqrt_add rosa/benchmarks.scala

clean:
	rm -rf tmp generated log *.log *~



