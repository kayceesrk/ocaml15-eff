include ../config.mk

REAGENTS_PATH=/home/kc/reagents/_build

all: chameneos-ghc chameneos-eff chameneos-monad mvar-test

mvar-test: MVar_test.ml
	$(OCAMLC) -o MVar_test sched.mli sched.ml MVar.mli MVar.ml MVar_test.ml

#Will only work with vanilla ocaml
chameneos-systhr: chameneos_systhr.ml
	ocamlfind $(OCAMLC) -o chameneos-systhr -thread -package threads -linkpkg chameneos_systhr.ml

#Will only work with vanilla ocaml
chameneos-lwt: chameneos_lwt.ml
	ocamlfind $(OCAMLC) -o chameneos-lwt -thread -package lwt,lwt.unix,threads -linkpkg chameneos_lwt.ml

chameneos-ghc: chameneos.hs
	ghc -o chameneos-ghc -cpp -XBangPatterns -XScopedTypeVariables \
	-XGeneralizedNewtypeDeriving chameneos.hs

#Needs multicore branch
chameneos-eff: chameneos.ml
	$(OCAMLC) -o chameneos-eff unix.cma Backoff.ml CAS.ml MSQueue.ml sched_ws.ml MVar.mli MVar.ml chameneos.ml

chameneos-monad: chameneos_monad.ml
	$(OCAMLC) -o chameneos_monad sched_monad.mli sched_monad.ml MVar_monad.mli MVar_monad.ml chameneos_monad.ml

clean:
	rm -f *.cmi *.cmx a.out *~ eff *.cmo MVar_test chameneos-eff chameneos-ghc \
	*.hi *.o chameneos-systhr chameneos-lwt *.annot *.s op chameneos_monad *.prof_*
