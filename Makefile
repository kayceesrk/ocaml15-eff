all: concurrent generator reify_reflect simple

simple: simple_1 simple_2

simple_1: simple_1.ml
	ocamlc -o simple_1 simple_1.ml

simple_2: simple_2.ml
	ocamlc -o simple_2 simple_2.ml

concurrent: sched.mli sched.ml concurrent.ml
	ocamlc -o concurrent sched.mli sched.ml concurrent.ml

generator: generator.ml
	ocamlc -o generator generator.ml

reify_reflect: reify_reflect.ml
	ocamlc -o reify_reflect reify_reflect.ml

clean:
	rm -f *.cmi *.cmo *.o concurrent generator *~ a.out state reify_reflect ref \
		transaction aio simple_1 simple_2 *cmx
	make -Cchameneos-redux clean
