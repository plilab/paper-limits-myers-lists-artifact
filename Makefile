all: myers-list

# NOTE: order matters here, you must avoid forward references
SRC = \
  src/util.ml \
  src/myers.ml \
  src/myers_noop.ml \
  src/myers_list.ml \
  src/myers_pair.ml \
  src/myers_cons.ml \
  src/myers_3lgn.ml \
  src/myers_2lgn.ml \
  src/myers_1lgn.ml \
  src/benchmarking.ml \
  src/main.ml

myers-list: $(SRC)
	ocamlfind ocamlopt -O3 -w +A \
		-I src \
		-o myers-list \
		-linkpkg -thread \
		-package core_bench \
		-package ppx_let \
		$+

clean:
	rm -f myers-list
	rm -f src/*.cmi src/*.cmx src/*.o


tests: myers-list 
	mkdir -p results
	@echo "Running benchmarks..."
	@echo "Waiting for all processes to complete. This will take 1 to 3 hours"
	./myers-list distance list -impl 3lgn -opt -length 0-100000 > results/3lgn.txt & \
	./myers-list distance list -impl 2lgn -opt -length 0-100000 > results/2lgn.txt & \
	./myers-list distance list -impl 1lgn -skip 2 -opt -length 0-100000 > results/1lgn-skip-2.txt & \
	./myers-list distance list -impl 1lgn -skip 4 -opt -length 0-100000 > results/1lgn-skip-4.txt & \
	./myers-list distance list -impl 1lgn -skip 8 -opt -length 0-100000 > results/1lgn-skip-8.txt & \
	./myers-list distance list -impl 1lgn -skip 16 -opt -length 0-100000 > results/1lgn-skip-16.txt & \
	wait;
	@echo "All benchmarks complete. Results are written in results/"

charts:
	@echo "Making charts..."
	mkdir -p charts/
	python3 mk_csv.py 3lgn
	python3 mk_csv.py 2lgn
	python3 mk_csv.py 1lgn-skip-2
	python3 mk_csv.py 1lgn-skip-4
	python3 mk_csv.py 1lgn-skip-8
	python3 mk_csv.py 1lgn-skip-16
	python3 mk_plots.py 3lgn
	python3 mk_plots.py 2lgn
	python3 mk_plots.py 1lgn-skip-2
	python3 mk_plots.py 1lgn-skip-4
	python3 mk_plots.py 1lgn-skip-8
	python3 mk_plots.py 1lgn-skip-16
	python3 theory.py
	rm results/*.csv
	@echo "All charts complete. Results are written in charts/."
	@echo "To copy the charts to the host machine, run the following command from the host machine:"
	@echo "scp -P 5555 artifact@localhost:charts/\"*.svg\" ."

clean:
	rm -f myers-list
	rm -f src/*.cmi src/*.cmx src/*.o


########################################
# Benchmarks
bench: all
	./myers-list bench-make-tree -height 10 -skip 4 -impl all +time
	./myers-list bench-make-list -length 1_000_000 -skip 4 -impl most +time
	./myers-list bench-make-list -length 1_000_000 -skip 4 -impl 1lgn +time -quota 20 # 1lgn needs a larger quota to work

########################################
# Distances
distance-map: all
	./myers-list distance-list -length 0-128 -impl 3lgn -array|perl -ne 's/.*\[\|//; s/[;|\]]//g; @x = /\b\d+\b/g; chomp; print; print(" 0" x (128 - $$#x), "\n")' >map.data
	gnuplot map.gnuplot

distance-chart: all
	./myers-list distance-list -length 0-128 -impl 3lgn | perl -ane 'print "$$F[9] $$F[11]\n"' >chart.data
	gnuplot chart.gnuplot

########################################
# Tests (Takes ~1 minute)
test: test.make-tree/make-list test.length-of-height test.find/index test.find-path/index-path test.find/find-path test.1lgn

test.make-tree/make-list: all
	########################################
	# Test make_tree_rev vs make_list_rev
	./myers-list test.make-tree/make-list -height 1-14 -skip 2 -impl cons | grep -v ': true$$' | cat # TODO: cons can't get to 15 for some reason
	./myers-list test.make-tree/make-list -height 1-15 -skip 2 -impl noop,list,pair,3lgn,2lgn | grep -v ': true$$' | cat
	for i in $$(seq 3 10); do ./myers-list test.make-tree/make-list -height 1-15 -skip $$i -impl 1lgn; done | grep -v ': true$$' | cat

test.length-of-height: all
	########################################
	# Test length_of_height
	./myers-list test.length-of-height -height 1-15 -skip 2 -impl most | grep -v ': true$$' | cat
	for i in $$(seq 2 10); do ./myers-list test.length-of-height -height 1-15 -skip $$i -impl 1lgn; done | grep -v ': true$$' | cat

test.find/index: all
	########################################
	# Test find vs index
	./myers-list test.find/index -length 0-1000 -skip 2 -impl most | grep -v ': true$$' | cat
	for i in $$(seq 2 10); do ./myers-list test.find/index -length 0-1000 -skip $$i -impl 1lgn; done | grep -v ': true$$' | cat

test.find-path/index-path: all
	########################################
	# Test find_path vs index_path
	./myers-list test.find-path/index-path -length 0-1000 -skip 2 -impl most | grep -v ': true$$' | cat
	for i in $$(seq 2 10); do ./myers-list test.find-path/index-path -length 0-1000 -skip $$i -impl 1lgn; done | grep -v ': true$$' | cat

test.find/find-path: all
	########################################
	# Test find vs find_path
	./myers-list test.find/find-path -length 0-1000 -skip 2 -impl most | grep -v ': true$$' | cat
	for i in $$(seq 2 10); do ./myers-list test.find/find-path -length 0-1000 -skip $$i -impl 1lgn; done | grep -v ': true$$' | cat

test.1lgn: test.1lgn.tree test.1lgn.path

test.1lgn.tree: all
	########################################
	# Test the 1lgn tree structure
	./myers-list dot-1lgn -height 10 -skip 3 -all >tree-10-3.new
	diff -u tree-10-3.txt tree-10-3.new

test.1lgn.path: all
	########################################
	# Test the 1lgn paths
	./myers-list path -src 0-500 -dst 0-500 -skip 3 -impl 1lgn -path >path-500-500-3.new
	diff -u path-500-500-3.txt path-500-500-3.new
