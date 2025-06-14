## OCaml Implementation of Myers Lists

## Setup

Make sure you have `opam` installed.  If you are on Ubuntu, you can use
the PPA at <https://launchpad.net/~avsm/+archive/ubuntu/ppa> with:

    sudo add-apt-repository ppa:avsm/ppa
    sudo apt update
    sudo apt install m4 opam

Then run the following.  (Each of these commands can take a while.)

    opam init
    opam update
    opam switch create 4.07.1
    eval $(opam env)
    opam install core_bench

If everything is setup correctly then `which ocaml` should show
something like `/home/$USER/.opam/4.07.1/bin/ocaml`.

### How to compile

    make

### How to run

Get help:

    ./myers-list

### How to run benchmarks 

    make tests

### How to generate charts 
Ensure that you have Python `matplotlib` installed. If you are on
Ubuntu, you can run:

    sudo apt install python3-matplotlib

Ensure that you have also completed the benchmarks using `make tests` above. Then,
just run 

    make charts
