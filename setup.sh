#version: 2.16

#sh install.sh #<(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)

#apt-get -y install opam

printf "add-apt-repository ppa:avsm/ppa\n"
add-apt-repository -y ppa:avsm/ppa

printf "apt-get update\n"
apt-get update

printf "apt-get -y install opam\n"
apt-get -y install opam
apt-get -y install gcc 
apt-get install -y ocaml-native-compilers camlp4 make m4



#apt-get install -y ocaml ocaml-native-compilers camlp4 make m4 curl
printf "opam init --disable-sandboxing\n"
opam init --disable-sandboxing
#printf "eval `opam config env`\n"
#eval `opam config env`

printf "opam switch create 4.07.1\n"
opam switch create 4.07.1

eval $(opam env)

opam install -y ocamlfind ounit 
opam install -y dune
opam install -y markup
opam install -y yojson

eval $(opam env) 
