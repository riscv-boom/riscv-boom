git clone https://github.com/csmith-project/csmith.git
cd csmith
cmake -DCMAKE_INSTALL_PREFIX=$RISCV .
make && make install
