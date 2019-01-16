# Script to run the csmith random test generator multiple times

TEST_NAME=test
SIM=$1
RUN_AMT=-1
P_INST=1
SEED=-1

# Make output directory
OUTPUT_DIR=output
SRC_DIR=sources
mkdir -p output

usage(){
    echo "run-csmith.sh --sim SIM_BINARY [--run RUN_AMT] [--parallel PARALLEL_INSTANCES] [--seed SEED]"
    echo "    --sim -s SIM_BINARY                   is the simulator to test spike against"
    echo "    --run -r RUN_AMT                      is the amount of times to run the csmith tests"
    echo "                                          defaults to infinity"
    echo "    --parallel -p PARALLEL_INSTANCES      is the amount of instances to spawn in parallel"
    echo "                                          defaults to one instance"
    echo "    --seed -e SEED                        runs a single test with the seed specified"
    echo "                                          ignores all other parameters"
}

# Exit everything on one ctrl+c
trap kill_group SIGINT
kill_group(){
    echo ""
    echo "[ALL] Killing instances."
    kill 0
}

# Run the csmith test once
run_once () {
    echo "[$1] Running csmith test with seed=$2"
    csmith --seed $2 > $OUTPUT_DIR/$TEST_NAME-$1-$2.c

    # Build both a RISCV binary and normal binary

    # Test x86-64 first
    gcc -I$RISCV/include/csmith-2.4.0 -w $OUTPUT_DIR/$TEST_NAME-$1-$2.c -o $OUTPUT_DIR/$TEST_NAME-$1-$2.bin
    timeout 1s ./$OUTPUT_DIR/$TEST_NAME-$1-$2.bin | awk '{print tolower($0)}' > $OUTPUT_DIR/$TEST_NAME-$1-$2.host.out
    RV=$?
    if [ $RV -ne 0 ]; then
        echo "[$1] x86-64 binary timed out. Discard and start over."
        rm $OUTPUT_DIR/$TEST_NAME-$1-$2.bin $OUTPUT_DIR/$TEST_NAME-$1-$2.host.out $OUTPUT_DIR/$TEST_NAME-$1-$2.c
        return 0
    fi

    # Test RISCV spike version
    riscv64-unknown-elf-gcc -w -I./$SRC_DIR -DPREALLOCATE=1 -mcmodel=medany -static -std=gnu99 -O2 -ffast-math -fno-common -o $OUTPUT_DIR/$TEST_NAME-$1-$2.riscv $OUTPUT_DIR/$TEST_NAME-$1-$2.c $SRC_DIR/syscalls.c $SRC_DIR/crt.S -static -nostdlib -nostartfiles -lm -lgcc -T $SRC_DIR/link.ld -I$RISCV/include/csmith-2.4.0
    timeout --foreground 10s spike $OUTPUT_DIR/$TEST_NAME-$1-$2.riscv 1> $OUTPUT_DIR/$TEST_NAME-$1-$2.spike.out 2> $OUTPUT_DIR/$TEST_NAME-$1-$2.spike.log
    RV=$?
    if [ $RV -ne 0 ]; then
        echo "[$1] Spike timed out. Discard and start over."
        rm $OUTPUT_DIR/$TEST_NAME-$1-$2.bin $OUTPUT_DIR/$TEST_NAME-$1-$2.host.out $OUTPUT_DIR/$TEST_NAME-$1-$2.c $OUTPUT_DIR/$TEST_NAME-$1-$2.riscv $OUTPUT_DIR/$TEST_NAME-$1-$2.spike.out $OUTPUT_DIR/$TEST_NAME-$1-$2.spike.log
        return 0
    fi

    # Compare x86-64 and Spike
    cmp -s $OUTPUT_DIR/$TEST_NAME-$1-$2.spike.out $OUTPUT_DIR/$TEST_NAME-$1-$2.host.out
    RV=$?
    if [ $RV -ne 0 ]; then
        echo "[$1] Spike produces wrong result compared to x86-64 binary. Discard and start over."
        rm $OUTPUT_DIR/$TEST_NAME-$1-$2.bin $OUTPUT_DIR/$TEST_NAME-$1-$2.host.out $OUTPUT_DIR/$TEST_NAME-$1-$2.c $OUTPUT_DIR/$TEST_NAME-$1-$2.riscv $OUTPUT_DIR/$TEST_NAME-$1-$2.spike.out $OUTPUT_DIR/$TEST_NAME-$1-$2.spike.log
        return 0
    fi

    # Compare simulator output versus spike
    $SIM $OUTPUT_DIR/$TEST_NAME-$1-$2.riscv 1> $OUTPUT_DIR/$TEST_NAME-$1-$2.sim.out
    cmp -s $OUTPUT_DIR/$TEST_NAME-$1-$2.sim.out $OUTPUT_DIR/$TEST_NAME-$1-$2.spike.out
    RV=$?
    if [ $RV -ne 0 ]; then
        echo "[$1] Simulator produced wrong result."
        $SIM $OUTPUT_DIR/$TEST_NAME-$1-$2.riscv +verbose +vcdplusfile=$OUTPUT_DIR/$TEST_NAME-$1-$2.vpd 1> $OUTPUT_DIR/$TEST_NAME-$1-$2.sim.out 2> $OUTPUT_DIR/$TEST_NAME-$1-$2.sim.log
        echo "[$1]  Vpd of error file:     $OUTPUT_DIR/$TEST_NAME-$1-$2.vpd"
        echo "[$1]  Simulator output file: $OUTPUT_DIR/$TEST_NAME-$1-$2.sim.out"
        echo "[$1]  Simulator log file:    $OUTPUT_DIR/$TEST_NAME-$1-$2.sim.log"
        kill_group
    else
        echo "[$1] Simulator and spike agree."
        rm $OUTPUT_DIR/$TEST_NAME-$1-$2.bin $OUTPUT_DIR/$TEST_NAME-$1-$2.host.out $OUTPUT_DIR/$TEST_NAME-$1-$2.c $OUTPUT_DIR/$TEST_NAME-$1-$2.riscv $OUTPUT_DIR/$TEST_NAME-$1-$2.spike.out $OUTPUT_DIR/$TEST_NAME-$1-$2.spike.log $OUTPUT_DIR/$TEST_NAME-$1-$2.sim.out
        return 0
    fi
}

run() {
    if [ $SEED == -1 ]; then
        if [ $RUN_AMT == -1 ]; then
            while true;
            do
                SEED=$(od -N 4 -t uL -An /dev/urandom | tr -d " ")
                run_once $1 $SEED
            done
        else
            for j in `seq 1 $RUN_AMT`;
            do
                SEED=$(od -N 4 -t uL -An /dev/urandom | tr -d " ")
                run_once $1 $SEED
            done
        fi
    else
        run_once $1 $SEED
    fi
}

while [ "$1" != "" ];
do
    case $1 in
        -s | --sim )    shift
                        SIM=$1
                        ;;
        -r | --run )    shift
                        RUN_AMT=$1
                        ;;
        -p | --parallel )   shift
                            P_INST=$1
                            ;;
        -e | --seed )   shift
                        SEED=$1
                        ;;
        -h | --help )   usage
                        return 0
    esac
    shift
done

# Start of script
if [ -z "$SIM" ]; then
    echo "Forgot simulator binary."
    usage
    exit 1
fi

if [ $SEED == -1 ]; then
    if [ $RUN_AMT == -1 ]; then
        echo "Spawning $P_INST instance(s), running csmith infinite times"
    else
        echo "Spawning $P_INST instance(s), running csmith $RUN_AMT times"
    fi

    for i in `seq 1 $P_INST`;
    do
        run $i &
    done
    wait
else
    RUN_AMT=1
    run 1
    wait
fi
