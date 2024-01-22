if [[ ( $@ == "--help") ||  $@ == "-h" ]]
then 
	echo "usage: $0 rts_path"
	exit 0
fi

if [  $# -l 1 ] 
then 
    echo "usage: $0 rts_path"
    exit 1
fi

mkdir -p output;
clang -emit-llvm $1 -c -o output/rts.bc

echo "Running correct programs..."
for sample in test/samples/test-*.mc;
    do  echo $sample
        (dune exec -- _build/default/bin/microcc.exe $sample -rts output/rts.bc -o output/exec.out) || true
        ./output/exec.out > output/output.txt
        diff -u "${sample%.*}".out output/output.txt
    done
