if [[ ( $@ == "--help") ||  $@ == "-h" ]]
then 
	echo "usage: $0 rts_path"
	exit 0
fi

if [  $# -lt 1 ] 
then 
    echo "usage: $0 rts_path"
    exit 1
fi

mkdir -p output;
clang -c $1 -o output/rts.o

echo "Running correct programs..."
for sample in test/samples/test-*.mc;
    do  echo $sample
        dune exec -- _build/default/bin/microcc.exe $sample -O -rts output/rts.o -o output/exec.out
        ./output/exec.out > output/output.txt
        diff -u "${sample%.*}".out output/output.txt
    done
