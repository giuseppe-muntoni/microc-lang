if [[ ( $@ == "--help") ||  $@ == "-h" ]]
then 
	echo "usage: $0 file_messages"
	exit 0
fi

if [  $# -l 1 ] 
then 
    echo "usage: $0 file_messages"
    exit 1
fi

echo "Running failing programs..."
for sample in test/samples/fail-*.mc;
    do  echo $sample
        (dune exec -- _build/default/bin/microcc.exe $sample || true) 1>> $1 2>&1
    done