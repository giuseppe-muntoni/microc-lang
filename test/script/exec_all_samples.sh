echo "CORRECT PROGRAMS";
for sample in test/samples/test-*.mc;
    do  echo $sample
        (dune exec -- _build/default/bin/microcc.exe $sample -rts executables/rt-support.bc -o executables/exec.out) || true
        ./executables/exec.out
    done

echo "FAILING PROGRAMS";
for sample in test/samples/fail-*.mc;
    do  echo $sample
        dune exec -- _build/default/bin/microcc.exe $sample || true
    done
