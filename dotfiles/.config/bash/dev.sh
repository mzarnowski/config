test_pants() {
    export TEST_SET=$1 
    export JAVA_HOME=/usr/lib/jvm/java-11-openjdk 
    
    if [ -n "$2" ]; then
        run="./scripts/run-tests-ci.sh --test-junit-test=$2"
    else
        run="./scripts/run-tests-ci.sh"
    fi

    if [ -n "$DEBUG" ];then
        run="$run --jvm-test-debug"
    fi

    echo ">> $run"

    ./scripts/prepare-ci-environment.sh \
    && ./scripts/setup-ci-environment.sh \
    && $run
}
