import pytest
import subprocess
import sys
import os

@pytest.fixture(scope="session", autouse=True)
def compile(request):
    result = subprocess.run('make')
    result.check_returncode()

def test_all():
    thisPath = sys.path[0]
    casesPath = os.path.join((thisPath), 'cases')


    test_cases = set(t[:t.index('.')] for t in os.listdir(casesPath) if not t.startswith('.'))

    for test in test_cases:
        pathToCase = os.path.join(casesPath, test)

        compileResult = subprocess.run(['./Main', '-i', pathToCase + '.idk', '-o', 'tests/tmp.s'], capture_output=True)

        if test.startswith('fail'):
            assert compileResult.returncode != 0 # invalid code should fail
        else:
            #print(compileResult.stderr.decode())
            if compileResult != 0: print(test, test_cases)
            compileResult.check_returncode()

            result = subprocess.run(['spim', '-file', 'tests/tmp.s'], capture_output=True)
            result.check_returncode()

            output = result.stdout.decode().split('Loaded: /usr/lib/spim/exceptions.s\n', maxsplit=1)[1]

            with open(pathToCase + '.result.txt') as f:
                solution = f.read()
            
            assert output == solution