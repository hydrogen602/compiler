import pytest
import subprocess
import sys
import os

@pytest.fixture(scope="session", autouse=True)
def compile(request):
    result = subprocess.run('make')
    result.check_returncode()


thisPath = sys.path[0]
casesPath = os.path.join((thisPath), 'cases')

cases = set(t[:t.index('.')] for t in os.listdir(casesPath) if t.endswith('.idk'))


def case_helper(test):
    pathToCase = os.path.join(casesPath, test)

    compileResult = subprocess.run(['./Main', '-i', pathToCase + '.idk', '-o', f'tests/out/{test}.s'], capture_output=True)

    if test.startswith('fail'):
        assert compileResult.returncode != 0 # invalid code should fail
    else:
        #print(compileResult.stderr.decode())
        if compileResult != 0: print(test, cases)

        if compileResult.returncode != 0:
            print(compileResult.stderr)
        compileResult.check_returncode()

        result = subprocess.run(['spim', '-file', f'tests/out/{test}.s'], capture_output=True)
        result.check_returncode()

        output = result.stdout.decode()
        if 'Loaded: /usr/lib/spim/exceptions.s' in output:
            output = output.split('Loaded: /usr/lib/spim/exceptions.s\n', maxsplit=1)[1]
        if 'Loaded: /usr/local/Cellar/spim/9.1.22/share/exceptions.s' in output:
             output = output.split('Loaded: /usr/local/Cellar/spim/9.1.22/share/exceptions.s\n', maxsplit=1)[1]

        with open(pathToCase + '.result.txt') as f:
            solution = f.read()
        
        assert output == solution


# creates all the tests automatically
for test in cases:
    exec(f'def test_{test}():\n    case_helper("{test}")')
