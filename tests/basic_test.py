import pytest
import subprocess
import sys
import os


@pytest.fixture(scope="session", autouse=True)
def compile(request):
    result = subprocess.run(['make', 'build'])
    result.check_returncode()


thisPath = sys.path[0]
casesPath = os.path.join((thisPath), 'cases')

cases = set(t[:t.index('.')]
            for t in os.listdir(casesPath) if t.endswith('.idk'))


def case_helper(test):
    pathToCase = os.path.join(casesPath, test)

    args = ['cabal', 'run', 'compiler', '--', '-i',
            f'{pathToCase}.idk', '-o', f'tests/out/{test}']
    compileResult = subprocess.run(args, capture_output=True)

    if test.startswith('fail'):
        assert compileResult.returncode != 0  # invalid code should fail
    else:
        # print(compileResult.stderr.decode())
        if compileResult != 0:
            print(test, cases)

        if compileResult.returncode != 0:
            print(compileResult.stderr)
        compileResult.check_returncode()

        result = subprocess.run(
            [f'tests/out/{test}'], capture_output=True)
        result.check_returncode()

        output = result.stdout.decode()
        with open(pathToCase + '.result.txt') as f:
            solution = f.read()

        assert output == solution


# creates all the tests automatically
for test in cases:
    exec(f'def test_{test}():\n    case_helper("{test}")')
