#!/usr/bin/env python3

import pathlib

casesDir = pathlib.Path(__file__).parent.absolute() / 'cases'
files = list(s for s in casesDir.iterdir() if s.suffix == '.idk')

choice = input('success or fail test? [s|f]: ')
while choice not in ['s','f']:
    print(f'Invalid input: "{choice}", expected s or f')
    choice = input('success or fail test? [s|f]: ')

if choice == 's':
    print('Enter valid code here (two empty lines to stop):')
    lines = []
    while lines[-2:] != ['', '']:
        lines.append(input().rstrip())
    
    i = 1
    while (casesDir / f'c{i}.idk').exists():
        i += 1

    file = (casesDir / f'c{i}.idk') 
    fileOut = (casesDir / f'c{i}.result.txt')

    print('Enter desired result:')
    linesOut = []
    while linesOut[-2:] != ['', '']:
        linesOut.append(input())
    
    nl = input('keep last new line in result? [y|n]: ')
    while nl not in ['n','y']:
        print(f'Invalid input: "{nl}", expected y or n')
        nl = input('keep last new line in result? [y|n]: ')

    with file.open('w') as f:
        f.write('\n'.join(lines[:-1]))

    x = -1 if nl == 'y' else -2  
    with fileOut.open('w') as f:
        f.write('\n'.join(linesOut[:x]))

    print('Created', file)



if choice == 'f':
    print('Enter invalid code here (two empty lines to stop):')
    lines = []
    while lines[-2:] != ['', '']:
        lines.append(input().rstrip())
    
    i = 1
    while (casesDir / f'fail{i}.idk').exists():
        i += 1

    file = (casesDir / f'fail{i}.idk') 

    with file.open('w') as f:
        f.write('\n'.join(lines[:-1]))
    
    print('Created', file)