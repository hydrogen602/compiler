
```
while expr {
    stuff
}
```


```
loop: 
    eval expr
    jump to while_end if expr is false -> beq expr $0

    stuff
    j loop
while_end:
```