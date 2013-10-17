def readInt() {
    i = malloc(2)
    cur = read()
    while(cur != "\n") {
	i = 10*i
	i += ord(cur)-ord("0")
        cur = read()
    }
    i
}


def printInt(n) {
    ar = array(7)
    i = 0
    while(n) {
	d = malloc(3)
	while(n >= 10) {
    	    n -= 10
	    d++
        }
	// n is now mod(n, 10)
	ar[i] = 48+n
	n = d
	i++
    }
    while(i) { i-- print ar[i] }
}

def mod(a,b) {
    while(a>=b) {
    	a -= b
    }
    a
}

def even(a) {
    q = malloc(1)
    q = a
    !mod(q,2)
}

print "please input upper\n"
upper = readInt() // 4000000
a = malloc(3, 1)
b = malloc(3, 2)
s = malloc(3, 0)
while(a <= upper) {
    print "."
    if(even(a)) {
        s += a
    }
    c = a + b
    a = b
    b = c
}
print "\n"
printInt(s)
print "\n"
