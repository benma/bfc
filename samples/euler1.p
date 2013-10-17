
def mod(a,b) {
    while(a>=b) {
    	a -= b
    }
    a
}

def readInt() {
    i = malloc(2)
    cur = read()
    while(cur != "\n") {
	i = 10 * i
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

print "please input upper\n"
upper = readInt()-1

print "hang on\n"
result = malloc(3)
while(upper) {
    print "."
    if(!mod(upper, 3) || !mod(upper, 5)) {
        result += upper
    }
    upper--
}
print "\n"
printInt(result)
print "\n"
