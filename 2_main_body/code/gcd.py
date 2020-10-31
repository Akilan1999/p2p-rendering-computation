import library

# GCD of a and b
def my_gcd(a, b):
    t = b
    while not b == 0:
        t = b
        b = a % b
        a = t
    return a