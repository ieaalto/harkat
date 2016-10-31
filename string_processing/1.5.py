import time


def print_runtime(f, n, msg):
    t = time.time()
    res = f(n)
    print(msg + " time: " + str(time.time() - t) + " result: " + str(res))


def brute_fibonacci(n):
    if n == 0:
        return 0
    if n == 1:
        return 1
    else:
        return brute_fibonacci(n-1) + brute_fibonacci(n-2)


def dynamic_fibonacci(n):

    def fibonacci_aux(m, memo):
        if m == 0:
            return 0
        if m == 1:
            return 1
        if m not in memo:
            memo[m] = fibonacci_aux(m - 2, memo) + fibonacci_aux(m - 1, memo)
        return memo[m]

    return fibonacci_aux(n, {})


# Takes approx. 3.7 seconds, >6 when n=35
print_runtime(brute_fibonacci, 34, "brute force: ")

# Takes approx. 0.0015 second when n=1000.
print_runtime(dynamic_fibonacci, 1000, "dynamic programming: ")