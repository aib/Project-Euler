import functools
import math
import time

import progress

p = progress.ProgressBar(73682)

# 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p)
COINS = [1, 2, 5, 10, 20, 50, 100, 200]

def fixed_point(f, x):
	(x, x1) = (None, x)
	while x != x1:
		(x, x1) = (x1, f(x1))
		p._progress = len(list(filter(lambda cs: sum(cs) == 200, x)))
		print(p)
		print(time.monotonic() + p.get_eta())
	return x

def add_coins(new_coins, base):
	bases = [base]
	for nc in new_coins:
		b = tuple(sorted(base + (nc,)))
		bases.append(b)
	return set(bases)

def add_coins_and_check(coin_sum, new_coins, bases):
	new_bases = []
	for base in bases:
		for new_base in add_coins(new_coins, base):
			if sum(new_base) <= coin_sum:
				new_bases.append(new_base)
	return set(new_bases)

def find_combos(amount):
	st = time.monotonic()
	f = functools.partial(add_coins_and_check, amount, COINS)
	all_sums = fixed_point(functools.partial(add_coins_and_check, amount, COINS), [()])
	exact_sums = list(filter(lambda cs: sum(cs) == amount, all_sums))
	et = time.monotonic()
	tt = et - st
	print(tt, tt / len(exact_sums))
	return len(exact_sums)

for i in [200]:#range(100):
	print(i, find_combos(i))
