import argparse
import random

def chinese_restaurant_process(num_customers, alpha):
    if not num_customers: return []

    assignments = [1]
    next_table  =  2

    for i in xrange(1, num_customers):
        r = random.random()
        new_table_prob = float(alpha) / (alpha + i)
        if r < new_table_prob:
            assignments.append(next_table)
            next_table += 1
        else:
            assignments.append( random.choice(assignments) )

    return assignments


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-n', help='number of customers')
    parser.add_argument('-a', help='alpha parameter')

    args = parser.parse_args()
    assignments = chinese_restaurant_process(int(args.n), float(args.a))

    print assignments


if (__name__ == '__main__'):
    main()
